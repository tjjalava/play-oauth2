package util

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, Controller, Results}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author jalavat
 * @since 10.8.15
 */
@Singleton
class FacebookUtil @Inject() (configuration: Configuration, wSClient: WSClient) {
  lazy val fbAuthId = configuration.getString("facebook.client.id").get
  lazy val fbAuthSecret = configuration.getString("facebook.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = configuration.getString("facebook.redirect.url").get
    baseUrl.format(fbAuthId, redirectUri, scope, state)
  }

  def getToken(code: String, redirectUri:String): Future[String] = {
    val tokenResponse = wSClient.url("https://graph.facebook.com/v2.4/oauth/access_token")
      .withQueryString("client_id" -> fbAuthId,
        "client_secret" -> fbAuthSecret,
        "code" -> code,
        "redirect_uri" -> redirectUri
      ).withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      println(response.body)
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("Sod off!"))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

class Facebook @Inject() (fbauth: FacebookUtil, wsClient: WSClient) extends Controller {

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("fb-state")
    } yield {
        if (state == oauthState) {
          fbauth.getToken(code, util.routes.Facebook.callback().absoluteURL()).map { accessToken =>
            Redirect(util.routes.Facebook.success()).withSession("fb-token" -> accessToken)
          }.recover {
            case ex: IllegalStateException => Unauthorized(ex.getMessage)
          }
        }
        else {
          Future.successful(BadRequest("Invalid facebook login"))
        }
      }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    request.session.get("fb-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      for {
        user <- wsClient.url("https://graph.facebook.com/me")
          .withQueryString(
            "fields" -> "name,email",
            "format" -> "json",
            "access_token" -> authToken
          ).get().map(_.json)
      } yield Ok(Json.prettyPrint(user))
    }
  }
}
