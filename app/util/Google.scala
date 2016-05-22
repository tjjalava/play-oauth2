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
 * @since 16.8.15
 */
@Singleton
class GoogleUtil @Inject() (configuration: Configuration, wsClient: WSClient) {
  lazy val googleAuthId = configuration.getString("google.client.id").get
  lazy val googleAuthSecret = configuration.getString("google.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = configuration.getString("google.redirect.url").get
    baseUrl.format(googleAuthId, redirectUri, scope, state)
  }

  def getToken(code: String, redirectUri:String): Future[String] = {
    val tokenResponse = wsClient.url("https://www.googleapis.com/oauth2/v3/token")
      .withQueryString("client_id" -> googleAuthId,
        "client_secret" -> googleAuthSecret,
        "code" -> code,
        "redirect_uri" -> redirectUri,
        "grant_type" -> "authorization_code"
      ).withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      println(response.body)
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("Sod off!"))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

class Google @Inject() (googleauth: GoogleUtil, wsClient: WSClient) extends Controller {

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("google-state")
    } yield {
        if (state == oauthState) {
          googleauth.getToken(code, util.routes.Google.callback().absoluteURL()).map { accessToken =>
            Redirect(util.routes.Google.success()).withSession("google-token" -> accessToken)
          }.recover {
            case ex: IllegalStateException => Unauthorized(ex.getMessage)
          }
        }
        else {
          Future.successful(BadRequest("Invalid Google login"))
        }
      }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    request.session.get("google-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      for {
        user <- wsClient.url("https://www.googleapis.com/oauth2/v3/userinfo")
          .withHeaders("Authorization" -> s"Bearer $authToken")
          .get().map { response =>
          response.json
        }
      } yield Ok(Json.prettyPrint(user))
    }
  }
}
