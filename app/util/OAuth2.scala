package util

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, Controller, Results}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class OAuth2Util @Inject() (configuration: Configuration, wsClient: WSClient) {
  lazy val githubAuthId = configuration.getString("github.client.id").get
  lazy val githubAuthSecret = configuration.getString("github.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
  val baseUrl = configuration.getString("github.redirect.url").get
    baseUrl.format(githubAuthId, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val tokenResponse = wsClient.url("https://github.com/login/oauth/access_token")
      .withQueryString("client_id" -> githubAuthId,
        "client_secret" -> githubAuthSecret,
        "code" -> code).
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("Sod off!"))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

class OAuth2 @Inject() (oauth2: OAuth2Util, wsClient: WSClient) extends Controller {

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("oauth-state")
    } yield {
      if (state == oauthState) {
        oauth2.getToken(code).map { accessToken =>
          Redirect(util.routes.OAuth2.success()).withSession("oauth-token" -> accessToken)
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      }
      else {
        Future.successful(BadRequest("Invalid github login"))
      }
    }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
        for {
            user <- wsClient.url("https://api.github.com/user")
                .withHeaders(HeaderNames.AUTHORIZATION -> s"token $authToken")
                .get().map { response =>
                    response.json
                }
            email <- wsClient.url("https://api.github.com/user/emails")
                .withHeaders(HeaderNames.AUTHORIZATION -> s"token $authToken")
                .get().map { response =>
                    response.json
                }
        } yield Ok(Json.prettyPrint(user) + Json.prettyPrint(email))
    }
  }
}