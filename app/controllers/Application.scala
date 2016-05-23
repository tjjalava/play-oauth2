package controllers

import java.util.UUID
import javax.inject.Named

import com.google.inject.Inject
import com.greitco.play.oauth2.{OAuth2, OAuth2Controller}
import play.api._
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc._
import util._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future

class Application @Inject() (oauth2: GithubUtil, fbauth: FacebookUtil,
                             @Named("Google") googleAuth: OAuth2,
                             wsClient:WSClient)
  extends OAuth2Controller {

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def github = Action { implicit request =>
    val callbackUrl = util.routes.Github.callback().absoluteURL()
    val scope = "user:email"   // github scope - request repo access
    val state = UUID.randomUUID().toString  // random confirmation string
    val redirectUrl = oauth2.getAuthorizationUrl(callbackUrl, scope, state)

    Redirect(redirectUrl).withSession("oauth-state" -> state)
  }

  def facebook = Action { implicit request =>
    val callbackUrl = util.routes.Facebook.callback().absoluteURL()
    val scope = "email"
    val state = UUID.randomUUID().toString  // random confirmation string
    val redirectUrl = fbauth.getAuthorizationUrl(callbackUrl, scope, state)

    Redirect(redirectUrl).withSession("fb-state" -> state)
  }

  def google = Action { implicit request =>
    val callbackUrl = controllers.routes.Application.oauth2Callback().absoluteURL()
    redirectToProvider(callbackUrl, googleAuth)
  }

  def googleSuccess = Action.async { request =>
    request.session.get("access-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      for {
        user <- wsClient.url("https://www.googleapis.com/oauth2/v3/userinfo")
          .withHeaders("Authorization" -> s"Bearer $authToken")
          .get().map { response =>
          response.json
        }
      } yield Ok(Json.prettyPrint(user))
    }
  }

  def oauth2Callback(code: Option[String] = None, state: Option[String] = None) = Action.async { implicit request =>
    googleAuth.handleCallback(code, state, controllers.routes.Application.googleSuccess())
  }

}