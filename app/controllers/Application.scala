package controllers

import java.util.UUID

import com.google.inject.Inject
import play.api._
import play.api.mvc._
import util._

class Application @Inject() (oauth2: GithubUtil, fbauth: FacebookUtil, googleAuth: GoogleUtil) extends Controller {

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
    val callbackUrl = util.routes.Google.callback().absoluteURL()
    val scope = "https://www.googleapis.com/auth/userinfo.email"
    val state = UUID.randomUUID().toString  // random confirmation string
    val redirectUrl = googleAuth.getAuthorizationUrl(callbackUrl, scope, state)

    Redirect(redirectUrl).withSession("google-state" -> state)
  }

}