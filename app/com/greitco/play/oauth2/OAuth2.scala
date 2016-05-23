package com.greitco.play.oauth2

import java.util.UUID

import com.google.inject.Inject
import play.api.Configuration
import play.api.libs.ws.WSClient
import play.api.mvc._
import com.netaporter.uri.dsl._
import play.api.http.{HeaderNames, MimeTypes}

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author tjjalava
  * @since 23.5.2016 
  */
trait OAuth2Controller extends Controller {

  protected def getState:String = UUID.randomUUID().toString

  def redirectToProvider(callbackUrl:String, provider:OAuth2, scope:Option[String] = None):Result = {
    val state = getState
    Redirect(provider.getAuthorizationUrl(callbackUrl, state, scope.getOrElse(provider.authScope))).withSession(OAuth2.stateKey -> state)
  }
}

object OAuth2 {
  val stateKey:String = "state"
}

trait OAuth2 {

  protected def getConf(path:String, configuration: Configuration) =
    configuration.getString(s"play.oauth2.$path").getOrElse(throw configuration.globalError(s"Path $path not found in configuration"))

  protected def authId:String
  protected def authSecret:String
  protected def providerURL:String

  def authScope:String
  def getAuthorizationUrl(redirectUri: String, state: String, scope:String = authScope): String =
    providerURL & ("client_id" -> authId) & ("redirect_uri" -> redirectUri) & ("scope" -> scope) & ("state" -> state)

  def handleCallback(code: Option[String], state: Option[String], successRoute:Call)
                    (implicit request:RequestHeader, ec:ExecutionContext):Future[Result]
}

class GoogleAuth @Inject() (configuration: Configuration, wsClient: WSClient) extends OAuth2 {
  import Results._

  override protected def authId: String = getConf("google.client-id", configuration)
  override protected def authSecret: String = getConf("google.client-secret", configuration)
  override protected def providerURL: String = getConf("google.redirect-url", configuration)
  override def authScope: String = "https://www.googleapis.com/auth/userinfo.email"

  private def getToken(code:String, redirect:String)(implicit ec:ExecutionContext):Future[String] = {
    val tokenResponse = wsClient.url("https://www.googleapis.com/oauth2/v3/token")
      .withQueryString("client_id" -> authId,
        "client_secret" -> authSecret,
        "code" -> code,
        "redirect_uri" -> redirect,
        "grant_type" -> "authorization_code"
      ).withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).post(Results.EmptyContent())

    tokenResponse.map { response =>
      (response.json \ "access_token").asOpt[String].getOrElse(throw new IllegalStateException("Sod off!"))
    }
  }

  override def handleCallback(code: Option[String], state: Option[String], successRoute: Call)
                             (implicit request: RequestHeader, ec:ExecutionContext): Future[Result] =
    (for {
      code <- code
      state <- state
      oauthState <- request.session.get(OAuth2.stateKey)
    } yield {
      if (state == oauthState) {
        getToken(code, util.routes.Google.callback().absoluteURL()).map { accessToken =>
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
