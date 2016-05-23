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

  def redirectToProvider(callbackUrl:String, provider:OAuth2):Result = {
    val state = getState
    Redirect(provider.getAuthorizationUrl(callbackUrl, state)).withSession(OAuth2.stateKey -> state)
  }
}

object OAuth2 {
  val stateKey:String = "state"
}

trait OAuth2 {

  protected val wsClient:WSClient

  protected def getConf(path:String, configuration: Configuration) =
    configuration.getString(s"play.oauth2.$path").getOrElse(throw configuration.globalError(s"Path $path not found in configuration"))

  protected def authId:String
  protected def authSecret:String
  protected def authUri:String
  protected def authScope:String
  protected def tokenUri:String

  def getAuthorizationUrl(redirectUri: String, state: String): String =
    authUri & ("client_id" -> authId) & ("redirect_uri" -> redirectUri) & ("scope" -> authScope) & ("state" -> state)

  def handleCallback(code: Option[String], state: Option[String], successRoute:Call)
                    (implicit request:RequestHeader, ec:ExecutionContext):Future[Result]

  protected def getToken(code:String, params:Map[String, String] = Map.empty)(implicit ec:ExecutionContext):Future[String] =
    wsClient.url(
      (tokenUri ? ("client_id" -> authId) &
        ("client_secret" -> authSecret) &
        ("code" -> code)
      ).addParams(params.toSeq)
    ).withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).post(Results.EmptyContent).map { response =>
      (response.json \ "access_token").asOpt[String].getOrElse(throw new IllegalStateException("Sod off!"))
    }
}

class GoogleAuth @Inject() (configuration: Configuration, override protected val wsClient: WSClient) extends OAuth2 {
  import Results._

  override protected val authId: String = getConf("google.client-id", configuration)
  override protected val authSecret: String = getConf("google.client-secret", configuration)
  override protected val authUri: String = getConf("google.auth-uri", configuration) ? ("response_type" -> "code")
  override protected val authScope: String = getConf("google.auth-scope", configuration)
  override protected val tokenUri = getConf("google.token-uri", configuration)

  override def handleCallback(code: Option[String], state: Option[String], successRoute: Call)
                             (implicit request: RequestHeader, ec:ExecutionContext): Future[Result] =
    (for {
      code <- code
      state <- state
      oauthState <- request.session.get(OAuth2.stateKey)
    } yield {
      if (state == oauthState) {
        val params = Map(
          "redirect_uri" -> "http://localhost:3000/oauth2callback",
          "grant_type" -> "authorization_code"
        )
        getToken(code, params).map { accessToken =>
          Redirect(successRoute).withSession("access-token" -> accessToken)
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      }
      else {
        Future.successful(BadRequest("Invalid Google login"))
      }
    }).getOrElse(Future.successful(BadRequest("No parameters supplied")))

}
