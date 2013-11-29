package controllers.core

import models.sql.OpenIDAssociation
import models.AccountDAO
import controllers.base.LoginHandler
import forms.OpenIDForm
import play.api.libs.openid._
import play.api.libs.concurrent.Execution.Implicits._
import play.api._
import play.api.mvc._
import concurrent.Future
import play.api.i18n.Messages
import com.google.inject._
import scala.concurrent.Future.{successful => immediate}
import backend.Backend

/**
 * OpenID login handler implementation.
 */
@Singleton
case class OpenIDLoginHandler @Inject()(implicit globalConfig: global.GlobalConfig, backend: Backend) extends LoginHandler {

  private lazy val userDAO: AccountDAO = play.api.Play.current.plugin(classOf[AccountDAO]).get

  def openIDLogin = optionalUserAction { implicit maybeUser => implicit request =>
    Ok(views.html.openIDLogin(OpenIDForm.openid, action = routes.OpenIDLoginHandler.openIDLoginPost))
  }

  def openIDLoginPost = optionalUserAction.async { implicit maybeUser => implicit request =>
    OpenIDForm.openid.bindFromRequest.fold(
      error => {
        Logger.info("bad request " + error.toString)
        immediate(BadRequest(views.html.openIDLogin(error, action = routes.OpenIDLoginHandler.openIDLoginPost)))
      },
      {
        case (openid) => OpenID.redirectURL(
            openid,
            routes.OpenIDLoginHandler.openIDCallback.absoluteURL(),
            Seq("email" -> "http://schema.openid.net/contact/email",
              "axemail" -> "http://axschema.org/contact/email"))
            .map(url => Redirect(url))
      }
    )
  }

  def openIDCallback = Action.async { implicit request =>
    OpenID.verifiedId.flatMap { info =>
      // check if there's a user with the right id
      OpenIDAssociation.findByUrl(info.id) match {
        // NOTE: If this user exists in the auth DB but not on the REST
        // server we have a bit of a problem at present...
        case Some(assoc) => gotoLoginSucceeded(assoc.user.get.id)
        case None => {
          val email = extractEmail(info.attributes).getOrElse(sys.error("No openid email"))
          userDAO.findByEmail(email).map { acc =>
            OpenIDAssociation.addAssociation(acc, info.id)
            gotoLoginSucceeded(acc.id)
              .map(_.withSession("access_uri" -> globalConfig.routeRegistry.default.url))
          } getOrElse {
            backend.createNewUserProfile.flatMap { up =>
              userDAO.create(up.id, email.toLowerCase).map { account =>
                OpenIDAssociation.addAssociation(account, info.id)
                gotoLoginSucceeded(account.id).map { r =>
                  r.withSession("access_uri" -> globalConfig.routeRegistry.default.url)
                }
              } getOrElse {
                immediate(BadRequest("Creation of user db failed!"))
              }
            }
          }
        }
      }
    } recoverWith {
      case e: Throwable => Future.successful {
        Redirect(controllers.core.routes.OpenIDLoginHandler.openIDLogin)
          .flashing("error" -> Messages("openid.openIdError", e.getMessage))
      }
    }
  }

  /**
   * Pick up the email from OpenID info. This may be stored in different
   * attributes depending on the provider.
   */
  private def extractEmail(attrs: Map[String, String]): Option[String]
      = attrs.get("email").orElse(attrs.get("axemail"))

}