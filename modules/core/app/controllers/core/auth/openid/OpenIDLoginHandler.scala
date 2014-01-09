package controllers.core.auth.openid

import models.sql.OpenIDAssociation
import models.{Account, AccountDAO}
import play.api.libs.openid._
import play.api.libs.concurrent.Execution.Implicits._
import play.api._
import play.api.mvc._
import concurrent.Future
import scala.concurrent.Future.{successful => immediate}
import backend.Backend
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.SimpleResult
import play.api.i18n.Messages
import utils.forms.isValidOpenIDUrl
import java.net.ConnectException

/**
 * OpenID login handler implementation.
 */
trait OpenIDLoginHandler {

  self: Controller =>

  val backend: Backend
  val globalConfig: global.GlobalConfig

  val userDAO: AccountDAO

  val openidForm = Form(single(
    "openid_identifier" -> nonEmptyText
  ) verifying("OpenID URL is invalid", f => f match  {
    case s => isValidOpenIDUrl(s)
  }))

  object openIDLoginPostAction {
    def async(handler: Call)(f: Form[String] => Request[AnyContent] => Future[SimpleResult]): Action[AnyContent] = {
      Action.async { implicit request =>
        try {
          openidForm.bindFromRequest.fold(
            error => {
              Logger.info("bad request " + error.toString)
              f(error)(request)
            }, openidUrl => OpenID.redirectURL(
              openidUrl,
              handler.absoluteURL(),
              Seq("email" -> "http://schema.openid.net/contact/email",
                "axemail" -> "http://axschema.org/contact/email")).map(url => Redirect(url))
            .recoverWith {
              case t: ConnectException => {
                Logger.warn("OpenID Login connect exception: {}", t)
                f(openidForm.fill(openidUrl)
                  .withGlobalError(Messages("openid.openIdUrlError", openidUrl)))(request)
              }
              case t => {
                Logger.warn("OpenID Login argument exception: {}", t)
                f(openidForm.fill(openidUrl)
                  .withGlobalError(Messages("openid.openIdUrlError", openidUrl)))(request)
              }
            }
          )
        } catch {
          case _: Throwable => f(openidForm
            .withGlobalError(Messages("openid.openIdUrlError")))(request)
        }
      }
    }

    def apply(handler: Call)(f: Form[String] => Request[AnyContent] => SimpleResult): Action[AnyContent] = {
      async(handler)(f.andThen(_.andThen(t => immediate(t))))
    }
  }

  object openIDCallbackAction {
    def async(f: Either[Form[String],Account] => Request[AnyContent] => Future[SimpleResult]): Action[AnyContent] = {
      Action.async { implicit request =>
        OpenID.verifiedId.flatMap { info =>
        // check if there's a user with the right id
          OpenIDAssociation.findByUrl(info.id).map { assoc =>
            // NOTE: If this user exists in the auth DB but not on the REST
            // server we have a bit of a problem at present...
            Logger.logger.info("User '{}' logged in via OpenId", assoc.user.get.id)
            f(Right(assoc.user.get))(request)
          } getOrElse {
            val email = extractEmail(info.attributes).getOrElse(sys.error("No openid email"))
            userDAO.findByEmail(email).map { acc =>
              OpenIDAssociation.addAssociation(acc, info.id)
              Logger.logger.info("User '{}' created OpenID association", acc.id)
              f(Right(acc))(request)
            } getOrElse {
              backend.createNewUserProfile().flatMap { up =>
                userDAO.create(up.id, email.toLowerCase).map { account =>
                  OpenIDAssociation.addAssociation(account, info.id)
                  Logger.logger.info("User '{}' created OpenID account", account.id)
                  f(Right(account))(request)
                } getOrElse {
                  sys.error("Creation of user db failed!")
                }
              }
            }
          }
        } recoverWith {
          case t => f(Left(openidForm.withGlobalError("openid.openIdError", t.getMessage)))(request)
            .map(_.flashing("error" -> Messages("openid.openIdError", t.getMessage)))
        }
      }
    }

    def apply(f: Either[Form[String],Account] => Request[AnyContent] => SimpleResult): Action[AnyContent] = {
      async(f.andThen(_.andThen(t => immediate(t))))
    }
  }

  /**
   * Pick up the email from OpenID info. This may be stored in different
   * attributes depending on the provider.
   */
  private def extractEmail(attrs: Map[String, String]): Option[String]
      = attrs.get("email").orElse(attrs.get("axemail"))

}