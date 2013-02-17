package controllers

import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._

import base.{ControllerHelpers, AuthController}
import play.api.data.{FormError, Form}
import play.api.data.Forms._
import defines.{EntityType, PermissionType, ContentType}
import play.api.i18n.Messages
import org.mindrot.jbcrypt.BCrypt
import models.UserProfileF
import models.sql.OpenIDUser


object Admin extends Controller with AuthController with ControllerHelpers {

  val userPasswordForm = Form(
    tuple(
      "email" -> email,
      "username" -> nonEmptyText,
      "name" -> nonEmptyText,
      "password" -> nonEmptyText,
      "confirm" -> nonEmptyText,
      "groups" -> optional(list(nonEmptyText))

    ) verifying(Messages("admin.passwordsDoNotMatch"), f => f match {
      case (_, _, _, pw, pwc, _) => pw == pwc
    })
  )

  def adminActions = adminAction { implicit userOpt => implicit request =>
    Ok(views.html.admin.actions())
  }

  def createUser = withContentPermission(PermissionType.Create, ContentType.UserProfile) { implicit userOpt => implicit request =>
    Ok(views.html.admin.createUser(userPasswordForm, routes.Admin.createUserPost))
  }

  def createUserPost = withContentPermission(PermissionType.Create, ContentType.UserProfile) { implicit userOpt => implicit request =>
    // TODO: Refactor to make this logic clearer...
    userPasswordForm.bindFromRequest.fold(
      errorForm => {
        Ok(views.html.admin.createUser(errorForm, routes.Admin.createUserPost))
      },
      values => {
        // Groups is currently unused, but will be when support is added to
        // the REST API to add the user to them at creation time.
        val (email, username, name, pw, _, groups) = values
        // check if the email is already registered...
        models.sql.OpenIDUser.findByEmail(email).map { account =>
          val errForm = userPasswordForm.bindFromRequest
            .withError(FormError("email", Messages("admin.userEmailAlreadyRegistered", account.profile_id)))
          BadRequest(views.html.admin.createUser(errForm, routes.Admin.createUserPost))
        } getOrElse {
          // It's not registered, so create the account...
          val user = UserProfileF(id=None, identifier=username, name=name,
            location=None, about=None, languages=None)
          AsyncRest {
            rest.EntityDAO(EntityType.UserProfile, userOpt).create(user).map { itemOrErr =>
              itemOrErr.right.map { entity =>
                models.sql.OpenIDUser.create(email, entity.id).map { account =>
                  account.setPassword(BCrypt.hashpw(pw, BCrypt.gensalt))
                  Redirect(routes.UserProfiles.get(entity.id))
                }.getOrElse {
                  // FIXME: Handle this - probably by throwing a global error.
                  // If it fails it'll probably die anyway...
                  BadRequest("creating user account failed!")
                }
              }
            }
          }
        }
      }
    )
  }

  def passwordLogin = Action { implicit request =>
    val form = Form(
      tuple(
        "email" -> email,
        "password" -> nonEmptyText
      )
    )
    Ok(views.html.pwLogin(form, routes.Admin.passwordLoginPost))
  }

  def passwordLoginPost = Action { implicit request =>
    val form = Form(
      tuple(
        "email" -> email,
        "password" -> nonEmptyText
      )
    ).bindFromRequest
    val action = routes.Admin.passwordLoginPost

    form.fold(
      errorForm => {
        BadRequest(views.html.pwLogin(errorForm, action))
      },
      data => {
        val (email, pw) = data
        OpenIDUser.findByEmail(email).flatMap { acc =>
          acc.password.flatMap { hashed =>
            if (BCrypt.checkpw(pw, hashed)) {
              Some(Application.gotoLoginSucceeded(acc.profile_id))
            } else {
              None
            }
          }
        } getOrElse {
          Redirect(routes.Admin.passwordLogin).flashing("error" -> Messages("login.badUsernameOrPassword"))
        }
      }
    )
  }

  //
  // Allow a logged-in user to change their account password.
  //
  def changePassword = TODO

  def changePasswordPost = TODO
}