package controllers.base

import play.api.mvc._
import jp.t2v.lab.play20.auth.{LoginLogout,Auth}

trait LoginHandler extends Controller with Auth with LoginLogout with Authorizer {

  implicit val globalConfig: global.GlobalConfig

  def logout = optionalUserAction { implicit maybeUser =>
    implicit request =>
      gotoLogoutSucceeded
  }
}