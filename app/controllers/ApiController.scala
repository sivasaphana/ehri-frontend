package controllers

import controllers.base.AuthController
import play.api.mvc.Controller
import controllers.base.ControllerHelpers
import play.api.mvc.SimpleResult
import play.api.mvc.ResponseHeader
import play.api.libs.iteratee.Enumerator
import play.api.libs.concurrent.execution.defaultContext

object ApiController extends Controller with AuthController with ControllerHelpers {

  def get(urlpart: String) = userProfileAction { implicit maybeUser =>
    implicit request =>
      Async {
        rest.ApiDAO(maybeUser.flatMap(_.profile))
        	.get(List(urlpart, request.rawQueryString).mkString("?"), request.headers).map { r =>
          SimpleResult(body = Enumerator(r.body), header = ResponseHeader(status = r.status))
        }
      }
  }
}