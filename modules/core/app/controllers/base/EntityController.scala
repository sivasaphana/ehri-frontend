package controllers.base

import play.api.mvc.{Request, AnyContent, Controller}
import defines.{ContentTypes,EntityType}
import models.UserProfile
import rest.{Backend, ApiUser}

trait EntityController extends Controller with AuthController with ControllerHelpers {
  val entityType: EntityType.Value
  val contentType: ContentTypes.Value

  val backend: Backend

  final val LOG_MESSAGE_PARAM = "logMessage"

  def getLogMessage(implicit request: Request[AnyContent]) = {
    import play.api.data.Form
    import play.api.data.Forms._
    Form(single(LOG_MESSAGE_PARAM -> optional(nonEmptyText)))
      .bindFromRequest.value.getOrElse(None)
  }
}
