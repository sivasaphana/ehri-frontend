package controllers

import defines._
import models.SystemEvent
import controllers.base.EntityRead
import play.api.libs.concurrent.Execution.Implicits._


object SystemEvents extends EntityRead[SystemEvent] {
  val entityType = EntityType.SystemEvent
  val contentType = ContentType.SystemEvent

  val builder = SystemEvent

  def get(id: String) = getAction(id) { item => annotations =>
    implicit maybeUser =>
      implicit request =>
      // In addition to the item itself, we also want to fetch the subjects associated with it.
      AsyncRest {
        val params = ListParams.bind(request)
        rest.SystemEventDAO(maybeUser).subjectsFor(id, processParams(params)).map { pageOrErr =>
          pageOrErr.right.map { page =>
            // TODO: Create list params for subjects
            Ok(views.html.systemEvents.show(SystemEvent(item), page, ListParams()))
          }
        }
      }
  }

  def list = listAction { page => params =>
    implicit maybeUser =>
      implicit request =>
        Ok(views.html.systemEvents.list(page.copy(list = page.list.map(SystemEvent(_))), params))
  }
}