package controllers.portal.api

import backend.BackendContentType
import client.json.ClientWriteable
import controllers.portal.base.{Find, PortalController, Get}
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.Json
import play.api.mvc.{Result, ActionFilter}
import scala.concurrent.{Future => immediate}

import scala.concurrent.Future

trait GetJson[MT] extends PortalController {

  this: Get[MT] =>

  import play.api.libs.concurrent.Execution.Implicits._
  def GetJsonFilter(implicit ct: BackendContentType[MT], w: ClientWriteable[MT]) = new ActionFilter[ItemBrowseRequest] {
    override protected def filter[A](request: ItemBrowseRequest[A]): Future[Option[Result]] = immediate {
      val accepts: Option[String] = request.request.headers.get(HeaderNames.ACCEPT)
      if (accepts.exists(_.contains(MimeTypes.JSON))) Some(Ok(Json.toJson(request.item)(w.clientFormat)))
      else None
    }
  }
}