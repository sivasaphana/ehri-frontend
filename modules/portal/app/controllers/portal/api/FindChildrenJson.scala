package controllers.portal.api

import backend.BackendContentType
import client.json.ClientWriteable
import controllers.portal.base.{Find, PortalController, FindChildren, Get}
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.Json
import play.api.mvc.{ActionFilter, Result}

import scala.concurrent.Future
import scala.concurrent.Future.{successful => immediate}

trait FindChildrenJson[MT,CMT] extends PortalController {

  this: Get[MT] with Find[MT] with FindChildren[MT, CMT] with FindJson[MT] =>

  def FindChildrenJsonFilter(implicit ct: BackendContentType[MT], w: ClientWriteable[CMT]) = new ActionFilter[ItemFindChildrenRequest] {
    override protected def filter[A](request: ItemFindChildrenRequest[A]): Future[Option[Result]] = immediate {
      val accepts: Option[String] = request.request.headers.get(HeaderNames.ACCEPT)
      if (accepts.exists(_.contains(MimeTypes.JSON))) Some(Ok(Json.toJson(dataPage(request.found))))
      else None
    }
  }
}