package controllers.portal.api

import backend.BackendContentType
import client.json.ClientWriteable
import controllers.generic.Search
import controllers.portal.base.{Find, PortalController}
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{ActionFilter, Result}
import utils.search.ItemPage

import scala.concurrent.Future
import scala.concurrent.Future.{successful => immediate}

trait FindJson[MT] extends PortalController {

  this: Search with Find[MT] =>

  import play.api.libs.json._
  protected implicit def itemPageWriter[T](implicit w: ClientWriteable[T]): Writes[ItemPage[T]] = new Writes[ItemPage[T]] {
    override def writes(p: ItemPage[T]): JsValue = Json.obj(
      "items" -> Writes.seq(w.clientFormat).writes(p.items),
      "offset" -> JsNumber(p.offset),
      "limit" -> JsNumber(p.limit),
      "total" -> JsNumber(p.total)
    )
  }

  protected def dataPage[T](q: QueryResult[T]): ItemPage[T] =
    q.page.copy(items = q.page.items.map(_._1))

  def FindJsonFilter(implicit ct: BackendContentType[MT], w: ClientWriteable[MT]) = new ActionFilter[ItemFindRequest] {
    override protected def filter[A](request: ItemFindRequest[A]): Future[Option[Result]] = immediate {
      val accepts: Option[String] = request.request.headers.get(HeaderNames.ACCEPT)
      if (accepts.exists(_.contains(MimeTypes.JSON))) Some(Ok(Json.toJson(dataPage(request.found))))
      else None
    }
  }
}