package controllers.portal.base

import backend.BackendContentType
import controllers.generic.Search
import models.UserProfile
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{ActionTransformer, Request, RequestHeader, WrappedRequest}

import scala.concurrent.Future

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
trait FindChildren[MT,CMT] extends Get[MT] {
  this: PortalController with Search =>

  def findChildren(id: String)(implicit userOpt: Option[UserProfile], request: RequestHeader): Future[QueryResult[CMT]]

  case class ItemFindChildrenRequest[A](
    item: MT,
    found: QueryResult[CMT],
    watched: Seq[String],
    userOpt: Option[UserProfile],
    request: Request[A]                                
  ) extends WrappedRequest[A](request)
    with WithOptionalUser

  def FindItemChildrenAction(id: String)(implicit ct: BackendContentType[MT], cct: BackendContentType[CMT]) =
    GetItemAction(id) andThen new ActionTransformer[ItemBrowseRequest, ItemFindChildrenRequest] {
      override protected def transform[A](request: ItemBrowseRequest[A]): Future[ItemFindChildrenRequest[A]] = {
        implicit val req = request
        findChildren(id).map { qf =>
          ItemFindChildrenRequest(request.item, qf, request.watched, request.userOpt, request)
        }
      }
    }
}
