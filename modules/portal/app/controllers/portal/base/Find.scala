package controllers.portal.base

import backend.BackendContentType
import controllers.generic.{Search, Read}
import models.{Annotation, Link, UserProfile}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.{RequestHeader, ActionTransformer, Request, WrappedRequest}
import utils.Page
import utils.search.QueryResponse

import scala.concurrent.Future

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
trait Find[MT] extends Read[MT] {
  this: PortalController with Search =>

  def findItems(implicit userOpt: Option[UserProfile], request: RequestHeader): Future[QueryResult[MT]]

  case class ItemFindRequest[A](
    found: QueryResult[MT],
    watched: Seq[String],
    userOpt: Option[UserProfile],
    request: Request[A]                                
  ) extends WrappedRequest[A](request)
    with WithOptionalUser

  def FindItemsAction(implicit ct: BackendContentType[MT]) =
    OptionalUserAction andThen new ActionTransformer[OptionalUserRequest, ItemFindRequest] {
      override protected def transform[A](request: OptionalUserRequest[A]): Future[ItemFindRequest[A]] = {
        implicit val req = request
        val watchedF: Future[Seq[String]] = watchedItemIds(request.userOpt.map(_.id))
        val qrF: Future[QueryResult[MT]] = findItems
        for {
          watched <- watchedF
          qf <- qrF
        } yield ItemFindRequest(qf, watched, request.userOpt, request)
      }
    }
}
