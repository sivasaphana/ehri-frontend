package controllers.base

import play.api.libs.concurrent.Execution.Implicits._
import models.base.AccessibleEntity
import play.api.mvc._
import models.{SystemEvent, Annotation, Entity, UserProfile}
import rest.{EntityDAO, Page}
import play.api.data.Form
import rest.RestPageParams
import play.api.Logger
import controllers.ListParams


/**
 * Controller trait which handles the listing and showing of Entities that
 * implement the AccessibleEntity trait.
 *
 * @tparam T
 */
trait EntityRead[T <: AccessibleEntity] extends EntityController[T] {
  val DEFAULT_LIMIT = 20

  val defaultPage: RestPageParams = new RestPageParams()
  val defaultChildPage: RestPageParams = new RestPageParams()

  def getEntity(id: String, user: Option[UserProfile])(f: Entity => Result)(implicit request: RequestHeader) = {
    implicit val maybeUser = user
    AsyncRest {
      rest.EntityDAO(entityType, maybeUser).get(id).map { itemOrErr =>
        itemOrErr.right.map(f)
      }
    }
  }

  def getGroups(user: Option[UserProfile])(f: Seq[(String,String)] => Seq[(String,String)] => Result)(implicit request: RequestHeader) = {
    implicit val maybeUser = user
    // TODO: Handle REST errors
    Async {
      for {
        users <- rest.RestHelpers.getUserList
        groups <- rest.RestHelpers.getGroupList
      } yield {
        f(users)(groups)
      }
    }
  }

  def getJson(id: String) = userProfileAction { implicit maybeUser =>
    implicit request =>
      import play.api.libs.json.Json
      AsyncRest {
        rest.EntityDAO(entityType, maybeUser).get(id).map { itemOrErr =>
          itemOrErr.right.map {
            item => Ok(Json.toJson(item.data))
          }
        }
      }
  }

  def getAction(id: String)(f: Entity => Map[String,List[Annotation]] => Option[UserProfile] => Request[AnyContent] => Result) = {
    itemPermissionAction(contentType, id) { item => implicit maybeUser =>
      implicit request =>
      Secured {
        AsyncRest {
          // NB: Effectively disable paging here by using a high limit
          val annsReq = rest.AnnotationDAO(maybeUser).getFor(id)
          for { annOrErr <- annsReq } yield {
            for { anns <- annOrErr.right } yield {
              f(item)(anns)(maybeUser)(request)
            }
          }
        }
      }
    }
  }

  def getWithChildrenAction[C <: AccessibleEntity](id: String, builder: Entity => C)(
      f: Entity => rest.Page[C] => ListParams =>  Map[String,List[Annotation]] => Option[UserProfile] => Request[AnyContent] => Result) = {
    itemPermissionAction(contentType, id) { item => implicit maybeUser =>
      implicit request =>
      Secured {
        AsyncRest {
          // NB: Effectively disable paging here by using a high limit
          val params = ListParams.bind(request)
          val annsReq = rest.AnnotationDAO(maybeUser).getFor(id)
          val cReq = rest.EntityDAO(entityType, maybeUser).pageChildren(id, processChildParams(params))
          for { annOrErr <- annsReq ; cOrErr <- cReq } yield {
            for { anns <- annOrErr.right ; children <- cOrErr.right } yield {
              f(item)(children.copy(list = children.list.map(builder(_))))(params)(anns)(maybeUser)(request)
            }
          }
        }
      }
    }
  }

  def listAction(f: rest.Page[Entity] => ListParams => Option[UserProfile] => Request[AnyContent] => Result) = {
    userProfileAction { implicit maybeUser =>
      implicit request =>
      Secured {
        AsyncRest {
          val params = ListParams.bind(request)
          rest.EntityDAO(entityType, maybeUser).page(processParams(params)).map { itemOrErr =>
            itemOrErr.right.map {
              page => f(page)(params)(maybeUser)(request)
            }
          }
        }
      }
    }
  }


  def historyAction[C <: AccessibleEntity](id: String)(
      f: Entity => rest.Page[SystemEvent] => Option[UserProfile] => Request[AnyContent] => Result) = {
    userProfileAction { implicit maybeUser => implicit request =>
      Secured {
        AsyncRest {
          val itemReq = rest.EntityDAO(entityType, maybeUser).get(id)
          // NOTE: we do not use the controller's RestPageParams here because
          // they won't apply to Event items...???
          val alReq = rest.SystemEventDAO(maybeUser).history(id, RestPageParams())
          for { itemOrErr <- itemReq ; alOrErr <- alReq  } yield {
            for { item <- itemOrErr.right ; al <- alOrErr.right  } yield {
              f(item)(al.copy(list = al.list.map(SystemEvent(_))))(maybeUser)(request)
            }
          }
        }
      }
    }
  }

  /**
   * Process parameters for the main list form.
   * @param listParams
   * @return
   */
  def processParams(listParams: ListParams): RestPageParams = {
    // don't do anything by default except for copy the page and limit
    // Inheriting controllers can override this to handle translating
    // filter values to the internal REST format.
    RestPageParams(listParams.page, listParams.limit)
  }

  /**
   * Process parameters for the child form.
   * @param listParams
   * @return
   */
  def processChildParams(listParams: ListParams): RestPageParams = processParams(listParams)
}
