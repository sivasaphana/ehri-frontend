package controllers.portal.api.v1

import javax.inject.Inject

import auth.AccountManager
import backend.rest.ItemNotFound
import backend.{Resource, AnonymousUser, DataApi}
import controllers.generic.Search
import controllers.portal.base.PortalController
import defines.{EnumUtils, EnumerationBinders, EntityType}
import models.UserProfile
import models.base.AnyModel
import play.api.cache.CacheApi
import play.api.i18n.MessagesApi
import play.api.libs.json.{Writes, Json}
import play.api.libs.ws.WSClient
import play.api.mvc.{PathBindable, Action}
import play.api.routing.Router
import utils.MovedPageLookup
import utils.search.{SearchEngine, SearchItemResolver}

import scala.concurrent.ExecutionContext

object ApiSupportedType extends Enumeration {
  val DocumentaryUnit = Value(EntityType.DocumentaryUnit.toString)
  val Repository = Value(EntityType.Repository.toString)
  val HistoricalAgent = Value(EntityType.HistoricalAgent.toString)
  val Country = Value(EntityType.Country.toString)

  implicit val _writes = EnumUtils.enumFormat(this)
  implicit val _binder = EnumerationBinders.bindableEnum(ApiSupportedType)
}

case class ApiV1 @Inject()(
  implicit config: play.api.Configuration,
  cache: CacheApi,
  globalConfig: global.GlobalConfig,
  searchEngine: SearchEngine,
  searchResolver: SearchItemResolver,
  dataApi: DataApi,
  accounts: AccountManager,
  pageRelocator: MovedPageLookup,
  messagesApi: MessagesApi,
  ws: WSClient,
  executionContext: ExecutionContext
) extends PortalController with Search {

  implicit val apiUser = AnonymousUser

  def types() = Action { implicit request =>
    Ok(Json.toJson(ApiSupportedType.values.toList))
  }

  def list(entityType: ApiSupportedType.Value) = Action.async { implicit request =>
    implicit val userOpt: Option[UserProfile] = None
    find[AnyModel](entities = Seq(EntityType.withName(entityType.toString))).map { result =>
      implicit val w: Writes[AnyModel] = client.json.anyModelJson.clientWrites
      val data = result.page.items.map(_._1)
      Ok(Json.toJson(data))
    }
  }

  def get(entityType: ApiSupportedType.Value, id: String) = Action.async { implicit request =>
    userDataApi.getType[AnyModel](entityType.toString, id).map { item =>
      Ok(Json.toJson(item)(client.json.anyModelJson.clientWrites))
    } recover {
      case e: ItemNotFound => NotFound(Json.obj(
        "error" -> e.message,
        "value" -> e.value,
        "key" -> e.key)
      )
    }
  }
}
