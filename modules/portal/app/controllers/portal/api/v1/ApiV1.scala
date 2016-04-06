package controllers.portal.api.v1

import javax.inject.Inject

import auth.AccountManager
import backend.rest.ItemNotFound
import backend.{AnonymousUser, DataApi}
import controllers.portal.base.PortalController
import models.base.AnyModel
import play.api.cache.CacheApi
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.Action
import utils.MovedPageLookup
import utils.search.{SearchEngine, SearchItemResolver}

import scala.concurrent.ExecutionContext

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
) extends PortalController {

  implicit val apiUser = AnonymousUser

  def get(entityType: defines.EntityType.Value, id: String) = Action.async { implicit request =>
    userDataApi.getAny[AnyModel](id).map { item =>
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
