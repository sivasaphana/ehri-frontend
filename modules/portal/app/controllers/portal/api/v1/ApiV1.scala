package controllers.portal.api.v1

import javax.inject.Inject

import auth.AccountManager
import backend.rest.ItemNotFound
import backend.{Resource, AnonymousUser, DataApi}
import controllers.generic.Search
import controllers.portal.base.PortalController
import defines.{EnumUtils, EnumerationBinders, EntityType}
import models._
import models.base.AnyModel
import play.api.cache.CacheApi
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsObject, JsValue, Writes, Json}
import play.api.libs.ws.WSClient
import play.api.mvc.{RequestHeader, PathBindable, Action}
import play.api.routing.Router
import utils.MovedPageLookup
import utils.search.{SearchEngine, SearchItemResolver}

import scala.concurrent.ExecutionContext

object ApiSupportedType extends Enumeration {
  val DocumentaryUnit = Value(EntityType.DocumentaryUnit.toString)
  val Repository = Value(EntityType.Repository.toString)
  val HistoricalAgent = Value(EntityType.HistoricalAgent.toString)
  val Country = Value(EntityType.Country.toString)
  val SystemEvent = Value(EntityType.SystemEvent.toString)

  implicit val _writes = EnumUtils.enumFormat(this)
  implicit val _binder = EnumerationBinders.bindableEnum(ApiSupportedType)
}

case class ApiDocumentaryUnitDescription(
    localId: Option[String],
    languageCode: String,
    name: String,
    parallelFormsOfName: Seq[String],
    extentAndMedium: Option[String],
    acquisition: Option[String],
    archivalHistory: Option[String],
    scopeAndContent: Option[String]
)

object ApiDocumentaryUnitDescription {
  implicit val writes = Json.writes[ApiDocumentaryUnitDescription]

  def apply(d: DocumentaryUnitDescriptionF): ApiDocumentaryUnitDescription =
    new ApiDocumentaryUnitDescription(
      d.localId,
      d.languageCode,
      d.name,
      parallelFormsOfName = d.identity.parallelFormsOfName.getOrElse(Seq.empty),
      extentAndMedium = d.identity.extentAndMedium,
      acquisition = d.context.acquisition,
      archivalHistory = d.context.archivalHistory,
      scopeAndContent =  d.content.scopeAndContent
    )
}

case class ApiDocumentaryUnit(
  ehriId: String,
  `type`: String,
  url: String,
  descriptions: Seq[ApiDocumentaryUnitDescription],
  holderUrl: Option[String],
  parentUrl: Option[String]
)

object ApiDocumentaryUnit {
  implicit val writes = Json.writes[ApiDocumentaryUnit]

  def apply(d: DocumentaryUnit)(implicit request: RequestHeader): ApiDocumentaryUnit =
    new ApiDocumentaryUnit(
      ehriId = d.id,
      `type` = d.isA.toString,
      url = controllers.portal.api.v1.routes.ApiV1.fetch(d.id).absoluteURL(),
      descriptions = d.descriptions.map(ApiDocumentaryUnitDescription.apply),
      holderUrl = d.holder.map { r =>
        controllers.portal.api.v1.routes.ApiV1.fetch(r.id).absoluteURL()
      },
      parentUrl = d.parent.map { p =>
        controllers.portal.api.v1.routes.ApiV1.fetch(p.id).absoluteURL()
      }
    )
}

case class ApiAddress(
  name: Option[String],
  contactPerson: Option[String] = None,
  streetAddress: Option[String] = None,
  city: Option[String] = None,
  region: Option[String] = None,
  postalCode: Option[String] = None,
  countryCode: Option[String] = None,
  email: Seq[String] = Nil,
  telephone: Seq[String] = Nil,
  fax: Seq[String] = Nil,
  url: Seq[String] = Nil
)

object ApiAddress {
  implicit val writes = Json.writes[ApiAddress]

  def apply(a: AddressF) = new ApiAddress(
    a.name,
    a.contactPerson,
    a.streetAddress,
    a.city,
    a.region,
    a.postalCode,
    a.countryCode,
    a.email,
    a.telephone,
    a.fax,
    a.url
  )
}

case class ApiRepository(
  ehriId: String,
  `type`: String,
  url: String,
  countryUrl: Option[String],
  address: Option[ApiAddress]
)

object ApiRepository {
  implicit val writes = Json.writes[ApiRepository]
  
  def apply(r: Repository)(implicit request: RequestHeader): ApiRepository =
    new ApiRepository(
      ehriId = r.id,
      `type` = r.isA.toString,
      url = controllers.portal.api.v1.routes.ApiV1.fetch(r.id).absoluteURL(),     
      countryUrl = r.country.map { c =>
        controllers.portal.api.v1.routes.ApiV1.fetch(c.id).absoluteURL()
      },
      address = r.descriptions.flatMap(_.addresses)
        .headOption.map(a => ApiAddress.apply(a))
    )
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
  implicit val userOpt: Option[UserProfile] = None

  private def unwrap[T](s: Option[Seq[T]]): Seq[T] = s.toSeq.flatten

  implicit def anyModelWrites(implicit request: RequestHeader): Writes[AnyModel] = new Writes[AnyModel] {
    override def writes(any: AnyModel): JsValue = any match {
      case doc: DocumentaryUnit => Json.toJson(ApiDocumentaryUnit.apply(doc)(request))
      case repo: Repository => Json.toJson(ApiRepository.apply(repo)(request))
      case agent: HistoricalAgent => ???
      case country: Country => ???
      case _ => throw new UnsupportedOperationException
    }
  }

  def search(q: Option[String], skip: Int, limit: Int) = Action { implicit request =>
    ???
  }

  def fetch(id: String) = Action.async { implicit request =>
    userDataApi.getAny[AnyModel](id).map { item =>
      Ok(Json.toJson(item))
    } recover {
      case e: ItemNotFound => NotFound(Json.obj(
        "error" -> e.message,
        "value" -> e.value,
        "key" -> e.key)
      )
    }
  }

  def searchIn(id: String, q: Option[String], skip: Int, limit: Int) = Action { implicit request =>
    ???
  }

  def annotations(id: String, skip: Int, limit: Int) = Action { implicit request =>
    ???
  }

  def links(id: String, skip: Int, limit: Int) = Action { implicit request =>
    ???
  }

//  def types() = Action { implicit request =>
//    Ok(Json.toJson(ApiSupportedType.values.toList))
//  }
//
//  def list(entityType: ApiSupportedType.Value) = Action.async { implicit request =>
//    implicit val userOpt: Option[UserProfile] = None
//    find[AnyModel](entities = Seq(EntityType.withName(entityType.toString))).map { result =>
//      implicit val w: Writes[AnyModel] = client.json.anyModelJson.clientWrites
//      val data = result.page.items.map(_._1)
//      Ok(Json.toJson(data))
//    }
//  }
//
//  def get(entityType: ApiSupportedType.Value, id: String) = Action.async { implicit request =>
//    userDataApi.getType[AnyModel](entityType.toString, id).map { item =>
//      Ok(Json.toJson(item)(client.json.anyModelJson.clientWrites))
//    } recover {
//      case e: ItemNotFound => NotFound(Json.obj(
//        "error" -> e.message,
//        "value" -> e.value,
//        "key" -> e.key)
//      )
//    }
//  }
}
