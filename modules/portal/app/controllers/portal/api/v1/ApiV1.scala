package controllers.portal.api.v1

import javax.inject.{Singleton, Inject}

import auth.AccountManager
import backend.rest.ItemNotFound
import backend.{AnonymousUser, DataApi}
import controllers.base.{AuthConfigImpl, ControllerHelpers, CoreActionBuilders}
import controllers.generic.Search
import defines.EntityType
import models._
import models.base.AnyModel
import play.api.cache.CacheApi
import play.api.http.HeaderNames
import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import utils.{Page, MovedPageLookup}
import utils.search.{SearchConstants, SearchParams, SearchEngine, SearchItemResolver}

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.Future.{successful => immediate}


case class DocumentaryUnitDescriptionAttrs(
    localId: Option[String],
    languageCode: String,
    name: String,
    parallelFormsOfName: Seq[String],
    extentAndMedium: Option[String],
    acquisition: Option[String],
    archivalHistory: Option[String],
    scopeAndContent: Option[String]
)

object DocumentaryUnitDescriptionAttrs {
  implicit val writes = Json.writes[DocumentaryUnitDescriptionAttrs]

  def apply(d: DocumentaryUnitDescriptionF): DocumentaryUnitDescriptionAttrs =
    new DocumentaryUnitDescriptionAttrs(
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

case class DocumentaryUnitAttrs(
  localId: String,
  alternateIds: Seq[String],
  descriptions: Seq[DocumentaryUnitDescriptionAttrs]
)

object DocumentaryUnitAttrs {
  implicit val writes = Json.writes[DocumentaryUnitAttrs]

  def apply(d: DocumentaryUnit): DocumentaryUnitAttrs =
    new DocumentaryUnitAttrs(
      localId = d.model.identifier,
      alternateIds = d.model.otherIdentifiers.getOrElse(Seq.empty),
      descriptions = d.descriptions.map(DocumentaryUnitDescriptionAttrs.apply)
    )
}

case class AddressAttrs(
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

object AddressAttrs {
  implicit val writes = Json.writes[AddressAttrs]

  def apply(a: AddressF) = new AddressAttrs(
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

case class RepositoryAttrs(
  name: Option[String] = None,
  parallelFormsOfName: Option[Seq[String]] = None,
  otherFormsOfName: Option[Seq[String]] = None,
  address: Option[AddressAttrs] = None,
  history: Option[String] = None,
  generalContext: Option[String] = None,
  mandates: Option[String] = None,
  administrativeStructure: Option[String] = None,
  records: Option[String] = None,
  buildings: Option[String] = None,
  holdings: Option[String] = None,
  findingAids: Option[String] = None,
  openingTimes: Option[String] = None,
  conditions: Option[String] = None,
  accessibility: Option[String] = None,
  researchServices: Option[String] = None,
  reproductionServices: Option[String] = None,
  publicAreas: Option[String] = None
)

object RepositoryAttrs {
  implicit val writes = Json.writes[RepositoryAttrs]
  
  def apply(r: Repository): RepositoryAttrs = {
    r.descriptions.headOption.map { d =>
      new RepositoryAttrs(
        name = Some(d.name),
        parallelFormsOfName = d.parallelFormsOfName,
        otherFormsOfName = d.parallelFormsOfName,
        address = d.addresses.headOption.map(a => AddressAttrs(a)),
        history = d.details.history,
        generalContext = d.details.generalContext,
        mandates = d.details.mandates,
        administrativeStructure = d.details.administrativeStructure,
        records = d.details.records,
        buildings = d.details.buildings,
        holdings = d.details.holdings,
        openingTimes = d.access.openingTimes,
        conditions = d.access.conditions,
        accessibility = d.access.accessibility,
        researchServices = d.services.researchServices,
        reproductionServices = d.services.reproductionServices,
        publicAreas = d.services.publicAreas
      )
    }.getOrElse {
      RepositoryAttrs()
    }
  }
}

case class DocumentaryUnitLinks(
  self: String,
  search: String,
  holder: Option[String],
  parent: Option[String]
)

object DocumentaryUnitLinks {
  implicit val writes = Json.writes[DocumentaryUnitLinks]
}

case class DocumentaryUnitRelations(
  holder: Option[JsValue],
  parent: Option[JsValue]
)

object DocumentaryUnitRelations {
  // Not using default writes here because missing (Optional)
  // relations should be expressed using null
  // http://jsonapi.org/format/#document-resource-object-related-resource-links
  implicit val writes = new Writes[DocumentaryUnitRelations] {
    def writes(r: DocumentaryUnitRelations): JsValue = Json.obj(
      "holder" ->r.holder,
      "parent" -> r.parent
    )
  }
}

case class HistoricalAgentAttrs(
  datesOfExistence: Option[String] = None,
  history: Option[String] = None,
  places: Option[Seq[String]] = None,
  legalStatus: Option[Seq[String]] = None,
  functions: Option[Seq[String]] = None,
  mandates: Option[Seq[String]] = None,
  internalStructure: Option[String] = None,
  generalContext: Option[String] = None
)

object HistoricalAgentAttrs {
  implicit val writes = Json.writes[HistoricalAgentAttrs]

  def apply(a: HistoricalAgent): HistoricalAgentAttrs = {
    a.descriptions.headOption.map { d =>
      new HistoricalAgentAttrs(
        datesOfExistence = d.details.datesOfExistence,
        history = d.details.history,
        places = d.details.places,
        legalStatus = d.details.legalStatus,
        functions = d.details.functions,
        mandates = d.details.mandates,
        internalStructure = d.details.internalStructure,
        generalContext = d.details.generalContext
      )
    }.getOrElse {
      HistoricalAgentAttrs()
    }
  }
}

case class RepositoryLinks(
  self: String,
  search: String,
  country: Option[String]
)

object RepositoryLinks {
  implicit val writes = Json.writes[RepositoryLinks]
}

case class RepositoryRelations(
  country: Option[JsValue]
)

object RepositoryRelations {
  implicit val writes = Json.writes[RepositoryRelations]
}

case class CountryAttrs(
  identifier: String,
  `abstract`: Option[String],
  history: Option[String],
  situation: Option[String],
  summary: Option[String],
  extensive: Option[String]
)

object CountryAttrs {
  implicit val writes = Json.writes[CountryAttrs]

  def apply(c: Country)(implicit requestHeader: RequestHeader): CountryAttrs =
    new CountryAttrs(
      identifier = c.model.identifier,
      `abstract` = c.model.abs,
      history = c.model.history,
      situation = c.model.situation,
      summary = c.model.summary,
      extensive = c.model.extensive
    )
}

case class CountryLinks(
  self: String,
  search: String
)

object CountryLinks {
  implicit val writes = Json.writes[CountryLinks]
}

case class JsonApiResponse(
  data: JsValue,
  links: Option[JsValue] = None,
  included: Option[JsValue] = None
)

object JsonApiResponse {
  implicit val writes = Json.writes[JsonApiResponse]
}

case class JsonApiResponseData(
  id: String,
  `type`: String,
  attributes: JsValue,
  relationships: Option[JsValue] = None,
  links: Option[JsValue] = None
)

object JsonApiResponseData {
  implicit val writes = Json.writes[JsonApiResponseData]
}

case class ResourceIdentifier(
  id: String,
  `type`: String
)

object ResourceIdentifier {
  implicit val writes = Json.writes[ResourceIdentifier]

  def apply(m: AnyModel) = new ResourceIdentifier(m.id, m.isA.toString)
}

@Singleton
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
) extends CoreActionBuilders
  with ControllerHelpers
  with AuthConfigImpl
  with Search {

  private final val JSONAPI_MIMETYPE = "application/vnd.api+json"

  implicit val apiUser = AnonymousUser
  implicit val userOpt: Option[UserProfile] = None

  private val apiRoutes = controllers.portal.api.v1.routes.ApiV1
  private val apiSupportedEntities = Seq(
    EntityType.DocumentaryUnit,
    EntityType.Repository,
    EntityType.HistoricalAgent,
    EntityType.Country
  )

  private def error(status: Int, message: String): Result =
    Status(status)(errorJson(status, message))

  private def errorJson(status: Int, message: String): JsObject = Json.obj(
    "errors" -> Json.arr(
      Json.obj(
        "status" -> status.toString,
        "detail" -> message
      )
    )
  )

  private def checkRateLimit[A](request: Request[A]): Boolean = true // placeholder

  private object RateLimit extends ActionBuilder[Request] {
    override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
      if (request.method != "POST") block(request)
      else {
        if (checkRateLimit(request)) block(request)
        else immediate(error(TOO_MANY_REQUESTS, "Too many requests"))
      }
    }
  }

  private object JsonApiCheckContentTypeFilter extends ActionFilter[Request] {
    override protected def filter[A](request: Request[A]): Future[Option[Result]] = {
      if (request.headers.getAll(HeaderNames.CONTENT_TYPE).contains(JSONAPI_MIMETYPE))
        immediate(None)
      else immediate(Some(error(UNSUPPORTED_MEDIA_TYPE, "Unsupported media type")))
    }
  }

  private object JsonApiCheckAcceptFilter extends ActionFilter[Request] {
    override protected def filter[A](request: Request[A]): Future[Option[Result]] = {
      // If there is an accept media type for JSON-API than one must be unmodified
      val accept: Seq[String] = request.headers.getAll(HeaderNames.ACCEPT).filter(a =>
        a.startsWith(JSONAPI_MIMETYPE))
      if (accept.isEmpty || accept.contains(JSONAPI_MIMETYPE)) immediate(None)
      else immediate(Some(error(NOT_ACCEPTABLE, "Not acceptable")))
    }
  }

  private def JsonApiAction = RateLimit andThen
      JsonApiCheckContentTypeFilter andThen
      JsonApiCheckAcceptFilter

  implicit def anyModelWrites(implicit request: RequestHeader): Writes[AnyModel] = new Writes[AnyModel] {
    override def writes(any: AnyModel): JsValue = any match {
      case doc: DocumentaryUnit => Json.toJson(
        JsonApiResponseData(
          id = doc.id,
          `type` = doc.isA.toString,
          attributes = Json.toJson(DocumentaryUnitAttrs(doc)),
          relationships = Some(
            Json.toJson(
              DocumentaryUnitRelations(
                holder = doc.holder.map { r =>
                  Json.obj("data" -> ResourceIdentifier(r))
                },
                parent = doc.parent.map { r =>
                  Json.obj("data" -> ResourceIdentifier(r))
                }
              )
            )
          ),
          links = Some(
            Json.toJson(
              DocumentaryUnitLinks(
                self = apiRoutes.fetch(doc.id).absoluteURL(),
                search = apiRoutes.searchIn(doc.id).absoluteURL(),
                holder = doc.holder.map(r => apiRoutes.fetch(r.id).absoluteURL()),
                parent = doc.parent.map(r => apiRoutes.fetch(r.id).absoluteURL())
              )
            )
          )
        )
      )
      case repo: Repository => Json.toJson(
        JsonApiResponseData(
          id = repo.id,
          `type` = repo.isA.toString,
          attributes = Json.toJson(RepositoryAttrs(repo)),
          relationships = Some(
            Json.toJson(
              RepositoryRelations(
                country = repo.country.map { c =>
                  Json.obj("data" -> ResourceIdentifier(c))
                }
              )
            )
          ),
          links = Some(
            Json.toJson(
              RepositoryLinks(
                self = apiRoutes.fetch(repo.id).absoluteURL(),
                search = apiRoutes.searchIn(repo.id).absoluteURL(),
                country = repo.country.map(c => apiRoutes.fetch(c.id).absoluteURL())
              )
            )
          )
        )
      )
      case agent: HistoricalAgent => Json.toJson(
        JsonApiResponseData(
          id = agent.id,
          `type` = agent.isA.toString,
          attributes = Json.toJson(HistoricalAgentAttrs(agent)),
          links = Some(
            Json.obj(
              "self" -> apiRoutes.fetch(agent.id).absoluteURL()
            )
          )
        )
      )
      case country: Country => Json.toJson(
        JsonApiResponseData(
          id = country.id,
          `type` = country.isA.toString,
          attributes = Json.toJson(CountryAttrs(country)),
          links = Some(
            Json.toJson(
              CountryLinks(
                self = apiRoutes.fetch(country.id).absoluteURL(),
                search = apiRoutes.searchIn(country.id).absoluteURL()
              )
            )
          )
        )
      )
      case _ => throw new ItemNotFound()
    }
  }

  private def includedData(any: AnyModel)(implicit requestHeader: RequestHeader): Option[JsValue] = any match {
    case doc: DocumentaryUnit => Some(
      Json.toJson(
        Seq[Option[AnyModel]](
          doc.holder,
          doc.parent
        ).filterNot(_.isEmpty).flatten
      )(Writes.seq(anyModelWrites))
    )
    case _ => None
  }

  private def searchFilterKey(any: AnyModel): String = any match {
    case repo: Repository => SearchConstants.HOLDER_ID
    case country: Country => SearchConstants.COUNTRY_CODE
    case _ => SearchConstants.PARENT_ID
  }

  private def includeKey(any: AnyModel): Option[String] = any match {
    case repo: Repository => Some("holder")
    case country: Country => Some("country")
    case doc: DocumentaryUnit => Some("parent")
    case _ => None
  }

  private def included(item: AnyModel)(implicit requestHeader: RequestHeader): JsValue =
    includeKey(item).map { key =>
      Json.obj(key -> Json.arr(item))
    }.getOrElse {
      Json.obj()
    }

  private def pageJson[T <: AnyModel](page: Page[T],
                                      urlFunc: Int => String,
                                      includedData: Option[JsValue] = None)(implicit w: Writes[T]): JsObject = {
    val json = Json.obj(
      "data" -> page.items,
      "links" -> Json.obj(
        "first" -> urlFunc(1),
        "last" -> urlFunc(page.numPages),
        "prev" -> (if (page.page == 1) Option.empty[String]
        else Some(urlFunc(page.page - 1))),
        "next" -> (if (!page.hasMore) Option.empty[String]
        else Some(urlFunc(page.page + 1)))
      )
    )
    includedData.fold(json)(inc => json.deepMerge(Json.obj("included" -> inc)))
  }

  def index() = JsonApiAction { implicit request =>
    // describe possible actions here...
    Ok(
      Json.obj(
        "meta" -> Json.obj(
          "name" -> "EHRI API V1",
          "routes" -> Json.obj(
            "search" -> (apiRoutes.search().absoluteURL() + "?[q=Text Query]"),
            "fetch" -> apiRoutes.fetch("ITEM-ID").absoluteURL(),
            "search-in" -> (apiRoutes.searchIn("ITEM-ID").absoluteURL() + "?[q=Text Query]")
          ),
          "status" -> "ALPHA: Do not use for production"
        ),
        "jsonapi" -> Json.obj(
          "version" -> "1.0"
        )
      )
    ).as(JSONAPI_MIMETYPE)
  }

  def search(q: Option[String], page: Int) = Action.async { implicit request =>
    val types = request.queryString.getOrElse("type", Seq.empty)
    find[AnyModel](
      defaultParams = SearchParams(query = q, page = Some(page)),
      entities = apiSupportedEntities.filter(e => types.isEmpty ||
        types.exists(_.toString.toLowerCase == e.toString.toLowerCase()))
    ).map { r =>
      Ok(pageJson(r.mapItems(_._1).page, p => apiRoutes.search(q, p).absoluteURL()))
        .as(JSONAPI_MIMETYPE)
    }
  }

  def fetch(id: String) = Action.async { implicit request =>
    userDataApi.getAny[AnyModel](id).map { item =>
      Ok(
        Json.toJson(
          JsonApiResponse(
            data = Json.toJson(item),
            included = includedData(item)
          )
        )
      ).as(JSONAPI_MIMETYPE)
    } recover {
      case e: ItemNotFound => NotFound(errorJson(404, e.message.getOrElse(id)))
    }
  }

  def searchIn(id: String, q: Option[String], page: Int) = Action.async { implicit request =>
    userDataApi.getAny[AnyModel](id).flatMap { item =>
      find[AnyModel](
        filters = Map(searchFilterKey(item) -> id),
        defaultParams = SearchParams(query = q, page = Some(page)),
        entities = apiSupportedEntities
      ).map { r =>
        Ok(pageJson(r.mapItems(_._1).page,
          p => apiRoutes.searchIn(id, q, p).absoluteURL(), Some(included(item)))
        ).as(JSONAPI_MIMETYPE)
      }
    }
  }

  override def authenticationFailed(request: RequestHeader)(implicit context: ExecutionContext): Future[Result] =
    immediate(Unauthorized("not authenticated"))

  override def authorizationFailed(request: RequestHeader,user: UserProfile)(implicit context: ExecutionContext): Future[Result] =
    immediate(error(FORBIDDEN, "Unauthorized"))

  override def authorizationFailed(request: RequestHeader, user: User, authority: Option[Authority])(implicit context: ExecutionContext): Future[Result] =
    immediate(error(FORBIDDEN, "Unauthorized"))

  protected def downForMaintenance(request: RequestHeader)(implicit context: ExecutionContext): Future[Result] =
    immediate(error(SERVICE_UNAVAILABLE, "Unavailable"))

  override protected def notFoundError(request: RequestHeader,msg: Option[String])(implicit context: ExecutionContext): Future[Result] =
    immediate(error(NOT_FOUND, msg.getOrElse("Not Found")))

  override def staffOnlyError(request: RequestHeader)(implicit context: ExecutionContext): Future[Result] =
    immediate(error(FORBIDDEN, "Unauthorized (staff only)"))

  override def verifiedOnlyError(request: RequestHeader)(implicit context: ExecutionContext): Future[Result] =
    immediate(error(FORBIDDEN, "Unauthorized (staff only)"))
}
