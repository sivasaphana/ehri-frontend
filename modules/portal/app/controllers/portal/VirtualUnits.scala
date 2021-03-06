package controllers.portal

import auth.AccountManager
import backend.rest.cypher.Cypher
import controllers.base.SearchVC
import controllers.generic.Search
import models._
import models.base.AnyModel
import play.api.cache.CacheApi
import play.api.i18n.MessagesApi
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.RequestHeader
import utils.MovedPageLookup
import utils.search._
import defines.EntityType
import backend.{IdGenerator, Backend}
import javax.inject._
import views.MarkdownRenderer

import scala.concurrent.Future
import scala.concurrent.Future.{successful => immediate}
import controllers.portal.base.{Generic, PortalController}


@Singleton
case class VirtualUnits @Inject()(
  implicit app: play.api.Application,
  cache: CacheApi,
  globalConfig: global.GlobalConfig,
  searchEngine: SearchEngine,
  searchResolver: SearchItemResolver,
  backend: Backend,
  accounts: AccountManager,
  idGenerator: IdGenerator,
  pageRelocator: MovedPageLookup,
  messagesApi: MessagesApi,
  markdown: MarkdownRenderer,
  cypher: Cypher
) extends PortalController
  with Generic[VirtualUnit]
  with Search
  with SearchVC
  with FacetConfig {

  private val vuRoutes = controllers.portal.routes.VirtualUnits

  def browseVirtualCollection(id: String) = GetItemAction(id).apply { implicit request =>
    if (isAjax) Ok(views.html.virtualUnit.itemDetailsVc(
      request.item, request.annotations, request.links, request.watched))
    else Ok(views.html.virtualUnit.show(
      request.item, request.annotations, request.links, request.watched))
  }

  private def filtersOrIds(item: AnyModel)(implicit request: RequestHeader): Future[Map[String,Any]] = {
    import SearchConstants._
    item match {
      // If we don't have an active query, this is just the immediate children
      case _ if !hasActiveQuery(request) => immediate(buildChildSearchFilter(item))
      // If we do, and the item is a virtual unit, we need to specifically query
      // the immediate leaf nodes and construct an ID-or-ANCESTOR query
      case v: VirtualUnit => descendantIds(item).map { seq =>
        if (seq.isEmpty) Map(ITEM_ID -> "__NO_VALID_ID__")
        else Map(s"$ITEM_ID:(${seq.mkString(" ")}) OR $ANCESTOR_IDS:(${seq.mkString(" ")})" -> Unit)
      }
      // otherwise, for documentary units, we can just query
      // all ancestors
      case d => immediate(Map(s"$ANCESTOR_IDS:${item.id}" -> Unit))
    }
  }

  def searchVirtualCollection(id: String) = GetItemAction(id).async { implicit request =>
    for {
      filters <- filtersOrIds(request.item)
      result <- find[AnyModel](
        filters = filters,
        entities = List(EntityType.VirtualUnit, EntityType.DocumentaryUnit),
        facetBuilder = docSearchFacets
      )
    } yield {
      if (isAjax) Ok(views.html.virtualUnit.childItemSearch(request.item, result,
        vuRoutes.searchVirtualCollection(id), request.watched))
      else Ok(views.html.virtualUnit.search(request.item, result,
        vuRoutes.searchVirtualCollection(id), request.watched))
    }
  }

  def browseVirtualCollections = UserBrowseAction.async { implicit request =>
    val filters = if (request.getQueryString(SearchParams.QUERY).forall(_.trim.isEmpty))
      Map(SearchConstants.TOP_LEVEL -> true) else Map.empty[String,Any]

    find[VirtualUnit](
      filters = filters,
      entities = List(EntityType.VirtualUnit),
      facetBuilder = docSearchFacets
    ).map { result =>
      Ok(views.html.virtualUnit.list(result, vuRoutes.browseVirtualCollections(),
        request.watched))
    }
  }

  def browseVirtualUnit(pathStr: String, id: String) = OptionalUserAction.async { implicit request =>
    val pathIds = pathStr.split(",").toSeq
    val pathF: Future[Seq[AnyModel]] = Future.sequence(pathIds.map(pid => userBackend.getAny[AnyModel](pid)))
    val itemF: Future[AnyModel] = userBackend.getAny[AnyModel](id)
    val linksF: Future[Seq[Link]] = userBackend.getLinksForItem[Link](id)
    val annsF: Future[Seq[Annotation]] = userBackend.getAnnotationsForItem[Annotation](id)
    val watchedF: Future[Seq[String]] = watchedItemIds(userIdOpt = request.userOpt.map(_.id))
    for {
      watched <- watchedF
      item <- itemF
      links <- linksF
      annotations <- annsF
      path <- pathF
    } yield {
      if (isAjax) Ok(views.html.virtualUnit.itemDetailsVc(item, annotations, links, watched, path))
      else Ok(views.html.virtualUnit.show(item, annotations, links, watched, path))
    }
  }

  def searchVirtualUnit(pathStr: String, id: String) = OptionalUserAction.async { implicit request =>
    val pathIds = pathStr.split(",").toSeq
    val pathF: Future[Seq[AnyModel]] = Future.sequence(pathIds.map(pid => userBackend.getAny[AnyModel](pid)))
    val itemF: Future[AnyModel] = userBackend.getAny[AnyModel](id)
    val watchedF: Future[Seq[String]] = watchedItemIds(userIdOpt = request.userOpt.map(_.id))
    for {
      watched <- watchedF
      item <- itemF
      path <- pathF
      filters <- filtersOrIds(item)
      result <- find[AnyModel](
        filters = filters,
        entities = List(EntityType.VirtualUnit, EntityType.DocumentaryUnit),
        facetBuilder = docSearchFacets
      )
    } yield {
      if (isAjax)
        Ok(views.html.virtualUnit.childItemSearch(item, result,
          vuRoutes.searchVirtualUnit(pathStr, id), watched, path))
      else Ok(views.html.virtualUnit.search(item, result,
          vuRoutes.searchVirtualUnit(pathStr, id), watched, path))
    }
  }
}

