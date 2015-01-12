package controllers.portal

import play.api.libs.concurrent.Execution.Implicits._
import backend.Backend
import com.google.inject.{Inject, Singleton}
import controllers.generic.Search
import controllers.portal.api.{FindChildrenJson, FindJson, GetJson}
import controllers.portal.base.{FindChildren, Find, Get, PortalController}
import defines.EntityType
import models.{UserProfile, DocumentaryUnit, AccountDAO}
import play.api.mvc.RequestHeader
import solr.SolrConstants
import utils.search._
import views.html.p

import scala.concurrent.Future


/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
@Singleton
case class DocumentaryUnits @Inject()(implicit globalConfig: global.GlobalConfig, searchDispatcher: Dispatcher, searchResolver: Resolver, backend: Backend,
                                  userDAO: AccountDAO)
  extends PortalController
  with Get[DocumentaryUnit]
  with GetJson[DocumentaryUnit]
  with Find[DocumentaryUnit]
  with FindJson[DocumentaryUnit]
  with FindChildren[DocumentaryUnit,DocumentaryUnit]
  with FindChildrenJson[DocumentaryUnit, DocumentaryUnit]
  with Search
  with FacetConfig {

  private val portalDocRoutes = controllers.portal.routes.DocumentaryUnits

  def findItems(implicit userOpt: Option[UserProfile], request: RequestHeader): Future[QueryResult[DocumentaryUnit]] = {
    val filters = if (request.getQueryString(SearchParams.QUERY).filterNot(_.trim.isEmpty).isEmpty)
      Map(SolrConstants.TOP_LEVEL -> true) else Map.empty[String,Any]

    find[DocumentaryUnit](
      filters = filters,
      entities = List(EntityType.DocumentaryUnit),
      facetBuilder = docSearchFacets
    )
  }

  def findChildren(id: String)(implicit userOpt: Option[UserProfile], request: RequestHeader): Future[QueryResult[DocumentaryUnit]] = {
    find[DocumentaryUnit](
      filters = Map(SolrConstants.PARENT_ID -> id),
      entities = List(EntityType.DocumentaryUnit),
      facetBuilder = localDocFacets,
      defaultOrder = SearchOrder.Id
    )
  }

  def searchAll = (FindItemsAction andThen FindJsonFilter).apply { implicit request =>
    Ok(p.documentaryUnit.list(request.found.page,
      request.found.params, request.found.facets, portalDocRoutes.searchAll(),
      request.watched))
  }

  def browse(id: String) = (GetItemAction(id) andThen GetJsonFilter).apply { implicit request =>
      if (isAjax) Ok(p.documentaryUnit.itemDetails(request.item, request.annotations, request.links, request.watched))
      else Ok(p.documentaryUnit.show(request.item, request.annotations, request.links, request.watched))
  }

  def search(id: String) = (FindItemChildrenAction(id) andThen FindChildrenJsonFilter).apply { implicit request =>
    if (isAjax) Ok(p.documentaryUnit.childItemSearch(request.item,
      request.found.page, request.found.params, request.found.facets,
      portalDocRoutes.search(id), request.watched))
    else Ok(p.documentaryUnit.search(request.item,
      request.found.page, request.found.params, request.found.facets,
      portalDocRoutes.search(id), request.watched))
  }
}
