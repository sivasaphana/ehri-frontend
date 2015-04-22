package controllers.portal

import auth.AccountManager
import backend.Backend
import com.google.inject.{Inject, Singleton}
import controllers.generic.Search
import controllers.portal.base.{Generic, PortalController}
import models.{Repository, DocumentaryUnit}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.RequestHeader
import utils.search._

import scala.concurrent.Future.{successful => immediate}

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
@Singleton
case class Repositories @Inject()(implicit globalConfig: global.GlobalConfig, searchEngine: SearchEngine, searchResolver: SearchItemResolver, backend: Backend,
                                  accounts: AccountManager, pageRelocator: utils.MovedPageLookup)
  extends PortalController
  with Generic[Repository]
  with Api[Repository]
  with Search
  with FacetConfig {

  private val portalRepoRoutes = controllers.portal.routes.Repositories

  def searchAllJson() = apiSearchAllJson(item => portalRepoRoutes.getJson(item.id).url)
  def getJson(id: String) = apiGetJson(id)

  private def filters(id: String)(implicit request: RequestHeader): Map[String,Any] =
    (if (!hasActiveQuery(request)) Map(SearchConstants.TOP_LEVEL -> true)
      else Map.empty[String,Any]) ++ Map(SearchConstants.HOLDER_ID -> id)


  def searchAll = UserBrowseAction.async { implicit request =>
    findType[Repository](
      facetBuilder = repositorySearchFacets
    ).map { result =>
      Ok(views.html.repository.list(result, portalRepoRoutes.searchAll(),
        request.watched))
    }
  }

  def searchAllByCountry = UserBrowseAction.async { implicit request =>
    findType[Repository](
      defaultParams = SearchParams(sort = Some(SearchOrder.Country)),
      facetBuilder = repositorySearchFacets
    ).map { result =>
      Ok(views.html.repository.listByCountry(result,
        portalRepoRoutes.searchAllByCountry(),
        request.watched))
    }
  }

  def browse(id: String) = GetItemAction(id).async { implicit request =>
    if (isAjax) immediate(Ok(views.html.repository.itemDetails(request.item, request.annotations, request.links, request.watched)))
    else findType[DocumentaryUnit](
      filters = filters(request.item.id),
      facetBuilder = localDocFacets,
      defaultOrder = SearchOrder.Id
    ).map { result =>
      Ok(views.html.repository.show(request.item, result, request.annotations,
        request.links, portalRepoRoutes.search(id), request.watched))
    }
  }

  def search(id: String) = GetItemAction(id).async { implicit request =>
    findType[DocumentaryUnit](
      filters = filters(request.item.id),
      facetBuilder = localDocFacets,
      defaultOrder = SearchOrder.Id
    ).map { result =>
      if (isAjax) Ok(views.html.repository.childItemSearch(request.item, result,
        portalRepoRoutes.search(id), request.watched))
      else Ok(views.html.repository.search(request.item, result,
        portalRepoRoutes.search(id), request.watched))
    }
  }
}
