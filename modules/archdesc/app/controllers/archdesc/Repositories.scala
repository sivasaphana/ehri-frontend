package controllers.archdesc

import forms.VisibilityForm
import controllers.base.ApiBase
import controllers.generic._
import models.{Repository,RepositoryF,DocumentaryUnit,DocumentaryUnitF}
import play.api.i18n.Messages
import defines.{EntityType,PermissionType,ContentTypes}
import views.Helpers
import utils.search.{Dispatcher, SearchParams, FacetSort}
import com.google.inject._
import solr.SolrConstants
import scala.concurrent.Future.{successful => immediate}

@Singleton
case class Repositories @Inject()(implicit globalConfig: global.GlobalConfig, searchDispatcher: Dispatcher, backend: rest.Backend) extends Read[Repository]
  with Update[RepositoryF, Repository]
  with Delete[Repository]
  with Creator[DocumentaryUnitF,DocumentaryUnit, Repository]
	with Visibility[Repository]
  with ScopePermissions[Repository]
  with Annotate[Repository]
  with Search
  with ApiBase[Repository] {

  val DEFAULT_SORT = "name"

  // Documentary unit facets
  import solr.facet._

  private val repositoryFacets: FacetBuilder = { implicit lang =>
    List(
      QueryFacetClass(
        key="childCount",
        name=Messages("repository.itemsHeldOnline"),
        param="items",
        render=s => Messages("repository." + s),
        facets=List(
          SolrQueryFacet(value = "false", solrValue = "0", name = Some("noChildItems")),
          SolrQueryFacet(value = "true", solrValue = "[1 TO *]", name = Some("hasChildItems"))
        )
      ),
      FieldFacetClass(
        key="countryCode",
        name=Messages("isdiah.countryCode"),
        param="country",
        render=Helpers.countryCodeToName,
        sort = FacetSort.Name
      ),
      FieldFacetClass(
        key="priority",
        name=Messages("priority"),
        param="priority",
        render=s => s match {
          case s if s == "0" => Messages("priority.zero")
          case s if s == "1" => Messages("priority.one")
          case s if s == "2" => Messages("priority.two")
          case s if s == "3" => Messages("priority.three")
          case s if s == "4" => Messages("priority.four")
          case s if s == "5" => Messages("priority.five")
          case s if s == "-1" => Messages("priority.reject")
          case _ => Messages("priority.unknown")
        }
      )
    )
  }

  implicit val resource = Repository.Resource

  val contentType = ContentTypes.Repository
  val targetContentTypes = Seq(ContentTypes.DocumentaryUnit)

  private val form = models.forms.RepositoryForm.form
  private val childForm = models.forms.DocumentaryUnitForm.form

  private val DEFAULT_SEARCH_PARAMS = SearchParams(entities = List(resource.entityType))

  private val repositoryRoutes = controllers.archdesc.routes.Repositories


  def search = searchAction[Repository](defaultParams = Some(DEFAULT_SEARCH_PARAMS), entityFacets = repositoryFacets) {
      page => params => facets => implicit userOpt => implicit request =>
    Ok(views.html.repository.search(page, params, facets, repositoryRoutes.search))
  }

  /**
   * Search documents inside repository.
   */
  def get(id: String) = getAction.async(id) { item => annotations => links => implicit userOpt => implicit request =>

    val filters = (if (request.getQueryString(SearchParams.QUERY).filterNot(_.trim.isEmpty).isEmpty)
      Map(SolrConstants.TOP_LEVEL -> true) else Map.empty[String,Any]) ++ Map(SolrConstants.HOLDER_ID -> item.id)

    searchAction[DocumentaryUnit](filters,
        defaultParams = Some(SearchParams(entities = List(EntityType.DocumentaryUnit))),
        entityFacets = repositoryFacets) {
      page => params => facets => _ => _ =>
        Ok(views.html.repository.show(item, page, params, facets, repositoryRoutes.get(id), annotations, links))
    }.apply(request)
  }

  def history(id: String) = historyAction(id) { item => page => params => implicit userOpt => implicit request =>
    Ok(views.html.systemEvents.itemList(item, page, params))
  }

  def list = pageAction { page => params => implicit userOpt => implicit request =>
    Ok(views.html.repository.list(page, params))
  }

  def update(id: String) = updateAction(id) {
      item => implicit userOpt => implicit request =>
    Ok(views.html.repository.edit(item, form.fill(item.model), repositoryRoutes.updatePost(id)))
  }

  def updatePost(id: String) = updatePostAction(id, form) {
      item => formOrItem => implicit userOpt => implicit request =>
    formOrItem match {
      case Left(errorForm) =>
        BadRequest(views.html.repository.edit(item, errorForm, repositoryRoutes.updatePost(id)))
      case Right(item) => Redirect(repositoryRoutes.get(item.id))
        .flashing("success" -> Messages("confirmations.itemWasUpdated", item.id))
    }
  }

  def createDoc(id: String) = childCreateAction(id, ContentTypes.DocumentaryUnit) {
      item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.documentaryUnit.create(item, childForm,
        VisibilityForm.form, users, groups, repositoryRoutes.createDocPost(id)))
  }

  def createDocPost(id: String) = childCreatePostAction.async(id, childForm, ContentTypes.DocumentaryUnit) {
      item => formsOrItem => implicit userOpt => implicit request =>
    formsOrItem match {
      case Left((errorForm,accForm)) => getUsersAndGroups { users => groups =>
        BadRequest(views.html.documentaryUnit.create(item,
          errorForm, accForm, users, groups, repositoryRoutes.createDocPost(id)))
      }
      case Right(citem) => immediate(Redirect(controllers.archdesc.routes.DocumentaryUnits.get(citem.id))
        .flashing("success" -> Messages("confirmations.itemWasCreated", citem.id)))
    }
  }

  def delete(id: String) = deleteAction(id) {
      item => implicit userOpt => implicit request =>
    Ok(views.html.delete(item, repositoryRoutes.deletePost(id),
        repositoryRoutes.get(id)))
  }

  def deletePost(id: String) = deletePostAction(id) { ok => implicit userOpt => implicit request =>
    Redirect(repositoryRoutes.search())
        .flashing("success" -> Messages("confirmations.itemWasDeleted", id))
  }

  def visibility(id: String) = visibilityAction(id) { item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.permissions.visibility(item,
      VisibilityForm.form.fill(item.accessors.map(_.id)),
      users, groups, repositoryRoutes.visibilityPost(id)))
  }

  def visibilityPost(id: String) = visibilityPostAction(id) {
      ok => implicit userOpt => implicit request =>
    Redirect(repositoryRoutes.get(id))
        .flashing("success" -> Messages("confirmations.itemWasUpdated", id))
  }

  def managePermissions(id: String) = manageScopedPermissionsAction(id) {
      item => perms => sperms => implicit userOpt => implicit request =>
    Ok(views.html.permissions.manageScopedPermissions(item, perms, sperms,
        repositoryRoutes.addItemPermissions(id), repositoryRoutes.addScopedPermissions(id)))
  }

  def addItemPermissions(id: String) = addItemPermissionsAction(id) {
      item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.permissions.permissionItem(item, users, groups,
        repositoryRoutes.setItemPermissions _))
  }

  def addScopedPermissions(id: String) = addItemPermissionsAction(id) {
      item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.permissions.permissionScope(item, users, groups,
        repositoryRoutes.setScopedPermissions _))
  }

  def setItemPermissions(id: String, userType: String, userId: String) = setItemPermissionsAction(id, userType, userId) {
      item => accessor => perms => implicit userOpt => implicit request =>
    Ok(views.html.permissions.setPermissionItem(item, accessor, perms, contentType,
        repositoryRoutes.setItemPermissionsPost(id, userType, userId)))
  }

  def setItemPermissionsPost(id: String, userType: String, userId: String) = setItemPermissionsPostAction(id, userType, userId) {
      bool => implicit userOpt => implicit request =>
    Redirect(repositoryRoutes.managePermissions(id))
        .flashing("success" -> Messages("confirmations.itemWasUpdated", id))
  }

  def setScopedPermissions(id: String, userType: String, userId: String) = setScopedPermissionsAction(id, userType, userId) {
      item => accessor => perms => implicit userOpt => implicit request =>
    Ok(views.html.permissions.setPermissionScope(item, accessor, perms, targetContentTypes,
        repositoryRoutes.setScopedPermissionsPost(id, userType, userId)))
  }

  def setScopedPermissionsPost(id: String, userType: String, userId: String) = setScopedPermissionsPostAction(id, userType, userId) {
      perms => implicit userOpt => implicit request =>
    Redirect(repositoryRoutes.managePermissions(id))
        .flashing("success" -> Messages("confirmations.itemWasUpdated", id))
  }
}