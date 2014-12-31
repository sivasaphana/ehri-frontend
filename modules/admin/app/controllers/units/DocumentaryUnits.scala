package controllers.units

import play.api.libs.concurrent.Execution.Implicits._
import forms.VisibilityForm
import models._
import controllers.generic._
import play.api.i18n.Messages
import defines.{ContentTypes,EntityType,PermissionType}
import views.Helpers
import utils.search._
import com.google.inject._
import solr.SolrConstants
import scala.concurrent.Future.{successful => immediate}
import backend.{ApiUser, Backend}
import play.api.Play.current
import play.api.Configuration
import play.api.http.MimeTypes
import utils.ead.EadExporter
import models.base.Description
import controllers.base.AdminController


@Singleton
case class DocumentaryUnits @Inject()(implicit globalConfig: global.GlobalConfig, searchDispatcher: Dispatcher, searchResolver: Resolver, backend: Backend, userDAO: AccountDAO)
  extends AdminController
  with Read[DocumentaryUnit]
  with Visibility[DocumentaryUnit]
  with Creator[DocumentaryUnitF, DocumentaryUnit, DocumentaryUnit]
  with Update[DocumentaryUnitF, DocumentaryUnit]
  with Delete[DocumentaryUnit]
  with ScopePermissions[DocumentaryUnit]
  with Annotate[DocumentaryUnit]
  with Linking[DocumentaryUnit]
  with Descriptions[DocumentaryUnitDescriptionF, DocumentaryUnitF, DocumentaryUnit]
  with AccessPoints[DocumentaryUnitDescriptionF, DocumentaryUnitF, DocumentaryUnit]
  with Search {

  // Documentary unit facets
  import solr.facet._

  private val entityFacets: FacetBuilder = { implicit request =>
    List(
      FieldFacetClass(
        key="isParent",
        name=Messages("facet.parent"),
        param="parent",
        render=s => Messages("facet.parent." + s),
        sort = FacetSort.Fixed,
        display = FacetDisplay.List
      ),
      FieldFacetClass(
        key=Description.LANG_CODE,
        name=Messages("documentaryUnit." + Description.LANG_CODE),
        param="lang",
        render=Helpers.languageCodeToName,
        display = FacetDisplay.DropDown
      ),
      FieldFacetClass(
        key="creationProcess",
        name=Messages("facet.source"),
        param="source",
        render=s => Messages("facet.source." + s),
        sort = FacetSort.Name,
        display = FacetDisplay.List
      ),
      FieldFacetClass(
        key="holderName",
        name=Messages("documentaryUnit.heldBy"),
        param="holder",
        sort = FacetSort.Name,
        display = FacetDisplay.DropDown
      ),
      FieldFacetClass(
        key="countryCode",
        name=Messages("repository.countryCode"),
        param="country",
        render= (s: String) => Helpers.countryCodeToName(s),
        sort = FacetSort.Name,
        display = FacetDisplay.DropDown
      ),
      FieldFacetClass(
        key="copyrightStatus",
        name=Messages("copyrightStatus.copyright"),
        param="copyright",
        render=s => Messages("copyrightStatus." + s)
      ),
      QueryFacetClass(
        key="charCount",
        name=Messages("lod"),
        param="lod",
        render=s => Messages("lod." + s),
        facets=List(
          SolrQueryFacet(value = "low", solrValue = "[0 TO 500]", name = Some("low")),
          SolrQueryFacet(value = "medium", solrValue = "[501 TO 2000]", name = Some("medium")),
          SolrQueryFacet(value = "high", solrValue = "[2001 TO *]", name = Some("high"))
        ),
        sort = FacetSort.Fixed,
        display = FacetDisplay.List
      ),
      FieldFacetClass(
        key="scope",
        name=Messages("scope.scope"),
        param="scope",
        render=s => Messages("scope." + s)
      )
    )
  }

  val formDefaults: Option[Configuration] = current.configuration.getConfig(EntityType.DocumentaryUnit)

  val targetContentTypes = Seq(ContentTypes.DocumentaryUnit)

  val form = models.DocumentaryUnit.form
  val childForm = models.DocumentaryUnit.form
  val descriptionForm = models.DocumentaryUnitDescription.form

  private val docRoutes = controllers.units.routes.DocumentaryUnits


  def search = OptionalProfileAction.async { implicit request =>
    // What filters we gonna use? How about, only list stuff here that
    // has no parent items - UNLESS there's a query, in which case we're
    // going to peer INSIDE items... dodgy logic, maybe...
    
    val filters = if (request.getQueryString(SearchParams.QUERY).filterNot(_.trim.isEmpty).isEmpty)
      Map(SolrConstants.TOP_LEVEL -> true) else Map.empty[String,Any]

    find[DocumentaryUnit](
      filters = filters,
      entities=List(EntityType.DocumentaryUnit),
      facetBuilder = entityFacets
    ).map { result =>
      Ok(views.html.admin.documentaryUnit.search(
        result.page, result.params, result.facets,
        docRoutes.search()))
    }
  }

  def searchChildren(id: String) = ItemPermissionAction(id).async { implicit request =>
    find[DocumentaryUnit](
      filters = Map(SolrConstants.PARENT_ID -> request.item.id),
      facetBuilder = entityFacets,
      defaultOrder = SearchOrder.Id
    ).map { result =>
      Ok(views.html.admin.documentaryUnit.search(
        result.page, result.params, result.facets,
        docRoutes.search()))
    }
  }

  def get(id: String) = ItemMetaAction(id).async { implicit request =>
    find[DocumentaryUnit](
      filters = Map(SolrConstants.PARENT_ID -> request.item.id),
      entities = List(EntityType.DocumentaryUnit),
      facetBuilder = entityFacets,
      defaultOrder = SearchOrder.Id
    ).map { result =>
      Ok(views.html.admin.documentaryUnit.show(request.item, result.page, result.params, result.facets,
          docRoutes.get(id), request.annotations, request.links))
    }
  }

  def history(id: String) = historyAction(id) { item => page => params => implicit userOpt => implicit request =>
    Ok(views.html.admin.systemEvents.itemList(item, page, params))
  }

  def list = ItemPageAction.apply { implicit request =>
    Ok(views.html.admin.documentaryUnit.list(request.page, request.params))
  }

  def update(id: String) = EditAction(id).apply { implicit request =>
    Ok(views.html.admin.documentaryUnit.edit(
      request.item, form.fill(request.item.model), docRoutes.updatePost(id)))
  }

  def updatePost(id: String) = UpdateAction(id, form).apply { implicit request =>
    request.formOrItem match {
      case Left(errorForm) => BadRequest(views.html.admin.documentaryUnit.edit(
          request.item, errorForm, docRoutes.updatePost(id)))
      case Right(item) => Redirect(docRoutes.get(item.id))
        .flashing("success" -> "item.update.confirmation")
    }
  }

  def createDoc(id: String) = NewChildAction(id).apply { implicit request =>
    Ok(views.html.admin.documentaryUnit.create(
      request.item, childForm, formDefaults, VisibilityForm.form.fill(request.item.accessors.map(_.id)),
      request.users, request.groups, docRoutes.createDocPost(id)))
  }

  def createDocPost(id: String) = CreateChildAction(id, childForm).async { implicit request =>
    request.formOrItem match {
      case Left((errorForm,accForm)) => getUsersAndGroups { users => groups =>
        BadRequest(views.html.admin.documentaryUnit.create(request.item,
          errorForm, formDefaults, accForm, users, groups,
          docRoutes.createDocPost(id)))
      }
      case Right(doc) => immediate(Redirect(docRoutes.get(doc.id))
        .flashing("success" -> "item.create.confirmation"))
    }
  }

  def createDescription(id: String) = WithItemPermissionAction(id, PermissionType.Update).apply { implicit request =>
    Ok(views.html.admin.documentaryUnit.createDescription(request.item,
        descriptionForm, formDefaults, docRoutes.createDescriptionPost(id)))
  }

  def createDescriptionPost(id: String) = createDescriptionPostAction(id, EntityType.DocumentaryUnitDescription, descriptionForm) {
      item => formOrItem => implicit userOpt => implicit request =>
    formOrItem match {
      case Left(errorForm) =>
        Ok(views.html.admin.documentaryUnit.createDescription(item,
          errorForm, formDefaults, docRoutes.createDescriptionPost(id)))
      case Right(_) => Redirect(docRoutes.get(item.id))
        .flashing("success" -> "item.create.confirmation")
    }
  }

  def updateDescription(id: String, did: String) = WithItemPermissionAction(id, PermissionType.Update).apply { implicit request =>
    itemOr404(request.item.model.description(did)) { desc =>
      Ok(views.html.admin.documentaryUnit.editDescription(request.item,
        descriptionForm.fill(desc),
        docRoutes.updateDescriptionPost(id, did)))
    }
  }

  def updateDescriptionPost(id: String, did: String) = updateDescriptionPostAction(id, EntityType.DocumentaryUnitDescription, did, descriptionForm) {
      item => formOrItem => implicit userOpt => implicit request =>
    formOrItem match {
      case Left(errorForm) =>
        Ok(views.html.admin.documentaryUnit.editDescription(item,
          errorForm, docRoutes.updateDescriptionPost(id, did)))
      case Right(_) => Redirect(docRoutes.get(item.id))
        .flashing("success" -> "item.update.confirmation")
    }
  }

  def deleteDescription(id: String, did: String) = deleteDescriptionAction(id, did) {
      item => description => implicit userOpt => implicit request =>
    Ok(views.html.admin.deleteDescription(item, description,
        docRoutes.deleteDescriptionPost(id, did),
        docRoutes.get(id)))
  }

  def deleteDescriptionPost(id: String, did: String) = deleteDescriptionPostAction(id, EntityType.DocumentaryUnitDescription, did) {
      implicit userOpt => implicit request =>
    Redirect(docRoutes.get(id))
        .flashing("success" -> "item.delete.confirmation")
  }

  def delete(id: String) = CheckDeleteAction(id).apply { implicit request =>
    Ok(views.html.admin.delete(
        request.item, docRoutes.deletePost(id), docRoutes.get(id)))
  }

  def deletePost(id: String) = DeleteAction(id).apply { implicit request =>
    Redirect(docRoutes.search())
        .flashing("success" -> "item.delete.confirmation")
  }

  def visibility(id: String) = EditVisibilityAction(id).apply { implicit request =>
    Ok(views.html.admin.permissions.visibility(request.item,
        VisibilityForm.form.fill(request.item.accessors.map(_.id)),
        request.users, request.groups, docRoutes.visibilityPost(id)))
  }

  def visibilityPost(id: String) = UpdateVisibilityAction(id).apply { implicit request =>
    Redirect(docRoutes.get(id))
        .flashing("success" -> "item.update.confirmation")
  }

  def managePermissions(id: String) = manageScopedPermissionsAction(id) {
      item => perms => sperms => implicit userOpt => implicit request =>
    Ok(views.html.admin.permissions.manageScopedPermissions(item, perms, sperms,
        docRoutes.addItemPermissions(id),
        docRoutes.addScopedPermissions(id)))
  }

  def addItemPermissions(id: String) = addItemPermissionsAction(id) {
      item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.admin.permissions.permissionItem(item, users, groups,
        docRoutes.setItemPermissions))
  }

  def addScopedPermissions(id: String) = addItemPermissionsAction(id) {
      item => users => groups => implicit userOpt => implicit request =>
    Ok(views.html.admin.permissions.permissionScope(item, users, groups,
        docRoutes.setScopedPermissions))
  }

  def setItemPermissions(id: String, userType: EntityType.Value, userId: String) = setItemPermissionsAction(id, userType, userId) {
      item => accessor => perms => implicit userOpt => implicit request =>
    Ok(views.html.admin.permissions.setPermissionItem(item, accessor, perms, DocumentaryUnit.Resource.contentType,
        docRoutes.setItemPermissionsPost(id, userType, userId)))
  }

  def setItemPermissionsPost(id: String, userType: EntityType.Value, userId: String) = setItemPermissionsPostAction(id, userType, userId) {
      bool => implicit userOpt => implicit request =>
    Redirect(docRoutes.managePermissions(id))
        .flashing("success" -> "item.update.confirmation")
  }

  def setScopedPermissions(id: String, userType: EntityType.Value, userId: String) = setScopedPermissionsAction(id, userType, userId) {
      item => accessor => perms => implicit userOpt => implicit request =>
    Ok(views.html.admin.permissions.setPermissionScope(item, accessor, perms, targetContentTypes,
        docRoutes.setScopedPermissionsPost(id, userType, userId)))
  }

  def setScopedPermissionsPost(id: String, userType: EntityType.Value, userId: String) = setScopedPermissionsPostAction(id, userType, userId) {
      perms => implicit userOpt => implicit request =>
    Redirect(docRoutes.managePermissions(id))
        .flashing("success" -> "item.update.confirmation")
  }

  def linkTo(id: String) = WithItemPermissionAction(id, PermissionType.Annotate).apply { implicit request =>
    Ok(views.html.admin.documentaryUnit.linkTo(request.item))
  }

  def linkAnnotateSelect(id: String, toType: EntityType.Value) = linkSelectAction(id, toType) {
    item => page => params => facets => etype => implicit userOpt => implicit request =>
      Ok(views.html.admin.link.linkSourceList(item, page, params, facets, etype,
          docRoutes.linkAnnotateSelect(id, toType),
          docRoutes.linkAnnotate))
  }

  def linkAnnotate(id: String, toType: EntityType.Value, to: String) = linkAction(id, toType, to) {
      target => source => implicit userOpt => implicit request =>
    Ok(views.html.admin.link.create(target, source,
        Link.form, docRoutes.linkAnnotatePost(id, toType, to)))
  }

  def linkAnnotatePost(id: String, toType: EntityType.Value, to: String) = linkPostAction(id, toType, to) {
      formOrAnnotation => implicit userOpt => implicit request =>
    formOrAnnotation match {
      case Left((target,source,errorForm)) =>
        BadRequest(views.html.admin.link.create(target, source,
          errorForm, docRoutes.linkAnnotatePost(id, toType, to)))
      case Right(annotation) =>
        Redirect(docRoutes.get(id))
          .flashing("success" -> "item.update.confirmation")
    }
  }

  def linkMultiAnnotate(id: String) = linkMultiAction(id) {
      target => implicit userOpt => implicit request =>
    Ok(views.html.admin.link.linkMulti(target,
        Link.multiForm, docRoutes.linkMultiAnnotatePost(id)))
  }

  def linkMultiAnnotatePost(id: String) = linkPostMultiAction(id) {
      formOrAnnotations => implicit userOpt => implicit request =>
    formOrAnnotations match {
      case Left((target,errorForms)) =>
        BadRequest(views.html.admin.link.linkMulti(target,
          errorForms, docRoutes.linkMultiAnnotatePost(id)))
      case Right(annotations) =>
        Redirect(docRoutes.get(id))
          .flashing("success" -> "item.update.confirmation")
    }
  }

  def manageAccessPoints(id: String, descriptionId: String) = manageAccessPointsAction(id, descriptionId) {
      item => desc => implicit userOpt => implicit request =>
    Ok(views.html.admin.documentaryUnit.editAccessPoints(item, desc))
  }

  import play.api.libs.concurrent.Execution.Implicits._

  def exportEad(id: String) = OptionalAuthAction.async { implicit authRequest =>
    implicit val apiUser = ApiUser(authRequest.user.map(_.id))
    val eadId: String = docRoutes.exportEad(id).absoluteURL(globalConfig.https)

    EadExporter(backend).exportEad(id, eadId).map { xml =>
      Ok(xml).as(MimeTypes.XML)
      //.withHeaders(HeaderNames.CONTENT_DISPOSITION -> s"attachment; filename=$id-ead.xml")
    }
  }
}


