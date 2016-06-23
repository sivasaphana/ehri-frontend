package controllers.portal.widgets

import javax.inject.{Singleton, Inject}

import auth.AccountManager
import backend.{AnonymousUser, ApiUser, DataApi}
import controllers.portal.base.PortalController
import defines.EntityType
import models.{UserProfile, Repository, DocumentaryUnit}
import play.api.cache.CacheApi
import play.api.i18n.MessagesApi
import play.api.mvc.Action
import play.twirl.api.Html
import views.{NoopHighlighter, MarkdownRenderer}

import scala.concurrent.ExecutionContext

@Singleton
case class Widgets @Inject()(
  implicit config: play.api.Configuration,
  cache: CacheApi,
  globalConfig: global.GlobalConfig,
  dataApi: DataApi,
  accounts: AccountManager,
  messagesApi: MessagesApi,
  pageRelocator: utils.MovedPageLookup,
  markdown: MarkdownRenderer,
  executionContext: ExecutionContext
) extends PortalController {

  private implicit val apiUser: ApiUser = AnonymousUser
  private implicit val userOpt: Option[UserProfile] = None

  def embedDocumentaryUnit(id: String) = Action.async { implicit request =>
    userDataApi.get[DocumentaryUnit](DocumentaryUnit.DocumentaryUnitResource, id).map { doc =>
      val html: Html = views.html.documentaryUnit
        .searchItem(doc, NoopHighlighter, watched = false, showRepository = true)

      Ok(views.html.layout.embedLayout(doc.toStringLang, EntityType.DocumentaryUnit.toString,
          Some(controllers.portal.routes.DocumentaryUnits.browse(id)))(html))
    }
  }

  def embedRepository(id: String) = Action.async { implicit request =>
    userDataApi.get[Repository](Repository.RepositoryResource, id).map { repo =>
      ???
    }
  }
}
