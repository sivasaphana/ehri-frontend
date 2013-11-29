package controllers.core

import play.api.libs.json.Json
import utils.search.{Resolver, Dispatcher}
import com.google.inject._
import controllers.generic.Search
import backend.Backend

@Singleton
case class SearchFilter @Inject()(implicit globalConfig: global.GlobalConfig, searchDispatcher: Dispatcher, searchResolver: Resolver, backend: Backend) extends Search {

  val searchEntities = List() // i.e. Everything

  /**
   * Quick filter action that searches applies a 'q' string filter to
   * only the name_ngram field and returns an id/name pair.
   * @return
   */
  def filter = filterAction() { page => implicit userOpt => implicit request =>
    Ok(Json.obj(
      "numPages" -> page.numPages,
      "page" -> page.page,
      "items" -> page.items.map { case (id, name, t) =>
        Json.arr(id, name, t.toString)
      }
    ))
  }
}