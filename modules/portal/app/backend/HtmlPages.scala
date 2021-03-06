package backend

import play.api.i18n.Messages
import play.twirl.api.Html

import scala.concurrent.Future

/**
 * Interface for static HTML pages that are pulled
 * from some external source and embedded in our
 * page template.
 *
 * @author Mike Bryant (http://github.com/mikesname)
 */
trait HtmlPages {
  /**
   * Fetch an external page fragment with a given key.
   *
   * @param key  the identifier for this fragment
   * @param noCache whether to prevent the fragment coming from the cache
   * @param lang the language of the document version
   * @return An optional, future of Css -> Body HTML
   */
  def get(key: String, noCache: Boolean = false)(implicit messages: Messages): Option[Future[(Html, Html)]]
}
