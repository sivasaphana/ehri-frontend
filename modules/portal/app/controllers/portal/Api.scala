package controllers.portal

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{Reads, Writes, Json}
import backend.{ContentType, Readable, Resource}
import controllers.generic.{SearchType, Search, Generic}
import client.json.ClientWriteable
import utils.Page

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */
trait Api[MT] extends SearchType[MT] {

  def apiSearchAllJson[B: Writes](f: MT => B)(implicit ct: ContentType[MT], cw: ClientWriteable[MT]) = OptionalUserAction.async {implicit request =>
    findType[MT]().map { r =>
      Ok(Json.toJson(r.mapItems(t => f(t._1)).page)(Page.pageWrites))
    }
  }

  def apiGetJson(id: String)(implicit ct: ContentType[MT], cw: ClientWriteable[MT]) = OptionalUserAction.async { implicit request =>
    userBackend.get[MT](id).map { tm =>
      Ok(Json.toJson(tm)(cw.clientFormat))
    }
  }
}
