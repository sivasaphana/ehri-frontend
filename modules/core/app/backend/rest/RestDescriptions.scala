package backend.rest

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json._
import defines.EntityType
import play.api.Play.current
import play.api.cache.Cache
import models.base.AnyModel
import backend.{BackendWriteable, BackendResource, EventHandler, ApiUser}


/**
 * Data Access Object for managing descriptions on entities.
 */
trait RestDescriptions extends RestDAO {

  val eventHandler: EventHandler
  private val entities = new EntityDAO(eventHandler)

  private def requestUrl = "http://%s:%d/%s/description".format(host, port, mount)

  def createDescription[MT,DT](id: String, item: DT, logMsg: Option[String] = None)(
        implicit apiUser: ApiUser, rs: BackendResource[MT], fmt: BackendWriteable[DT], rd: backend.BackendReadable[MT], executionContext: ExecutionContext): Future[MT] = {
    userCall(enc(requestUrl, id)).withHeaders(msgHeader(logMsg): _*)
        .post(Json.toJson(item)(fmt.restFormat)).flatMap { response =>
      checkError(response)
      entities.get(id).map { item =>
        eventHandler.handleUpdate(id)
        Cache.remove(id)
        item
      }
    }
  }

  def updateDescription[MT,DT](id: String, did: String, item: DT, logMsg: Option[String] = None)(
      implicit apiUser: ApiUser, rs: BackendResource[MT], fmt: BackendWriteable[DT], rd: backend.BackendReadable[MT], executionContext: ExecutionContext): Future[MT] = {
    userCall(enc(requestUrl, id, did)).withHeaders(msgHeader(logMsg): _*)
        .put(Json.toJson(item)(fmt.restFormat)).flatMap { response =>
      checkError(response)
      entities.get(id).map { item =>
        eventHandler.handleUpdate(id)
        Cache.remove(id)
        item
      }
    }
  }

  def deleteDescription[MT](id: String, did: String, logMsg: Option[String] = None)(
      implicit apiUser: ApiUser, rs: BackendResource[MT], rd: backend.BackendReadable[MT], executionContext: ExecutionContext): Future[Boolean] = {
    userCall(enc(requestUrl, id, did)).withHeaders(msgHeader(logMsg): _*)
          .delete().map { response =>
      checkError(response)
      eventHandler.handleDelete(did)
      Cache.remove(id)
      true
    }
  }

  def createAccessPoint[DT](id: String, did: String, item: DT, logMsg: Option[String] = None)(
        implicit apiUser: ApiUser, fmt: BackendWriteable[DT], executionContext: ExecutionContext): Future[DT] = {
    userCall(enc(requestUrl, id, did, EntityType.AccessPoint))
        .withHeaders(msgHeader(logMsg): _*)
        .post(Json.toJson(item)(fmt.restFormat)).map { response =>
      eventHandler.handleUpdate(id)
      Cache.remove(id)
      checkErrorAndParse[DT](response)(fmt.restFormat)
    }
  }

  def deleteAccessPoint(id: String, did: String, apid: String, logMsg: Option[String] = None)(implicit apiUser: ApiUser, executionContext: ExecutionContext): Future[Boolean] = {
    val url = enc(requestUrl, id, did, EntityType.AccessPoint, apid)
    userCall(url).withHeaders(msgHeader(logMsg): _*).delete().map { response =>
      checkError(response)
      eventHandler.handleDelete(id)
      true
    }
  }
}


case class DescriptionDAO(eventHandler: EventHandler) extends RestDescriptions