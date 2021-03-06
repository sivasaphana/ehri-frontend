package backend.rest

import scala.concurrent.Future
import play.api.libs.json._
import defines.EntityType
import backend._


/**
 * Data Access Object for managing descriptions on entities.
 */
trait RestDescriptions extends RestDAO with RestContext with Descriptions {

  private def requestUrl = s"$baseUrl/description"

  override def createDescription[MT: Resource, DT: Writable](id: String, item: DT, logMsg: Option[String] = None): Future[DT] = {
    val url: String = enc(requestUrl, id)
    userCall(url).withHeaders(msgHeader(logMsg): _*)
        .post(Json.toJson(item)(Writable[DT].restFormat)).map { response =>
      val desc: DT = checkErrorAndParse(response, context = Some(url))(Writable[DT].restFormat)
      eventHandler.handleUpdate(id)
      cache.remove(canonicalUrl(id))
      desc
    }
  }

  override def updateDescription[MT: Resource, DT: Writable](id: String, did: String, item: DT, logMsg: Option[String] = None): Future[DT] = {
    val url: String = enc(requestUrl, id, did)
    userCall(url).withHeaders(msgHeader(logMsg): _*)
        .put(Json.toJson(item)(Writable[DT].restFormat)).map { response =>
      val desc: DT = checkErrorAndParse(response, context = Some(url))(Writable[DT].restFormat)
      eventHandler.handleUpdate(id)
      cache.remove(canonicalUrl(id))
      desc
    }
  }

  override def deleteDescription[MT: Resource](id: String, did: String, logMsg: Option[String] = None): Future[Unit] = {
    userCall(enc(requestUrl, id, did)).withHeaders(msgHeader(logMsg): _*).delete().map { response =>
      checkError(response)
      eventHandler.handleDelete(did)
      cache.remove(canonicalUrl(id))
    }
  }

  override def createAccessPoint[MT: Resource, AP: Writable](id: String, did: String, item: AP, logMsg: Option[String] = None): Future[AP] = {
    val url: String = enc(requestUrl, id, did, EntityType.AccessPoint)
    userCall(url)
        .withHeaders(msgHeader(logMsg): _*)
        .post(Json.toJson(item)(Writable[AP].restFormat)).map { response =>
      eventHandler.handleUpdate(id)
      cache.remove(canonicalUrl(id))
      checkErrorAndParse(response, context = Some(url))(Writable[AP].restFormat)
    }
  }

  override def deleteAccessPoint[MT: Resource](id: String, did: String, apid: String, logMsg: Option[String] = None): Future[Unit] = {
    val url = enc(requestUrl, id, did, EntityType.AccessPoint, apid)
    userCall(url).withHeaders(msgHeader(logMsg): _*).delete().map { response =>
      checkError(response)
      eventHandler.handleUpdate(id)
      cache.remove(canonicalUrl(id))
    }
  }
}