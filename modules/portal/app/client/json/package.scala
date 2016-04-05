package client

import models._
import play.api.libs.json._
import models.base.{AnyModel, Accessor}
import play.api.libs.functional.syntax._
import models.json._
import defines.EntityType
import play.api.libs.json.JsObject
import play.api.Logger
import backend.Entity
import play.api.mvc.RequestHeader

package object json {

  implicit object datePeriodJson extends ClientWriteable[DatePeriodF] {
    def clientWrites(implicit request: RequestHeader): Writes[DatePeriodF] = Json.writes[DatePeriodF]
  }

  implicit object addressJson extends ClientWriteable[AddressF] {
    def clientWrites(implicit request: RequestHeader): Writes[AddressF] = Json.writes[AddressF]
  }

  implicit object anyModelJson extends ClientWriteable[AnyModel] {
    private def clientWritesRegistry(implicit requestHeader: RequestHeader): PartialFunction[EntityType.Value, Writes[AnyModel]] = {
      case EntityType.Repository => repositoryJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Country => countryJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.DocumentaryUnit => documentaryUnitJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Vocabulary => vocabularyJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Concept => conceptJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.HistoricalAgent => historicalAgentJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.AuthoritativeSet => authoritativeSetJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.SystemEvent => systemEventJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Group => groupJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.UserProfile => userProfileJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Link => linkJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.Annotation => annotationJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.PermissionGrant => permissionGrantJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.ContentType => contentTypeJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.AccessPoint => accessPointJson.clientWrites.asInstanceOf[Writes[AnyModel]]
      case EntityType.VirtualUnit => virtualUnitJson.clientWrites.asInstanceOf[Writes[AnyModel]]
    }

    implicit def clientWrites(implicit request: RequestHeader): Writes[AnyModel] = new Writes[AnyModel] {
      def writes(a: AnyModel): JsValue = {
        clientWritesRegistry.lift(a.isA).map(f => Json.toJson(a)(f)) getOrElse {
          // FIXME: Throw an error here???
          Logger.logger.warn("Unregistered AnyModel type {} (Writing to Client)", a.isA)
          Json.toJson(Entity(id = a.id, `type` = a.isA, relationships = Map.empty))(Entity.entityFormat)
        }
      }
    }
  }

  implicit object contentTypeJson extends ClientWriteable[ContentType] {
    def clientWrites(implicit request: RequestHeader): Writes[ContentType] = Json.writes[ContentType]
  }

  implicit object permissionGrantJson extends ClientWriteable[PermissionGrant] {
    implicit def clientWrites(implicit request: RequestHeader): Writes[PermissionGrant] = (
      JsPath.write(Json.writes[PermissionGrantF]) and
      (__ \ "accessor").lazyWriteNullable[Accessor](accessorJson.clientWrites) and
      (__ \ "targets").writeSeqOrEmpty(anyModelJson.clientWrites) and
      (__ \ "scope").lazyWriteNullable[AnyModel](anyModelJson.clientWrites) and
      (__ \ "grantedBy").lazyWriteNullable[UserProfile](userProfileJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(PermissionGrant.unapply))
  }

  implicit object accessPointJson extends ClientWriteable[AccessPoint] {
    def clientWrites(implicit request: RequestHeader): Writes[AccessPoint] =
      Json.writes[AccessPoint]
  }

  implicit object linkJson extends ClientWriteable[Link] {
    def clientWrites(implicit request: RequestHeader): Writes[Link] = (
      JsPath.write[LinkF](Json.writes[LinkF]) and
      (__ \ "targets").writeSeqOrEmpty(anyModelJson.clientWrites) and
      (__ \ "user").lazyWriteNullable[UserProfile](userProfileJson.clientWrites) and
      (__ \ "accessPoints").writeSeqOrEmpty(accessPointJson.clientWrites) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "promotedBy").writeSeqOrEmpty(userProfileJson.clientWrites) and
      (__ \ "demotedBy").writeSeqOrEmpty(userProfileJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(Link.unapply))
  }

  implicit object countryJson extends ClientWriteable[Country] {
    def clientWrites(implicit request: RequestHeader): Writes[Country] = (
      JsPath.write[CountryF](Json.writes[CountryF]) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(Country.unapply))
  }

  implicit object versionJson extends ClientWriteable[Version] {
    def clientWrites(implicit request: RequestHeader): Writes[Version] = (
      JsPath.write[VersionF](Json.writes[VersionF]) and
      (__ \ "event").lazyWriteNullable(systemEventJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(Version.unapply))
  }

  implicit object accessorJson extends ClientWriteable[Accessor] {
    implicit def clientWrites(implicit request: RequestHeader): Writes[Accessor] = new Writes[Accessor] {
      def writes(a: Accessor): JsValue = {
        Json.toJson(a)(anyModelJson.clientWrites.asInstanceOf[Writes[Accessor]])
      }
    }
  }

  implicit object systemEventJson extends ClientWriteable[SystemEvent] {
    def clientWrites(implicit request: RequestHeader): Writes[SystemEvent] = (
      JsPath.write[SystemEventF](Json.writes[SystemEventF]) and
      (__ \ "scope").lazyWriteNullable[AnyModel](anyModelJson.clientWrites) and
      (__ \ "firstSubject").lazyWriteNullable[AnyModel](anyModelJson.clientWrites) and
      (__ \ "user").lazyWriteNullable[Accessor](accessorJson.clientWrites) and
      (__ \ "version").lazyWriteNullable(versionJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(SystemEvent.unapply))
  }

  implicit object groupJson extends ClientWriteable[Group] {
    def clientWrites(implicit request: RequestHeader): Writes[Group] = (
      JsPath.write[GroupF](Json.writes[GroupF]) and
      (__ \ "groups").lazyNullableSeqWrites(clientWrites) and
      (__ \ "accessibleTo").lazyNullableSeqWrites(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(Group.unapply))
  }

  implicit object userProfileJson extends ClientWriteable[UserProfile] {
    def clientWrites(implicit request: RequestHeader): Writes[UserProfile] = (
      JsPath.write[UserProfileF](Json.writes[UserProfileF]) and
      (__ \ "groups").writeSeqOrEmpty(groupJson.clientWrites) and
      (__ \ "accessibleTo").lazyNullableSeqWrites(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(UserProfile.quickUnapply))
  }

  implicit object annotationJson extends ClientWriteable[Annotation] {
    def clientWrites(implicit request: RequestHeader): Writes[Annotation] = (
      JsPath.write[AnnotationF](Json.writes[AnnotationF]) and
      (__ \ "annotations").lazyNullableSeqWrites(clientWrites) and
      (__ \ "user").lazyWriteNullable[UserProfile](userProfileJson.clientWrites) and
      (__ \ "source").lazyWriteNullable[AnyModel](anyModelJson.clientWrites) and
      (__ \ "target").lazyWriteNullable[AnyModel](anyModelJson.clientWrites) and
      (__ \ "targetPart").lazyWriteNullable[Entity](Entity.entityFormat) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "promotedBy").writeSeqOrEmpty(userProfileJson.clientWrites) and
      (__ \ "demotedBy").writeSeqOrEmpty(userProfileJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(Annotation.unapply))
  }

  implicit object documentaryUnitDescriptionJson extends ClientWriteable[DocumentaryUnitDescriptionF] {
    def clientWrites(implicit request: RequestHeader): Writes[DocumentaryUnitDescriptionF] = {
      implicit val accessPointFormat = accessPointJson.clientWrites
      implicit val datePeriodFormat = datePeriodJson.clientWrites
      implicit val isadGIdentityFormat = Json.writes[IsadGIdentity]
      implicit val isadGContextFormat = Json.writes[IsadGContext]
      implicit val isadGContentFormat = Json.writes[IsadGContent]
      implicit val isadGConditionsFormat = Json.writes[IsadGConditions]
      implicit val isadGMaterialsFormat = Json.writes[IsadGMaterials]
      implicit val isadGControlFormat = Json.writes[IsadGControl]
      Json.writes[DocumentaryUnitDescriptionF]
    }
  }

  implicit object historicalAgentDescriptionJson extends ClientWriteable[HistoricalAgentDescriptionF] {
    def clientWrites(implicit request: RequestHeader): Writes[HistoricalAgentDescriptionF] = {
      implicit val accessPointFormat = accessPointJson.clientWrites
      implicit val datePeriodFormat = datePeriodJson.clientWrites
      implicit val isaarDetailsFormat = Json.writes[IsaarDetail]
      implicit val isaarControlFormat = Json.writes[IsaarControl]
      Json.writes[HistoricalAgentDescriptionF]
    }
  }

  implicit object repositoryDescriptionJson extends ClientWriteable[RepositoryDescriptionF] {
    def clientWrites(implicit request: RequestHeader): Writes[RepositoryDescriptionF] = {
      implicit val addressFormat = addressJson.clientWrites
      implicit val accessPointFormat = accessPointJson.clientWrites
      implicit val isdiahDetailsFormat = Json.writes[IsdiahDetails]
      implicit val isdiahAccessFormat = Json.writes[IsdiahAccess]
      implicit val isdiahServicesFormat = Json.writes[IsdiahServices]
      implicit val isdiahControlFormat = Json.writes[IsdiahControl]
      Json.writes[RepositoryDescriptionF]
    }
  }

  implicit object conceptDescriptionJson extends ClientWriteable[ConceptDescriptionF] {
    def clientWrites(implicit request: RequestHeader): Writes[ConceptDescriptionF] = {
      implicit val accessPointFormat = accessPointJson.clientWrites
      Json.writes[ConceptDescriptionF]
    }
  }

  implicit object historicalAgentJson extends ClientWriteable[HistoricalAgent] {
    def clientWrites(implicit request: RequestHeader): Writes[HistoricalAgent] = {
      implicit val haDescFmt = historicalAgentDescriptionJson.clientWrites
      (JsPath.write(Json.writes[HistoricalAgentF]) and
        (__ \ "set").writeNullable[AuthoritativeSet](authoritativeSetJson.clientWrites) and
        (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
        (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
        (__ \ "meta").write[JsObject]
      )(unlift(HistoricalAgent.unapply))
    }
  }

  implicit object repositoryJson extends ClientWriteable[Repository] {
    def clientWrites(implicit request: RequestHeader): Writes[Repository] = {
      implicit val repoDescFmt = repositoryDescriptionJson.clientWrites
      (JsPath.write(Json.writes[RepositoryF]) and
        (__ \ "country").writeNullable[Country](countryJson.clientWrites) and
        (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
        (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
        (__ \ "meta").format[JsObject]
      )(unlift(Repository.unapply))
    }
  }

  implicit object documentaryUnitJson extends ClientWriteable[DocumentaryUnit] {
    def clientWrites(implicit request: RequestHeader): Writes[DocumentaryUnit] = {
      implicit val docDescFmt = documentaryUnitDescriptionJson.clientWrites
      (JsPath.write(Json.writes[DocumentaryUnitF]) and
        (__ \ "holder").writeNullable[Repository](repositoryJson.clientWrites) and
        (__ \ "parent").lazyWriteNullable[DocumentaryUnit](clientWrites) and
        (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
        (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
        (__ \ "meta").format[JsObject]
      )(unlift(DocumentaryUnit.unapply))
    }
  }

  implicit object virtualUnitJson extends ClientWriteable[VirtualUnit] {
    def clientWrites(implicit request: RequestHeader): Writes[VirtualUnit] = (
    JsPath.write[VirtualUnitF](Json.writes[VirtualUnitF]) and
      (__ \ "descriptions").writeSeqOrEmpty(documentaryUnitJson.clientWrites) and
      (__ \ "author").writeNullable[Accessor](accessorJson.clientWrites) and
      (__ \ "parent").lazyWriteNullable[VirtualUnit](clientWrites) and
      (__ \ "holder").writeNullable[Repository](repositoryJson.clientWrites) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(VirtualUnit.unapply))
  }

  implicit object authoritativeSetJson extends ClientWriteable[AuthoritativeSet] {
    def clientWrites(implicit request: RequestHeader): Writes[AuthoritativeSet] = (
      JsPath.write[AuthoritativeSetF](Json.writes[AuthoritativeSetF]) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").format[JsObject]
    )(unlift(AuthoritativeSet.unapply))
  }

  implicit object vocabularyJson extends ClientWriteable[Vocabulary] {
    def clientWrites(implicit request: RequestHeader): Writes[Vocabulary] = (
      JsPath.write[VocabularyF](Json.writes[VocabularyF]) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(Vocabulary.unapply))
  }

  implicit object conceptJson extends ClientWriteable[Concept] {
    def clientWrites(implicit request: RequestHeader): Writes[Concept] = (
      JsPath.write[ConceptF](Json.writes[ConceptF]) and
      (__ \ "vocabulary").writeNullable[Vocabulary](vocabularyJson.clientWrites) and
      (__ \ "parent").lazyWriteNullable[Concept](clientWrites) and
      (__ \ "broaderTerms").lazyNullableSeqWrites(clientWrites) and
      (__ \ "accessibleTo").writeSeqOrEmpty(accessorJson.clientWrites) and
      (__ \ "event").writeNullable[SystemEvent](systemEventJson.clientWrites) and
      (__ \ "meta").write[JsObject]
    )(unlift(Concept.unapply))
  }
}
