package models

import models.base._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import defines.EntityType
import models.json._
import eu.ehri.project.definitions.Ontology
import utils.forms._
import play.api.data.Form
import play.api.data.Forms._
import backend.{Entity, Readable, Writable}
import Description._

case class IsdiahDetails(
  history: Option[String] = None,
  generalContext: Option[String] = None,
  mandates: Option[String] = None,
  administrativeStructure: Option[String] = None,
  records: Option[String] = None,
  buildings: Option[String] = None,
  holdings: Option[String] = None,
  findingAids: Option[String] = None
) extends AttributeSet

case class IsdiahAccess(
  openingTimes: Option[String] = None,
  conditions: Option[String] = None,
  accessibility: Option[String] = None
) extends AttributeSet

case class IsdiahServices(
  researchServices: Option[String] = None,
  reproductionServices: Option[String] = None,
  publicAreas: Option[String] = None
) extends AttributeSet

case class IsdiahControl(
  descriptionIdentifier: Option[String] = None,
  institutionIdentifier: Option[String] = None,
  rulesAndConventions: Option[String] = None,
  status: Option[String] = None,
  levelOfDetail: Option[String] = None,
  datesCDR: Option[String] = None,
  languages: Option[Seq[String]] = None,
  scripts: Option[Seq[String]] = None,
  sources: Option[Seq[String]] = None,
  maintenanceNotes: Option[String] = None
) extends AttributeSet

object RepositoryDescriptionF {

  import Entity._
  import Isdiah._
  import eu.ehri.project.definitions.Ontology._

  implicit val repositoryDescriptionFormat: Format[RepositoryDescriptionF] = (
    (__ \ TYPE).formatIfEquals(EntityType.RepositoryDescription) and
    (__ \ ID).formatNullable[String] and
    (__ \ DATA \ LANG_CODE).format[String] and
    (__ \ DATA \ AUTHORIZED_FORM_OF_NAME).format[String] and
    (__ \ DATA \ OTHER_FORMS_OF_NAME).formatSeqOrSingleNullable[String] and
    (__ \ DATA \ PARALLEL_FORMS_OF_NAME).formatSeqOrSingleNullable[String] and
    (__ \ RELATIONSHIPS \ ENTITY_HAS_ADDRESS).formatSeqOrEmpty[AddressF] and
    (__ \ DATA).format[IsdiahDetails]((
      (__ \ HISTORY).formatNullable[String] and
      (__ \ GEOCULTURAL_CONTEXT).formatNullable[String] and
      (__ \ MANDATES).formatNullable[String] and
      (__ \ ADMINISTRATIVE_STRUCTURE).formatNullable[String] and
      (__ \ RECORDS).formatNullable[String] and
      (__ \ BUILDINGS).formatNullable[String] and
      (__ \ HOLDINGS).formatNullable[String] and
      (__ \ FINDING_AIDS).formatNullable[String]
    )(IsdiahDetails.apply, unlift(IsdiahDetails.unapply))) and
    (__ \ DATA).format[IsdiahAccess]((
      (__ \ OPENING_TIMES).formatNullable[String] and
      (__ \ CONDITIONS).formatNullable[String] and
      (__ \ ACCESSIBILITY).formatNullable[String]
    )(IsdiahAccess.apply, unlift(IsdiahAccess.unapply))) and
    (__ \ DATA).format[IsdiahServices]((
      (__ \ RESEARCH_SERVICES).formatNullable[String] and
      (__ \ REPROD_SERVICES).formatNullable[String] and
      (__ \ PUBLIC_AREAS).formatNullable[String]
    )(IsdiahServices.apply, unlift(IsdiahServices.unapply))) and
    (__ \ DATA).format[IsdiahControl]((
      (__ \ DESCRIPTION_IDENTIFIER).formatNullable[String] and
      (__ \ INSTITUTION_IDENTIFIER).formatNullable[String] and
      (__ \ RULES_CONVENTIONS).formatNullable[String] and
      (__ \ STATUS).formatNullable[String] and
      (__ \ LEVEL_OF_DETAIL).formatNullable[String] and
      (__ \ DATES_CVD).formatNullable[String] and
      (__ \ LANGUAGES_USED).formatNullable[Seq[String]] and
      (__ \ SCRIPTS_USED).formatSeqOrSingleNullable[String] and
      (__ \ SOURCES).formatSeqOrSingleNullable[String] and
      (__ \ MAINTENANCE_NOTES).formatNullable[String]
    )(IsdiahControl.apply, unlift(IsdiahControl.unapply))) and
    (__ \ DATA \ CREATION_PROCESS).formatWithDefault(CreationProcess.Manual) and
    (__ \ RELATIONSHIPS \ HAS_ACCESS_POINT).formatSeqOrEmpty[AccessPointF] and
    (__ \ RELATIONSHIPS \ HAS_MAINTENANCE_EVENT).formatSeqOrEmpty[Entity] and
    (__ \ RELATIONSHIPS \ HAS_UNKNOWN_PROPERTY).formatSeqOrEmpty[Entity]
  )(RepositoryDescriptionF.apply, unlift(RepositoryDescriptionF.unapply))

  implicit object Converter extends Writable[RepositoryDescriptionF] {
    val restFormat = repositoryDescriptionFormat
  }
}


case class RepositoryDescriptionF(
  isA: EntityType.Value = EntityType.RepositoryDescription,
  id: Option[String],
  languageCode: String,
  name: String,
  otherFormsOfName: Option[Seq[String]] = None,
  parallelFormsOfName: Option[Seq[String]] = None,
  @models.relation(Ontology.ENTITY_HAS_ADDRESS) addresses: Seq[AddressF] = Nil,
  details: IsdiahDetails,
  access: IsdiahAccess,
  services: IsdiahServices,
  control: IsdiahControl,
  creationProcess: CreationProcess.Value = CreationProcess.Manual,
  accessPoints: Seq[AccessPointF] = Nil,
  maintenanceEvents: Seq[Entity] = Nil,
  unknownProperties: Seq[Entity] = Nil
) extends Model with Persistable with Description {

  import Isdiah._

  def displayText = details.history orElse details.generalContext

  def toSeq = Seq(
    HISTORY -> details.history,
    GEOCULTURAL_CONTEXT -> details.generalContext,
    MANDATES -> details.mandates,
    ADMINISTRATIVE_STRUCTURE -> details.administrativeStructure,
    RECORDS -> details.records,
    BUILDINGS -> details.buildings,
    HOLDINGS -> details.holdings,
    FINDING_AIDS -> details.findingAids,
    OPENING_TIMES -> access.openingTimes,
    CONDITIONS -> access.conditions,
    ACCESSIBILITY -> access.accessibility,
    RESEARCH_SERVICES -> services.researchServices,
    REPROD_SERVICES -> services.reproductionServices,
    PUBLIC_AREAS -> services.publicAreas,
    DESCRIPTION_IDENTIFIER -> control.descriptionIdentifier,
    INSTITUTION_IDENTIFIER -> control.institutionIdentifier,
    RULES_CONVENTIONS -> control.rulesAndConventions,
    STATUS -> control.status,
    LEVEL_OF_DETAIL -> control.levelOfDetail,
    DATES_CVD -> control.datesCDR,
    MAINTENANCE_NOTES -> control.maintenanceNotes
  )
}

object RepositoryDescription {
  import Isdiah._
  import Entity._
  import defines.EnumUtils.enumMapping

  val form = Form(
    mapping(
      ISA -> ignored(EntityType.RepositoryDescription),
      ID -> optional(nonEmptyText),
      LANG_CODE -> nonEmptyText,
      AUTHORIZED_FORM_OF_NAME -> text,
      OTHER_FORMS_OF_NAME -> optional(seq(nonEmptyText)),
      PARALLEL_FORMS_OF_NAME -> optional(seq(nonEmptyText)),
      ADDRESS_AREA -> seq(Address.form.mapping),
      DESCRIPTION_AREA -> mapping(
        HISTORY -> optional(nonEmptyText),
        GEOCULTURAL_CONTEXT -> optional(nonEmptyText),
        MANDATES -> optional(nonEmptyText),
        ADMINISTRATIVE_STRUCTURE -> optional(nonEmptyText),
        RECORDS -> optional(nonEmptyText),
        BUILDINGS -> optional(nonEmptyText),
        HOLDINGS -> optional(nonEmptyText),
        FINDING_AIDS -> optional(nonEmptyText)
      )(IsdiahDetails.apply)(IsdiahDetails.unapply),
      ACCESS_AREA -> mapping(
        OPENING_TIMES -> optional(text),
        CONDITIONS -> optional(text),
        ACCESSIBILITY -> optional(text)
      )(IsdiahAccess.apply)(IsdiahAccess.unapply),
      SERVICES_AREA -> mapping(
        RESEARCH_SERVICES -> optional(text),
        REPROD_SERVICES -> optional(text),
        PUBLIC_AREAS -> optional(text)
      )(IsdiahServices.apply)(IsdiahServices.unapply),
      CONTROL_AREA -> mapping(
        DESCRIPTION_IDENTIFIER -> optional(text),
        INSTITUTION_IDENTIFIER -> optional(text),
        RULES_CONVENTIONS -> optional(text),
        STATUS -> optional(text),
        LEVEL_OF_DETAIL -> optional(text),
        DATES_CVD -> optional(text),
        LANGUAGES_USED -> optional(seq(nonEmptyText)),
        SCRIPTS_USED -> optional(seq(nonEmptyText)),
        SOURCES -> optional(seq(nonEmptyText)),
        MAINTENANCE_NOTES -> optional(text)
      )(IsdiahControl.apply)(IsdiahControl.unapply),
      CREATION_PROCESS -> default(enumMapping(CreationProcess), CreationProcess.Manual),
      ACCESS_POINTS -> seq(AccessPoint.form.mapping),
      MAINTENANCE_EVENTS -> seq(entity),
      UNKNOWN_DATA -> seq(entity)
    )(RepositoryDescriptionF.apply)(RepositoryDescriptionF.unapply)
  )
}

