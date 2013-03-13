package models.json

import play.api.libs.json._
import models.{DatePeriodType, Entity, DatePeriodF}
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import defines.EntityType


object DatePeriodFormat {
  import Entity.{TYPE => ETYPE,_}
  import DatePeriodF._
  import defines.EnumReader.enumReads

  implicit val datePeriodTypeReads = enumReads(DatePeriodType)

  implicit val datePeriodReads: Reads[DatePeriodF] = (
    (__ \ TYPE).read[EntityType.Value](equalsReads(EntityType.DatePeriod)) andKeep
    (__ \ ID).readNullable[String] and
      (__ \ DATA \ TYPE).readNullable[DatePeriodType.Value] and
      (__ \ DATA \ START_DATE).read[DateTime] and
      (__ \ DATA \ END_DATE).read[DateTime]
    )(DatePeriodF.apply _)

  import defines.EnumWriter.enumWrites
  implicit val datePeriodWrites = new Writes[DatePeriodF] {
    def writes(d: DatePeriodF): JsValue = {
      Json.obj(
        ID -> d.id,
        TYPE -> d.isA,
        DATA -> Json.obj(
          TYPE -> d.`type` ,
          START_DATE -> d.startDate,
          END_DATE -> d.endDate
        )
      )
    }
  }
}