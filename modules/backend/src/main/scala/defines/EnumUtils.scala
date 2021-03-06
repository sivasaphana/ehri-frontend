package defines

/**
 * @author Mike Bryant (http://github.com/mikesname)
 */

import play.api.data.format.Formatter
import play.api.data.{FormError, Forms, Mapping}
import play.api.libs.json._

object EnumUtils {

  /**
   * Deserializer for Enumeration types.
   *
   * {{{
   * (Json \ "status").as(enum(Status))
   * }}}
   */
  def enumReads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
    def reads(json: JsValue): JsResult[E#Value] = json match {
      case JsString(s) =>
        try {
          JsSuccess(enum.withName(s))
        } catch {
          case _: NoSuchElementException => JsError("Enumeration expected of type: '%s', but it does not appear to contain the value: '%s'".format(enum.getClass, s))
        }
      case _ => JsError("String value expected")
    }
  }

  implicit def enumWrites[E <: Enumeration]: Writes[E#Value] = new Writes[E#Value] {
    def writes(v: E#Value): JsValue = JsString(v.toString)
  }

  implicit def enumFormat[E <: Enumeration](enum: E): Format[E#Value] = Format(enumReads(enum), enumWrites)

  /**
   * Constructs a simple mapping for a text field (mapped as `scala.Enumeration`)
   *
   * For example:
   * {{{
   *   Form("status" -> enum(Status))
   * }}}
   *
   * @param enum the Enumeration#Value
   */
  def enumMapping[E <: Enumeration](enum: E): Mapping[E#Value] = Forms.of(enumFormBinder(enum))

  /**
   * Default formatter for `scala.Enumeration`
   *
   */
  private def enumFormBinder[E <: Enumeration](enum: E): Formatter[E#Value] = new Formatter[E#Value] {
    def bind(key: String, data: Map[String, String]) = {
      play.api.data.format.Formats.stringFormat.bind(key, data).right.flatMap { s =>
        scala.util.control.Exception.allCatch[E#Value]
          .either(enum.withName(s))
          .left.map(e => Seq(FormError(key, "error.enum", Nil)))
      }
    }
    def unbind(key: String, value: E#Value) = Map(key -> value.toString)
  }

  /**
   * Tolerant form binder for a sequence of enumeration values.
   *
   * Takes only the valid values and ignores the rest.
   */
  def tolerantSeq[E <: Enumeration](enum: E): Mapping[Seq[E#Value]] = Forms.seq(Forms.text).transform(
    strings => enum.values.filter(v => strings.contains(v.toString)).toSeq,
    values => values.map(_.toString)
  )
}
