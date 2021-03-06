package utils.search

import defines.EntityType
import backend.rest.Constants._
import play.api.libs.json.Json


object SearchField extends Enumeration {
  type Field = Value
  val Identifier = Value("identifier")
  val Title = Value("title")
  val Creator = Value("creator")
  val Person = Value("person")
  val Place = Value("place")
  val Subject = Value("subject")
  val Address = Value("address")

  implicit val format = defines.EnumUtils.enumFormat(SearchField)
}

object SearchOrder extends Enumeration {
  type Order = Value
  val Id = Value("isParent.desc,identifier.asc")
  val Score = Value("score.desc")
  val Name = Value("name_sort.asc")
  val DateNewest = Value("lastUpdated.desc")
  val Country = Value("countryCode.asc")
  val Holder = Value("repositoryName.asc")
  val Location = Value("geodist().asc")
  val Detail = Value("charCount.desc")
  val ChildCount = Value("childCount.desc")

  implicit val format = defines.EnumUtils.enumFormat(SearchOrder)
}

object SearchType extends Enumeration {
  type Type = Value
  val All = Value("all")
  val Collection = Value("collection")
  val Authority = Value("authority")
  val Repository = Value("repository")

  implicit val format = defines.EnumUtils.enumFormat(SearchType)
}

object SearchMode extends Enumeration {
  type Type = Value
  val DefaultAll = Value("all")
  val DefaultNone = Value("none")

  implicit val format = defines.EnumUtils.enumFormat(SearchMode)
}


/**
 * Class encapsulating the parameters of a Solr search.
 *
 * User: michaelb
 */
case class SearchParams(
  query: Option[String] = None,
  page: Option[Int] = None,
  count: Option[Int] = None,
  sort: Option[SearchOrder.Value] = None,
  reverse: Option[Boolean] = Some(false),
  entities: Seq[EntityType.Value] = Nil,
  fields: Seq[SearchField.Value] = Nil,
  excludes: Option[List[String]] = None,
  filters: Option[List[String]] = None
) {

  def pageOrDefault: Int = page.getOrElse(1)

  def countOrDefault: Int = count.getOrElse(DEFAULT_LIST_LIMIT)

  /**
   * Is there an active constraint on these params?
   * TODO: Should this include page etc?
   */
  def isFiltered: Boolean = !query.forall(_.trim.isEmpty)

  def offset = Math.max(0, (pageOrDefault - 1) * countOrDefault)

  /**
   * Set unset values from another (optional) instance.
   */
  def setDefault(default: Option[SearchParams]): SearchParams = default match {
    case Some(d) => copy(
      query = query orElse d.query,
      page = page orElse d.page,
      count = count orElse d.count,
      sort = sort orElse d.sort,
      reverse = reverse orElse d.reverse,
      entities = if (entities.isEmpty) d.entities else entities,
      fields = if (fields.isEmpty) d.fields else fields,
      excludes = excludes orElse d.excludes,
      filters = filters orElse d.filters
    )
    case None => this
  }
}

object SearchParams {
  val REVERSE = "desc"
  val SORT = "sort"
  val QUERY = "q"
  val FIELD = "qf"
  val ENTITY = "st"
  val EXCLUDE = "ex"
  val FILTERS = "f"

  import play.api.data.Forms._
  import play.api.data.Form
  import utils.PageParams._
  import defines.EnumUtils._

  def empty: SearchParams = new SearchParams()

  implicit val writes = Json.writes[SearchParams]

  // Form deserialization
  val form = Form(
    mapping(
      QUERY -> optional(nonEmptyText),
      PAGE_PARAM -> optional(number(min = 1)),
      // ensure the maximum list limit is respected, but as
      // a limit, not as a constraint that causes binding failure.
      LIMIT_PARAM -> optional(number(min = 0).transform(
        i => i.min(MAX_LIST_LIMIT),
        (i: Int) => i)
      ),
      SORT -> optional(enumMapping(SearchOrder)),
      REVERSE -> optional(boolean),
      ENTITY -> tolerantSeq(EntityType),
      FIELD -> tolerantSeq(SearchField),
      EXCLUDE -> optional(list(nonEmptyText)),
      FILTERS -> optional(list(nonEmptyText))
    )(SearchParams.apply)(SearchParams.unapply)
  )
}
