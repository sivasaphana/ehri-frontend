package solr

import solr.facet.{QueryFacetClass, FieldFacetClass}
import scala.xml.{Node, Elem}
import defines.EntityType
import utils.search.{FacetDisplay, AppliedFacet, SearchDescription, FacetClassList}
import play.api.Logger

/**
 * User: michaelb
 */
object SolrQueryParser {
  def apply(responseString: String) = new SolrQueryParser(xml.XML.loadString(responseString))
}

/**
 * Helper class for parsing a Solr XML Response.
 * @param response
 */
case class SolrQueryParser(response: Elem) {

  import SolrConstants._

  /**
   * Fetch the search description items returned in this response.
   */
  lazy val items: Seq[SearchDescription] = (response \ "lst" \ "lst" \ "result" \ "doc").map { doc =>
    SearchDescription(
      id = (doc \\ "str").filter(hasAttr("name", ID)).text,
      itemId = (doc \\ "str").filter(hasAttr("name", ITEM_ID)).text,
      name = (doc \\ "str").filter(hasAttr("name", NAME_EXACT)).text,
      `type` = EntityType.withName((doc \\ "str").filter(hasAttr("name", TYPE)).text.trim),
      gid = (doc \\ "long").filter(hasAttr("name", DB_ID)).text.toLong
    )
  }

  /**
   * Get the *first* spellcheck suggestion offered. Ultimately, more might be useful,
   * but the first is okay for now...
   */
  lazy val spellcheckSuggestion: Option[(String,String)] = {
    for {
      suggestion <- (response \ "lst" \ "lst").filter(hasAttr("name", "suggestions")).headOption
      name <- (suggestion \ "lst" \ "@name").headOption
      word <- (suggestion \ "lst" \ "arr" \ "lst" \ "str").filter(hasAttr("name", "word")).headOption
    } yield (name.text, word.text)
  }


  /**
   * Count the number of search descriptions returned in this response.
   */
  lazy val count: Int = {
    val s = (response \ "lst" \ "lst" \ "int").filter(hasAttr("name", "ngroups")).text
    try {
      s.toInt
    } catch {
      case e: NumberFormatException => 0
    }
  }

  /**
   * Extract the facet data from this response, given a list of the facets
   * used to constrain the response, and the complete set of facet info requested.
   * @param appliedFacets
   * @param allFacets
   * @return
   */
  def extractFacetData(appliedFacets: List[AppliedFacet], allFacets: FacetClassList): FacetClassList = {
    val tags = allFacets.filter(_.tagExclude).map(_.key)
    allFacets.flatMap { fc => fc match {
      case ffc: FieldFacetClass => List(extractFieldFacet(ffc, appliedFacets, tags))
      case qfc: QueryFacetClass => List(extractQueryFacet(qfc, appliedFacets, tags))
      case e => {
        Logger.logger.warn("Unknown facet class type: {}", e)
        Nil
      }
    }}
  }

  private def tagFunc(tags: List[String]): String = tags match {
    case Nil => ""
    case _ => "{!ex=" + tags.mkString(",") + "}"
  }

  /**
   * Extract field facets from XML which looks like:
   *
   * <lst name="facet_counts">
   *  <lst name="facet_queries"/>
   *   <lst name="facet_fields">
   *     <lst name="languageCode">
   *       <int name="en">697</int>
   *       <int name="de">1</int>
   *       <int name="nl">1</int>
   *   ...
   */
  private def extractFieldFacet(fc: solr.facet.FieldFacetClass, appliedFacets: List[AppliedFacet], tags: List[String] = Nil): solr.facet.FieldFacetClass = {
    val applied: List[String] = appliedFacets.filter(_.name == fc.key).headOption.map(_.values).getOrElse(List[String]())
    val nodeOpt = response.descendant.filter(n => (n \ "@name").text == "facet_fields").headOption
    val facets = nodeOpt.toList.flatMap { node =>
      val children = node.descendant.filter(n => (n \ "@name").text == fc.key)
      children.flatMap(_.descendant).flatMap { c =>
        val nameNode = (c \ "@name")
        if (nameNode.length == 0) Nil
        else
           List(solr.facet.SolrFieldFacet(
              nameNode.text, nameNode.text, None,
              c.text.toInt, applied.contains(nameNode.text)))
      }
    }

    fc.copy(facets = facets)
  }

  /**
   * Extract query facets from Solr XML response.
   */
  private def extractQueryFacet(fc: solr.facet.QueryFacetClass, appliedFacets: List[AppliedFacet], tags: List[String] = Nil): solr.facet.QueryFacetClass = {
    val applied: List[String] = appliedFacets.filter(_.name == fc.key).headOption.map(_.values).getOrElse(List[String]())
    val facets = fc.facets.flatMap{ f =>
      var nameval = "%s%s:%s".format(tagFunc(tags), fc.key, f.solrValue)
      response.descendant.filter(n => (n \\ "@name").text == nameval).text match {
        case "" => Nil
        case v => List(
          f.copy(count = v.toInt, applied = applied.contains(f.value))
        )
      }
    }

    fc.copy(facets = facets)
  }

  private def hasAttr(name: String, value: String)(node: Node): Boolean = {
    node.attributes.exists(attr => attr.key == name && attr.value.text == value)
  }
}
