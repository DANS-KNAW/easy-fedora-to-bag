package nl.knaw.dans.easy.fedoratobag.filter

import nl.knaw.dans.lib.string._
import org.joda.time.DateTime

import scala.util.Try
import scala.xml.{ Elem, Node }

case class VersionInfo(submitted: DateTime,
                       self: Seq[String],
                       previous: Seq[String],
                       next: Seq[String],
                      )

object VersionInfo {
  def apply(emd: Elem): Try[VersionInfo] = Try {
    val relations = emd \ "relation"
    new VersionInfo(
      parse((emd \ "date" \ "dateSubmitted").text),
      (emd \ "identifier" \ "identifier").theSeq.filter(isSelf).map(_.text),
      getDansIDs((relations \ "replaces").theSeq ++ (relations \ "isVersionOf").theSeq),
      getDansIDs((relations \ "replacedBy").theSeq ++ (relations \ "hasVersion").theSeq),
    )
  }

  private def parse(date: String): DateTime = {
    val parsed = DateTime.parse(date)
    if (parsed.getYear < 9999) parsed
    else DateTime.parse(date.substring(0, 8))
  }

  val easNameSpace = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  private def isSelf(node: Node) = {
    val scheme = node.attribute(easNameSpace, "scheme")
      .map(_.text).getOrElse("")
    Seq("PID", "DMO_ID", "DOI").exists(scheme.contains)
  }

  private def getDansIDs(relations: Seq[Node]) = {
    relations.map(relation =>
      child("subject-link", relation).getOrElse(
        child("subject-title", relation).getOrElse(relation)
      )
    )
  }.map(_.text)
    .withFilter(isDansId)
    .map(strip(dansIdPrefixes))

  private def child(tag: String, node: Node) = (node \ tag).theSeq.find(_.text.toOption.isDefined)

  val dansIdPrefixes = Seq("10.17026", "10.5072", "easy-dataset:", "urn:nbn:nl:ui:13-")

  def isDansId(s: String): Boolean = dansIdPrefixes.exists(s.contains(_))

  private def strip(prefixes: Seq[String])(value: String) = prefixes.foldLeft(value) {
    case (acc, nextPrefix) => acc.replaceAll(s".*$nextPrefix", nextPrefix)
  }
}
