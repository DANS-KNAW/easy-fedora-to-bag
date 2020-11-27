package nl.knaw.dans.easy.fedoratobag.filter

import nl.knaw.dans.lib.string._

import scala.util.Try
import scala.xml.{ Elem, Node }

case class Versions(previous: Seq[String],
                    next: Seq[String],
                   )

object Versions {
  def apply(emd: Elem): Try[Versions] = Try {
    val relations = emd \ "relation"
    new Versions(
      getDansIDs((relations \ "replaces").theSeq ++ (relations \ "isVersionOf").theSeq),
      getDansIDs((relations \ "replacedBy").theSeq ++ (relations \ "hasVersion").theSeq),
    )
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
