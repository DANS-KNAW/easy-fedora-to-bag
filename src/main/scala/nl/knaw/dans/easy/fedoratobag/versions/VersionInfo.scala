/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.fedoratobag.versions

import nl.knaw.dans.lib.string._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat.forPattern

import scala.util.Try
import scala.xml.{ Elem, Node }

case class VersionInfo(submitted: Long,
                       self: Seq[String],
                       previous: Seq[String],
                       next: Seq[String],
                      )

object VersionInfo {
  def apply(emd: Elem): Try[VersionInfo] = Try {
    val relations = emd \ "relation"
    val dateContainer = emd \ "date"
    val date = (dateContainer \ "dateSubmitted").headOption
      .getOrElse(dateContainer \ "created").headOption
      .map(_.text)
      .getOrElse("1900-01-01")
    new VersionInfo(
      fixDateIfTooLarge(date).getOrElse(0),
      (emd \ "identifier" \ "identifier").theSeq.filter(isSelf).map(_.text),
      getDansIDs((relations \ "replaces").theSeq ++ (relations \ "isVersionOf").theSeq),
      getDansIDs((relations \ "replacedBy").theSeq ++ (relations \ "hasVersion").theSeq),
    )
  }

  private def fixDateIfTooLarge(date: String): Try[Long] = Try(new DateTime(date))
    .map { dateTime =>
      if (dateTime.getYear < 10000) dateTime
      else DateTime.parse(dateTime.getYear.toString, forPattern("yMMdd"))
    }.map(_.getMillis)

  val easNameSpace = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"

  private def isSelf(node: Node) = {
    val scheme = node
      .attribute(easNameSpace, "scheme")
      .map(_.text)
      .getOrElse("")
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
