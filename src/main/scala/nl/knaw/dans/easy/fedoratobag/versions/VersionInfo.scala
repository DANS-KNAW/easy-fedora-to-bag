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

import java.time.format.DateTimeFormatter.BASIC_ISO_DATE
import java.time.{ DateTimeException, LocalDate }

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
      fixDateIfTooLarge((emd \ "date" \ "dateSubmitted").text),
      (emd \ "identifier" \ "identifier").theSeq.filter(isSelf).map(_.text),
      getDansIDs((relations \ "replaces").theSeq ++ (relations \ "isVersionOf").theSeq),
      getDansIDs((relations \ "replacedBy").theSeq ++ (relations \ "hasVersion").theSeq),
    )
  }

  private def fixDateIfTooLarge(date: String): DateTime = Try{new DateTime(
    if (date.length <= 13) date
    else try {
      LocalDate.parse(date.substring(0, 8), BASIC_ISO_DATE).toString
    } catch {
      case _: DateTimeException => date
    }
  )}.getOrElse(
    throw new IllegalArgumentException(s"Missing or invalid dateSubmitted [$date]")
  )

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
