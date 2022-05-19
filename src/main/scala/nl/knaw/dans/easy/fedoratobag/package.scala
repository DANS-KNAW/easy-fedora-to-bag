/*
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
package nl.knaw.dans.easy

import better.files.File
import org.apache.commons.csv.{ CSVFormat, CSVParser }
import org.apache.commons.lang.StringUtils
import org.joda.time.format.{ DateTimeFormatter, ISODateTimeFormat }
import org.joda.time.{ DateTime, DateTimeZone }

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.UUID
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.util.{ Failure, Try }
import scala.xml.{ Node, PrettyPrinter, Utility }

package object fedoratobag {

  type DatasetId = String
  type Depositor = String

  type LdapEnv = java.util.Hashtable[String, String]

  case class NoPayloadFilesException() extends Exception("Running with original-versioned and no payload files found to put into bag")
  case class DatabaseConnection(url: String, username: String, password: String)

  val dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime()

  def now: String = DateTime.now(DateTimeZone.UTC).toString(dateTimeFormatter)

  /** An interpolated string is a method. It needs evaluation before passing in to define expectations. */
  def mockFriendly(interpolated: String): String = interpolated

  private val prologue = """<?xml version='1.0' encoding='UTF-8'?>"""
  private val logPrinter = new PrettyPrinter(-1, 0)

  implicit class XmlExtensions(val elem: Node) extends AnyVal {

    def serialize: String = {
      prologue + "\n" + Utility.serialize(elem, preserveWhitespace = true)
    }

    def toOneLiner: String = {
      logPrinter.format(Utility.trim(elem)).trim
    }
  }

  implicit class RichTries[T](val tries: TraversableOnce[Try[T]]) extends AnyVal {
    // TODO candidate for nl.knaw.dans.lib.error ?
    //  copied from https://github.com/DANS-KNAW/easy-deposit-api/blob/ff109d27d2f2548c9e053c34d41627a539a381d9/src/main/scala/nl.knaw.dans.easy.deposit/package.scala#L48
    def failFastOr[R](onSuccess: => Try[R]): Try[R] = {
      tries
        .collectFirst { case Failure(e) => Failure(e) }
        .getOrElse(onSuccess)
    }
  }

  implicit class RichPath(val path: Path) extends AnyVal {
    def startsWithOriginalFolder(): Boolean = {
      path.getName(0).toString.toLowerCase == "original"
    }

    def bagPath(isOriginalVersioned: Boolean): Path = {
      if (isOriginalVersioned && startsWithOriginalFolder)
        path.subpath(1, path.getNameCount)
      else path
    }
  }

  def loadInputFile(csvFile: File): Try[List[InputFileRecord]] = {
    import resource.managed

    def csvParse(csvParser: CSVParser): Iterator[InputFileRecord] = {
      csvParser.iterator().asScala
        .map(r => {
          val datasetId = r.get("dataset_id")
          val optUuid1 = if (StringUtils.isBlank(r.get("uuid1"))) None
                         else Option(UUID.fromString(r.get("uuid1")))
          val optUuid2 = if (StringUtils.isBlank(r.get("uuid2"))) None
                         else Option(UUID.fromString(r.get("uuid2")))
          InputFileRecord(datasetId, optUuid1, optUuid2)
        })
    }

    managed(CSVParser.parse(
      csvFile.toJava,
      StandardCharsets.UTF_8,
      CSVFormat.RFC4180.withFirstRecordAsHeader().withIgnoreEmptyLines())).map(csvParse).map(_.toList).tried
  }
}
