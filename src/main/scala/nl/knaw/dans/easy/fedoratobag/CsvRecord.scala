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
package nl.knaw.dans.easy.fedoratobag

import better.files.{ Dispose, File }
import nl.knaw.dans.easy.fedoratobag.Command.FeedBackMessage
import nl.knaw.dans.easy.fedoratobag.TransformationType.ORIGINAL_VERSIONED
import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import java.io.IOException
import java.util.UUID
import scala.util.Try

case class CsvRecord(easyDatasetId: DatasetId,
                     packageUuid1: Option[UUID],
                     packageUuid2: Option[UUID],
                     doi: String,
                     depositor: Depositor,
                     transformationType: String,
                     comment: String,
                    ) {
  def print(implicit printer: CSVPrinter): Try[FeedBackMessage] = Try {
    printer.printRecord(easyDatasetId, packageUuid1.getOrElse(""), packageUuid2.getOrElse(""), doi, depositor, transformationType, comment)
    comment
  }
}

object CsvRecord {
  val csvFormat: CSVFormat = CSVFormat.RFC4180
    .withHeader("easyDatasetId", "uuid1", "uuid2", "doi", "depositor", "transformationType", "comment")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  @throws[IOException]
  def printer(file: File): Dispose[CSVPrinter] = {
    val writer = file.newFileWriter(append = true)
    new Dispose(csvFormat.print(writer))
  }

  def apply(datasetId: DatasetId, datasetInfo: DatasetInfo, uuid1: UUID, uuid2: Option[UUID], options: Options): CsvRecord = {
    val violations = datasetInfo.maybeFilterViolations
    val commentSuffix = (datasetInfo.withPayload , options.noPayload) match {
      case (true, _) => ""
      case (false, false) => s"; no payload, nr of files exceeds ${ options.cutoff }"
      case (false, true) => s"; no payload specified"
    }
    val commentPrefix = violations.map(_ => "OK though it ").getOrElse("OK")
    val typePrefix = violations.map(_ => "not strict ").getOrElse("")
    val typeSuffix = uuid2.map(_ => "")
      .getOrElse(
        if (options.transformationType == ORIGINAL_VERSIONED)
          " without second bag"
        else ""
      )
    new CsvRecord(
      datasetId,
      Option(uuid1),
      uuid2,
      datasetInfo.doi,
      datasetInfo.depositor,
      typePrefix + options.transformationType + typeSuffix,
      // N.B: called after movePackageAtomically hence OK
      commentPrefix + violations.mkString("") + commentSuffix,
    )
  }
}
