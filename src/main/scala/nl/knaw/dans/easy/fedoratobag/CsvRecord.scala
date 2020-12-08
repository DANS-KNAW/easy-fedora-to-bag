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
package nl.knaw.dans.easy.fedoratobag

import java.io.IOException
import java.util.UUID

import better.files.{ Dispose, File }
import nl.knaw.dans.easy.fedoratobag.Command.FeedBackMessage
import nl.knaw.dans.easy.fedoratobag.TransformationType.ORIGINAL_VERSIONED
import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import scala.util.Try

case class CsvRecord(easyDatasetId: DatasetId,
                     packageUuid1: UUID,
                     packageUuid2: Option[UUID],
                     doi: String,
                     depositor: Depositor,
                     transformationType: String,
                     comment: String,
                    ) {
  def print(implicit printer: CSVPrinter): Try[FeedBackMessage] = Try {
    printer.printRecord(easyDatasetId, packageUuid1, packageUuid2.getOrElse(""), doi, depositor, transformationType, comment)
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
    val comment = if (uuid2.isEmpty && options.transformationType == ORIGINAL_VERSIONED)
                    "No second bag. " + violations.mkString("")
                  else violations.getOrElse("OK")
    new CsvRecord(
      datasetId,
      uuid1,
      uuid2,
      datasetInfo.doi,
      datasetInfo.depositor,
      violations.map(_ => "not strict ").getOrElse("") + options.transformationType,
      comment,
    )
  }
}
