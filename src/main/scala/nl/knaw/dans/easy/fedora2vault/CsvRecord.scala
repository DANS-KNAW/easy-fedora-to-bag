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
package nl.knaw.dans.easy.fedora2vault

import java.io.IOException
import java.util.UUID

import better.files.{ Dispose, File }
import nl.knaw.dans.easy.fedora2vault.Command.{ FeedBackMessage, commandLine }
import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import scala.util.Try

case class CsvRecord(easyDatasetId: DatasetId,
                     ipUUID: UUID,
                     doi: String,
                     depositor: Depositor,
                     transformationType: String,
                     comment: String,
                    ) {
  def print(implicit printer: CSVPrinter): Try[FeedBackMessage] = Try {
    printer.printRecord(easyDatasetId, ipUUID, doi, depositor, transformationType, comment)
    comment
  }
}

object CsvRecord {
  val csvFormat: CSVFormat = CSVFormat.RFC4180
    .withHeader("easyDatasetId", "uuid", "doi", "depositor", "transformationType", "comment")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  @throws[IOException]
  def printer(file: File): Dispose[CSVPrinter] = {
    val writer = file.newFileWriter(append = true)
    new Dispose(csvFormat.print(writer))
  }
}
