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

import java.util.UUID

import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.TransformationType.TransformationType
import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import scala.util.Try

case class CsvRecord(easyDatasetId: DatasetId,
                     doi: String,
                     depositor: Depositor,
                     transformationType: TransformationType,
                     uuid: UUID,
                     comment: String,
                    ) {
  def print(implicit printer: CSVPrinter): Try[FeedBackMessage] = Try(
    printer.printRecord(easyDatasetId, doi, depositor, transformationType, uuid, comment)
  ).map(_ => comment)
}

object CsvRecord {
  val csvFormat: CSVFormat = CSVFormat.RFC4180
    .withHeader("easyDatasetId", "doi", "depositor", "transformationType", "uuid", "comment")
    .withDelimiter(',')
    .withRecordSeparator('\n')
}
