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

import better.files.File
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import org.joda.time.format.ISODateTimeFormat

import scala.util.Try

object DepositProperties {
  private val dateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis()

  def create(depositDir: File, csvRecord: CsvRecord): Try[Unit] = Try {

    val nowWithoutMillis: DatasetId = {
      val now = DateTime.now(UTC)
      now.minusMillis(now.millisOfSecond().get())
    }.toString(dateTimeFormatter)

    new PropertiesConfiguration() {
      addProperty("creation.timestamp", nowWithoutMillis)
      addProperty("state.label", "SUBMITTED")
      addProperty("state.description", "Deposit is valid and ready for post-submission processing")
      addProperty("depositor.userId", csvRecord.depositor)
      addProperty("identifier.doi", csvRecord.doi)
      addProperty("identifier.fedora", csvRecord.easyDatasetId)
      addProperty("deposit.origin", "easy-fedora2vault")
    }.save((depositDir / "deposit.properties").toJava)
  }
}
