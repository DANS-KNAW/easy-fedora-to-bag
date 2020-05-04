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

import better.files.File
import nl.knaw.dans.easy.fedora2vault.TransformationType.TransformationType

case class LogRecord(easyDatasetId: DatasetId,
                     doi: String,
                     depositor: Depositor,
                     transformationType: TransformationType,
                     uuid: UUID,
                     comment: String,
                   ) {
  def write (implicit logFile: File): String = {
    logFile.appendLine(s"$easyDatasetId\t$doi\t$depositor\t$transformationType\t$uuid\t$comment")
    comment
  }
}
object LogRecord {
  val header = "easyDatasetId\tdoi\tdepositor\ttransformationType\tuuid\tcomment"
}
