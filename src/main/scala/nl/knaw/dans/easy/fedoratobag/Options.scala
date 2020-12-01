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

import nl.knaw.dans.easy.fedoratobag.filter.FileFilterType._
import nl.knaw.dans.easy.fedoratobag.TransformationType.ORIGINAL_VERSIONED
import nl.knaw.dans.easy.fedoratobag.filter.DatasetFilter

import scala.xml.Node

/**
 *
 * @param datasetFilter      which datasets are allowed for export
 * @param strict             if false: violation of the datasetFilter only cause a warning
 * @param europeana          if true: export only the largest PDF or image
 * @param originalVersioning if true: export two bags, the first with original files only;
 *                           not in combination with europeana
 */
case class Options(datasetFilter: DatasetFilter,
                   strict: Boolean = true,
                   europeana: Boolean = false,
                   originalVersioning: Boolean = false,
                  ) {
  private def isDCMI(node: Node) = node
    .attribute("http://easy.dans.knaw.nl/easy/easymetadata/eas/", "scheme")
    .exists(_.text == "DCMI")

  def firstFileFilter(emd: Node): FileFilterType = {
    if (originalVersioning) ORIGINAL_FILES
    else if (!europeana) ALL_FILES
         else {
           val dcmiType = (emd \ "type" \ "type").filter(isDCMI)
           if (dcmiType.text.toLowerCase.trim == "text") LARGEST_PDF
           else LARGEST_IMAGE
         }
  }
}
object Options {
  def apply(datasetFilter: DatasetFilter, commandLine: CommandLineOptions): Options = {
    Options(
      datasetFilter: DatasetFilter,
      commandLine.strictMode(),
      commandLine.europeana(),
      originalVersioning = commandLine.transformation() == ORIGINAL_VERSIONED,
    )
  }
}
