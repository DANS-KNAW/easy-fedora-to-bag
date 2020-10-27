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

import nl.knaw.dans.pf.language.emd.EasyMetadata

import scala.collection.JavaConverters._

object FileFilterType extends Enumeration {
  type FileFilterType = Value

  // @formatter:off
  val PDF: FileFilterType = Value("PDF")
  val IMAGE: FileFilterType = Value("IMAGE")
  val ALL: FileFilterType = Value("ALL")
  // @formatter:on

  def from(europeana: Boolean, emd: EasyMetadata): FileFilterType = {
    if (!europeana) FileFilterType.ALL
    else null
  }

}
