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

import java.util.UUID

import nl.knaw.dans.bag.v0.DansV0Bag

case class VersionInfo(
                        doi: String,
                        urn: String,
                        packageId: UUID,
                      ) {
  def addVersionOf(bag: DansV0Bag): DansV0Bag = {
    bag.withIsVersionOf(packageId)
      // the following keys should match easy-fedora-to-bag
      .addBagInfo("Base-DOI", doi)
      .addBagInfo("Base-URN", urn)
  }
}
object VersionInfo {
  def apply(datasetInfo: DatasetInfo, packageID: UUID): VersionInfo = {
    VersionInfo(datasetInfo.doi, datasetInfo.urn, packageID)
  }
}
