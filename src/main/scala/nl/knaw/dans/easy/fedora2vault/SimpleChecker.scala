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

import nl.knaw.dans.common.lang.dataset.AccessCategory.{ OPEN_ACCESS, REQUEST_PERMISSION }
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl

import scala.util.{ Failure, Success, Try }
import scala.xml.Node

case class SimpleChecker(bagIndex: BagIndex) {
  def isSimple(emd: EasyMetadataImpl, ddm: Node, amd: Node, jumpOff: Seq[String]): Try[Unit] = {
    val doi = emd.getEmdIdentifier.getDansManagedDoi

    def metadataChecks = Try {
      if (doi == null) throw NotSimple("no DOI")
      if ((amd \ "datasetState").text == "PUBLISHED") throw NotSimple("not published")
      if (jumpOff.nonEmpty) throw NotSimple("has " + jumpOff.mkString(", "))
      if (emd.getEmdTitle.getPreferredTitle.toLowerCase.contains("thematische collectie"))
        throw NotSimple("is a thematische collectie")
      emd.getEmdRights.getAccessCategory match {
        case OPEN_ACCESS | REQUEST_PERMISSION =>
        case _ => throw NotSimple("AccessCategory is neither OPEN_ACCESS nor REQUEST_PERMISSION")
      }

      ((ddm \ "isVersionOf").theSeq ++ (ddm \ "replaces").theSeq)
        .foreach(node =>
          if (Seq("DOI", "URN").contains(node \@ "scheme") ||
            node.text.contains("easy-dataset:")
          ) throw NotSimple("has isVersionOf/replaces " + node.toString())
        )
    }

    for {
      _ <- metadataChecks
      maybe <- bagIndex.bagByDoi(doi)
      _ <- maybe.map(failNotSimple(doi)).getOrElse(Success(()))
    } yield ()
  }

  private def failNotSimple(doi: DatasetId)(responseBody: String) = Failure(
    NotSimple(s"dataset with DOI[$doi] found in vault - $responseBody")
  )

  /** An Exception that is fatal for the dataset but NOT fatal for a batch of datasets */
  case class NotSimple(s: String) extends Exception(s"Not a simple dataset: $s")
}
