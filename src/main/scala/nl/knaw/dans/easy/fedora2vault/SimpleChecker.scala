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

  /** An Exception that is fatal for the dataset but NOT fatal for a batch of datasets */
  private case class NotSimple(s: String) extends Exception(s"Not a simple dataset: $s")

  def isSimple(emd: EasyMetadataImpl, ddm: Node, amd: Node, jumpOff: Seq[String]): Try[Unit] = {
    val doi = emd.getEmdIdentifier.getDansManagedDoi

    def emdAmdChecks = Try {
      if (doi == null) throw NotSimple("no DOI")
      if (jumpOff.nonEmpty) throw NotSimple("has " + jumpOff.mkString(", "))
      if (emd.getEmdTitle.getPreferredTitle.toLowerCase.contains("thematische collectie"))
        throw NotSimple("is a thematische collectie")
      emd.getEmdRights.getAccessCategory match {
        case OPEN_ACCESS | REQUEST_PERMISSION =>
        case _ => throw NotSimple("AccessCategory is neither OPEN_ACCESS nor REQUEST_PERMISSION")
      }
      if ((amd \ "datasetState").text != "PUBLISHED") throw NotSimple("not published")
    }

    def ddmRelation(qualifier: String): Seq[Node] =
      (ddm \\ qualifier).theSeq

    def bagFoundFailure(bagInfo: String) = Failure(
      NotSimple(s"Dataset found in vault. DOI[$doi] ${ bagIndex.bagIndexUri } returned: $bagInfo")
    )

    for {
      _ <- (ddmRelation("isVersionOf") ++ ddmRelation("replaces"))
        .map(internalRelationCheck)
        .failFastOr(Success(()))
      _ <- emdAmdChecks
      maybeBagInfo <- bagIndex.bagByDoi(doi)
      _ <- maybeBagInfo.map(bagFoundFailure).getOrElse(Success(()))
    } yield ()
  }

  private def internalRelationCheck(node: Node): Try[Unit] = {
    // see both DDM.toRelationXml methods for what might occur
    lazy val hasInternal = Failure(NotSimple("invalid isVersionOf/replaces " + node.toString()))
    (node \@ "href", node.text) match {
      case (h, _) if h.startsWith("https://doi.org/10.17026") => hasInternal
      case (_, t) if t.startsWith("https://doi.org/10.17026") => hasInternal
      case _ => Success(())
    }
  }
}
