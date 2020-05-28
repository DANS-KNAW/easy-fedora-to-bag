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

    val dansRelations = Seq(
      ddmRelation("isVersionOf"),
      ddmRelation("replaces"),
    ).flatten.filter(hasDansId)
    for {
      _ <- if (dansRelations.isEmpty) Success(())
           else Failure(NotSimple("has DANS-id(s) in " + dansRelations.map(_.toOneLiner).mkString))
      _ <- emdAmdChecks
      maybeBagInfo <- bagIndex.bagByDoi(doi)
      _ <- maybeBagInfo.map(bagFoundFailure).getOrElse(Success(()))
    } yield ()
  }

  private def hasDansId(node: Node): Boolean = {
    // see both DDM.toRelationXml methods for what might occur
    (node \@ "href", node.text) match {
      case (href, _) if isDansId(href) => true
      case (_, text) if isDansId(text) => true
      case _ => false
    }
  }

  private def isDansId(s: String) = Seq(
    "doi.org/10.17026/",
    "easy-dataset:",
    "urn:nbn:nl:ui:13-",
  ).exists(s.contains(_))
}
