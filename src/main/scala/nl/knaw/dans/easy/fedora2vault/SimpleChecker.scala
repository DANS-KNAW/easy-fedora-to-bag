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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl

import scala.util.{ Failure, Success, Try }
import scala.xml.Node

case class SimpleChecker(bagIndex: BagIndex) extends DebugEnhancedLogging {

  def isSimple(emd: EasyMetadataImpl, ddm: Node, amd: Node, jumpOff: Seq[String]): Try[Unit] = {
    val doi = emd.getEmdIdentifier.getDansManagedDoi
    val triedMaybeVaultResponse = bagIndex.bagByDoi(doi).map(_.toSeq)
    val violations = Seq(
      "1: has no DANS DOI" -> (if (Option(doi).isEmpty) Seq("")
                               else Seq[String]()),
      "2: has jump off" -> jumpOff,
      "3: title has 'thematische collectie'" -> Option(emd.getEmdTitle.getPreferredTitle)
        .filter(_.toLowerCase.contains("thematische collectie")).toSeq,
      "4: has invalid rights" -> findInvalidRights(emd),
      "5: not published" -> findInvalidState(amd),
      "6: DANS relations" -> findDansRelations(ddm),
      "7: is in the vault" -> triedMaybeVaultResponse.getOrElse(Seq("IO exception")),
    ).filter(_._2.nonEmpty).toMap

    violations.foreach { case (rule, violations) =>
      violations.map(s => logger.warn(s"violated $rule $s"))
    }
    lazy val errorMessage: String = violations.keys
      .map(_.replaceAll(":.*", ""))
      .mkString("Not a simple dataset. Violates rule ", ", ", "")
    for {
      _ <- triedMaybeVaultResponse // an IOException is not a violation
      _ <- if (violations.isEmpty) Success(())
           else Failure(new Exception(errorMessage))
    } yield ()
  }

  private def findInvalidRights(emd: EasyMetadataImpl) = {
    val maybe = Option(emd.getEmdRights.getAccessCategory)
    if (maybe.isEmpty) Seq("not found")
    else maybe
      .withFilter(!Seq(OPEN_ACCESS, REQUEST_PERMISSION).contains(_))
      .map(_.toString).toSeq
  }

  private def findInvalidState(amd: Node) = {
    val seq = amd \ "datasetState"
    if (seq.isEmpty) Seq("not found")
    else seq
      .withFilter(node => !(node.text == "PUBLISHED"))
      .map(_.text)
  }

  private def findDansRelations(ddm: Node) = {
    Seq(
      (ddm \\ "isVersionOf").theSeq,
      (ddm \\ "replaces").theSeq,
    ).flatten
      .withFilter(hasDansId)
      .map(_.toOneLiner)
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
