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
package nl.knaw.dans.easy.fedoratobag.filter

import nl.knaw.dans.common.lang.dataset.AccessCategory.{ OPEN_ACCESS, REQUEST_PERMISSION }
import nl.knaw.dans.easy.fedoratobag._
import nl.knaw.dans.easy.fedoratobag.versions.EmdVersionInfo
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl

import scala.util.{ Success, Try }
import scala.xml.Node

case class InvalidTransformationException(msg: String) extends Exception(msg)

trait DatasetFilter extends DebugEnhancedLogging {
  val targetIndex: TargetIndex
  val allowOriginalAndOthers: Boolean = false
  private val invalidStateKey = "5: invalid state"

  def violations(emd: EasyMetadataImpl, ddm: Node, amd: Node, fedoraIDs: Seq[String] = Seq.empty, fileInfos: List[FileInfo] = List.empty): Try[Option[String]] = {
    val maybeDoi = Option(emd.getEmdIdentifier.getDansManagedDoi)
    val mixOfOriginalAndOthers = allowOriginalAndOthers || !fileInfos.hasOriginalAndOthers
    val triedMaybeInTargetResponse: Try[Option[String]] = maybeDoi
      .map(targetIndex.getByDoi)
      .getOrElse(Success(None)) // no DOI => no bag found by DOI
    val violations = Seq(
      "1: DANS DOI" -> (if (maybeDoi.isEmpty) Seq("not found")
                        else Seq[String]()),
      "2: has jump off" -> fedoraIDs.filter(_.startsWith("dans-jumpoff:")),
      "3: invalid title" -> Option(emd.getEmdTitle.getPreferredTitle)
        .filter(title => forbiddenTitle(title)).toSeq,
      invalidStateKey -> findInvalidState(amd),
      "6: DANS relations" -> findDansRelations(ddm).map(_.toOneLiner),
      "7: is in the vault" -> triedMaybeInTargetResponse.getOrElse(None).toSeq,
      "8: original and other files" -> (if (mixOfOriginalAndOthers) Seq.empty
                                        else Seq("should not occur both")),
    ).filter(_._2.nonEmpty).toMap

    violations.foreach { case (rule, violations) =>
      violations.foreach(s => logger.warn(mockFriendly(s"violated $rule $s")))
    }

    triedMaybeInTargetResponse.map { _ =>
      if (violations.isEmpty) None
      else Some(violations.map {
        case (k, v) if k == invalidStateKey => k + v.mkString(" (", ", ", ")")
        case (k, _) => k
      }.mkString("Violates ", "; ", ""))
    }
  }

  def forbiddenTitle(title: String): Boolean

  private def findInvalidState(amd: Node) = {
    val seq = amd \ "datasetState"
    if (seq.isEmpty) Seq("not found")
    else seq
      .withFilter(node => !(node.text == "PUBLISHED"))
      .map(_.text)
  }

  def findDansRelations(ddm: Node): Seq[Node] = {
    val dcmi = ddm \ "dcmiMetadata"
    Seq(
      (dcmi \ "isVersionOf").theSeq,
      (dcmi \ "hasVersion").theSeq,
      (dcmi \ "replaces").theSeq,
      (dcmi \ "isReplacedBy").theSeq,
    ).flatten
      .filter(hasDansId)
  }

  private def hasDansId(node: Node): Boolean = {
    // see both DDM.toRelationXml methods for what might occur
    (node \@ "href", node.text) match {
      case (href, _) if EmdVersionInfo.isDansId(href) => true
      case (_, text) if EmdVersionInfo.isDansId(text) => true
      case _ => false
    }
  }
}
