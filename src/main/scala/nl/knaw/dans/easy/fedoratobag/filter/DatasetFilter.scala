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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl

import scala.util.{ Success, Try }
import scala.xml.Node

case class InvalidTransformationException(msg: String) extends Exception(msg)

trait DatasetFilter extends DebugEnhancedLogging {
  val targetIndex: TargetIndex

  def violations(emd: EasyMetadataImpl, ddm: Node, amd: Node, fedoraIDs: Seq[String]): Try[Option[String]] = {
    val maybeDoi = Option(emd.getEmdIdentifier.getDansManagedDoi)
    val triedMaybeVaultResponse: Try[Option[String]] = maybeDoi
      .map(targetIndex.getByDoi)
      .getOrElse(Success(None)) // no DOI => no bag found by DOI
    val invalidRightsKey = "4: invalid rights"
    val invalidStateKey = "5: invalid state"
    val keysWithValues = Seq(invalidRightsKey, invalidStateKey)
    val violations = Seq(
      "1: DANS DOI" -> (if (maybeDoi.isEmpty) Seq("not found")
                        else Seq[String]()),
      "2: has jump off" -> fedoraIDs.filter(_.startsWith("easy-jumpoff:")),
      "3: invalid title" -> Option(emd.getEmdTitle.getPreferredTitle)
        .filter(title => forbiddenTitle(title)).toSeq,
      invalidRightsKey -> findInvalidRights(emd),
      invalidStateKey -> findInvalidState(amd),
      "6: DANS relations" -> findDansRelations(ddm),
      "7: is in the vault" -> triedMaybeVaultResponse.getOrElse(None).toSeq,
    ).filter(_._2.nonEmpty).toMap

    violations.foreach { case (rule, violations) =>
      violations.foreach(s => logger.warn(mockFriendly(s"violated $rule $s")))
    }

    triedMaybeVaultResponse.map { _ =>
      if (violations.isEmpty) None
      else Some(violations.map {
        case (k, v) if keysWithValues.contains(k) => k + v.mkString(" (", ", ", ")")
        case (k, _) => k
      }.mkString("Violates ", "; ", ""))
    }
  }

  def forbiddenTitle(title: String): Boolean

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
