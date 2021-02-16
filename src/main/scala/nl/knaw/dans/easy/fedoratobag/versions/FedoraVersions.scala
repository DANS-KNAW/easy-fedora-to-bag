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
package nl.knaw.dans.easy.fedoratobag.versions

import com.yourmediashelf.fedora.client.FedoraClientException
import nl.knaw.dans.easy.fedoratobag.{ DatasetId, FedoraProvider }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import scala.xml.XML

case class FedoraVersions(fedoraProvider: FedoraProvider) extends DebugEnhancedLogging {
  val resolver: Resolver = Resolver()

  /* a submission date for each ID */
  private type Family = mutable.Map[DatasetId, Long]

  private type Members = mutable.ListBuffer[String]

  private val families: mutable.ListBuffer[Family] = mutable.ListBuffer[Family]()

  /* 3 entries for each member of any family: a URN, DOI and DatasetId */
  private val collectedIds = new Members()

  def findChains(ids: Iterator[DatasetId]): Try[Seq[Seq[String]]] = {
    for {
      _ <- ids
        .withFilter(!collectedIds.contains(_))
        .map(findVersions)
        .filter {
          case Failure(e: FedoraClientException) if e.getStatus == 404 =>
            logger.error(e.getMessage)
            false
          case _ => true
        }.find(_.isFailure)
        .getOrElse(Success(()))
      chains = families.map(_.toSeq
        .sortBy { case (_, date) => date }
        .map { case (id, _) => id }
      )
    } yield chains
  }

  private def findVersions(startDatasetId: DatasetId): Try[Members] = {
    val family: Family = mutable.Map[DatasetId, Long]()

    /* dataset IDs (no URN/DOI) of members related to this family found in other families */
    val connections = new Members()

    def connect(): Unit = {
      val connectedWith = families.filter(family =>
        family.keySet.intersect(connections.toSet).nonEmpty
      ) // not inline: changing while searching might require a specific order
      connectedWith.foreach { oldFamily =>
        logger.info(s"$startDatasetId family merged with $oldFamily")
        families -= oldFamily
        family ++= oldFamily
      }
    }

    def readVersionInfo(anyId: String): Try[EmdVersionInfo] = for {
      datasetId <- resolver.getDatasetId(anyId)
      emd <- fedoraProvider
        .datastream(datasetId, "EMD")
        .map(XML.load)
        .tried
      versionInfo <- EmdVersionInfo(emd)
      _ = family += datasetId -> versionInfo.submitted
      _ = collectedIds ++= (versionInfo.self :+ datasetId).distinct
    } yield versionInfo

    def follow(ids: Seq[String], f: EmdVersionInfo => Seq[String]): Try[Unit] = {
      val grouped = ids.groupBy(collectedIds.contains(_))

      connections ++= grouped.get(true).toSeq.flatten
        .map(resolver.getDatasetId(_).unsafeGetOrThrow)
        .filterNot(family.contains)

      // recursion
      grouped.get(false).toSeq.flatten.map { id =>
        for {
          versionInfo <- readVersionInfo(id)
          _ = logger.info(s"$startDatasetId following $versionInfo")
          _ <- follow(f(versionInfo), f)
        } yield ()
      }.find(_.isFailure).getOrElse(Success(()))
    }

    def log(): Unit = {
      val msg = family.mkString(
        s"$startDatasetId Family[${ family.size }]: ",
        ", ",
        connections.mkString(
          s" Connections[${ connections.size }]: ",
          ", ",
          ""
        )
      )
      if (family.values.exists(_ <= 0))
        logger.warn(msg)
      else logger.info(msg)
    }

    for {
      emdVersionInfo <- readVersionInfo(startDatasetId)
      _ = logger.info(s"$startDatasetId $emdVersionInfo")
      _ <- follow(emdVersionInfo.previous, _.previous)
      _ <- follow(emdVersionInfo.next, _.next)
      _ = log()
      _ = if (connections.nonEmpty) connect()
      _ = logger.info(s"$startDatasetId new family $family")
      _ = families += family
    } yield connections
  }
}
