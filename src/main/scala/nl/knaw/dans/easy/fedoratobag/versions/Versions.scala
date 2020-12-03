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

import nl.knaw.dans.easy.fedoratobag.{ DatasetId, FedoraProvider }

import scala.collection.mutable
import scala.util.{ Success, Try }
import scala.xml.XML

abstract class Versions() {
  val resolver: Resolver = Resolver()
  val fedoraProvider: FedoraProvider

  private val collectedIds: mutable.ListBuffer[String] = mutable.ListBuffer[String]()

  def findVersions(startDatasetId: DatasetId): Try[mutable.Map[DatasetId,Long]] = {
    val datasetMap = mutable.Map[DatasetId, Long]()

    def readVersionInfo(anyId: String): Try[VersionInfo] = for {
      datasetId <- resolver.getDatasetId(anyId)
      emd <- fedoraProvider
        .datastream(datasetId, "EMD")
        .map(XML.load)
        .tried
      versionInfo <- VersionInfo(emd)
      _ = datasetMap += datasetId -> versionInfo.submitted
      _ = collectedIds ++= (versionInfo.self :+ datasetId).distinct
    } yield versionInfo

    def follow(ids: Seq[String], f: VersionInfo => Seq[String]): Try[Unit] = {
      val freshIds = ids.filter(!collectedIds.contains(_))
      if (freshIds.isEmpty) Success(())
      else freshIds.map { id =>
        for {
          versionInfo <- readVersionInfo(id)
          _ <- follow(f(versionInfo), f)
        } yield ()
      }.find(_.isFailure).getOrElse(Success(()))
    }

    for {
      versionInfo <- readVersionInfo(startDatasetId)
      _ <- follow(versionInfo.previous, _.previous)
      _ <- follow(versionInfo.next, _.next)
    } yield datasetMap
  }
}
