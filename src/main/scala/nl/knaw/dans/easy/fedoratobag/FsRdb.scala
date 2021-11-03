/*
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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.{ManagedResource, managed}

import java.sql.{Connection, DriverManager}
import scala.collection.mutable.ListBuffer
import scala.util.Try

class FsRdb(fsrdb: DatabaseConnection) extends DebugEnhancedLogging {

  val fsRdbConnection = newFsRdbConnection

  private def newFsRdbConnection: ManagedResource[Connection] = {
    Class.forName("org.postgresql.Driver")
    managed {
      val connection = DriverManager.getConnection(fsrdb.url, fsrdb.username, fsrdb.password)
      connection.setAutoCommit(false)
      logger.debug("Connected to postgres database")
      connection
    }
  }

  def getSubordinates(identifier: DatasetId): Try[Seq[String]] = {
    fsRdbConnection.map(implicit connection => getSubordinatesFromFsRdb(identifier)).tried
  }

  private def getSubordinatesFromFsRdb(fedoraId: DatasetId)(implicit connection: Connection): Seq[String] = {
    val subordinatesQuery = s"SELECT pid FROM easy_files WHERE dataset_sid = '$fedoraId';"
    val statement = connection.createStatement()
    val resultSet = statement.executeQuery(subordinatesQuery)

    val ids = ListBuffer[String]()
    while (resultSet.next) {
      val id = resultSet.getString("pid")
      ids += id
    }
    ids.toList
  }
}
