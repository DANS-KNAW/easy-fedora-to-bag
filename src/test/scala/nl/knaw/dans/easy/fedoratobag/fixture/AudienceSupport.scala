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
package nl.knaw.dans.easy.fedoratobag.fixture

import nl.knaw.dans.easy.fedoratobag.FedoraProvider
import org.scalamock.scalatest.MockFactory

import scala.util.Success

trait AudienceSupport extends MockFactory {
  def expectedAudiences(map: Map[String, String])
                       (implicit fedoraProvider: FedoraProvider): Unit = {
    map.foreach { case (key, value) =>
      (fedoraProvider.loadFoXml(_: String)) expects key once() returning Success(audienceFoXML(key, value))
    }
  }

  def audienceFoXML(key: String, value: String) = {
      <foxml:digitalObject VERSION="1.1" PID={key}
               xmlns:foxml="info:fedora/fedora-system:def/foxml#"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd">
          <foxml:datastream ID="DMD" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
              <foxml:datastreamVersion ID="DMD.0" LABEL="Discipline metadata" CREATED="2020-03-17T06:05:24.686Z" MIMETYPE="text/xml" SIZE="303">
                  <foxml:xmlContent>
                      <dmd:discipline-md xmlns:dmd="http://easy.dans.knaw.nl/easy/discipline-md/">
                          <order>4200</order>
                          <OICode>{ value }</OICode>
                          <Easy1BranchID>twips.dans.knaw.nl--8739414114196558923-1179232222081</Easy1BranchID>                      </dmd:discipline-md>
                  </foxml:xmlContent>
              </foxml:datastreamVersion>
          </foxml:datastream>
      </foxml:digitalObject>
  }
}
