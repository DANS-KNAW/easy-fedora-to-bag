package nl.knaw.dans.easy.fedora2vault.fixture

import nl.knaw.dans.easy.fedora2vault.FedoraProvider
import org.scalamock.scalatest.MockFactory

import scala.util.Success

trait AudienceSupport  extends MockFactory{
  def expectedAudiences(map: Map[String,String])
                       (implicit fedoraProvider: FedoraProvider): Unit = {
    map.foreach { case (key, value) =>
      (fedoraProvider.loadFoXml(_: String)) expects key once() returning Success(
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
      </foxml:digitalObject>,
      )
    }
  }
}
