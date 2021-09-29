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

import nl.knaw.dans.easy.fedoratobag.FoXml._

import scala.util.Try
import scala.xml.Node

object AgreementsXml {
  val schemaNameSpace = "http://easy.dans.knaw.nl/schemas/bag/metadata/agreements/"
  val schemaLocation = "https://easy.dans.knaw.nl/schemas/bag/metadata/agreements/2019/09/agreements.xsd"

  def apply(foXml: Node, ldap: Ldap): Try[Node] = {
    for {
      userId <- getOwner(foXml)
      user <- ldap.query(userId)
      submitDate <- getEmdDateSubmitted(foXml)
      accepted <- getEmdLicenseAccepted(foXml).map(_ == "accept")
    } yield
      <agreements
          xmlns={ schemaNameSpace }
          xmlns:dcterms="http://purl.org/dc/terms/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation={ s"$schemaNameSpace $schemaLocation" }>
        <depositAgreement>
          <signerId easy-account={ user.id } email={ user.email }>{ user.name }</signerId>
          <dcterms:dateAccepted>{ submitDate }</dcterms:dateAccepted>
          <depositAgreementAccepted>{ accepted }</depositAgreementAccepted>
        </depositAgreement>
        <personalDataStatement>
          <notAvailable> </notAvailable>
        </personalDataStatement>
      </agreements>
  }
}
