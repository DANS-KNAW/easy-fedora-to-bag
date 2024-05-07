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
package nl.knaw.dans.easy.fedoratobag.fixture

import scala.xml.{ Elem, Text }

trait FileFoXmlSupport {

  val digests = Map(
    "acabadabra" -> "4efe30290dd3d4498d66ef569c858cae1cfdb484",
    "lalala" -> "df2efa060e335f97628ca39c9fef5469ab3cb837",
    "rabarbera" -> "1f630a1c539661e70072ea791da18ec600062b93",
    "barbapappa" -> "233e316356c2f4213eb0bf7ca26eec925a3cf214",
  )

  def fileFoXml(id: Int = 35,
                location: String = "original",
                name: String = "something.txt",
                mimeType: String = "text/plain",
                size: Long = 30,
                visibleTo: String = "ANONYMOUS",
                accessibleTo: String = "RESTRICTED_REQUEST",
                digest: String = "dd466d19481a28ba8577e7b3f029e496027a3309",
                creatorRole: String = "DEPOSITOR",
                derivedFrom: Option[Int] = None,
                contentUrl: Option[String] = None
               ): Elem = {
    val contentLocation = contentUrl
      .map(url => <foxml:contentLocation TYPE="URL" REF={ url }/>)
      .getOrElse(<foxml:contentLocation TYPE="INTERNAL_ID" REF={ s"easy-file:$id+EASY_FILE+EASY_FILE.0" }/>)

    <foxml:digitalObject VERSION="1.1" PID={s"easy-file:$id"}
                     xmlns:foxml="info:fedora/fedora-system:def/foxml#"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd">
      <foxml:objectProperties>
          <foxml:property NAME="info:fedora/fedora-system:def/model#state" VALUE="Active"/>
          <foxml:property NAME="info:fedora/fedora-system:def/model#label" VALUE={ name }/>
          <foxml:property NAME="info:fedora/fedora-system:def/model#ownerId" VALUE="user001"/>
          <foxml:property NAME="info:fedora/fedora-system:def/model#createdDate" VALUE="2020-03-17T10:24:17.229Z"/>
          <foxml:property NAME="info:fedora/fedora-system:def/view#lastModifiedDate" VALUE="2020-03-17T10:24:18.118Z"/>
      </foxml:objectProperties>
      <foxml:datastream ID="EASY_FILE" STATE="A" CONTROL_GROUP="M" VERSIONABLE="false">
          <foxml:datastreamVersion ID="EASY_FILE.0" LABEL="" CREATED="2020-03-17T10:24:17.542Z" MIMETYPE={ mimeType } SIZE={ size.toString }>
              <foxml:contentDigest TYPE="SHA-1" DIGEST={ digest }/>
              { contentLocation }
          </foxml:datastreamVersion>
      </foxml:datastream>
      <foxml:datastream ID="EASY_FILE_METADATA" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
          <foxml:datastreamVersion ID="EASY_FILE_METADATA.0" LABEL="" CREATED="2020-03-17T10:24:17.660Z" MIMETYPE="text/xml" SIZE="359">
              <foxml:xmlContent>
                  <fimd:file-item-md xmlns:fimd="http://easy.dans.knaw.nl/easy/file-item-md/" version="0.1">
                      <name>{ name }</name>
                      <path>{s"$location/$name"}</path>
                      <mimeType>{ mimeType }</mimeType>
                      <size>{ size }</size>
                      <creatorRole>{creatorRole}</creatorRole>
                      <visibleTo>{visibleTo}</visibleTo>
                      <accessibleTo>{accessibleTo}</accessibleTo>
                  </fimd:file-item-md>
              </foxml:xmlContent>
          </foxml:datastreamVersion>
      </foxml:datastream>
      <foxml:datastream ID="RELS-EXT" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
          <foxml:datastreamVersion ID="RELS-EXT.2" LABEL="rels-ext" CREATED="2019-04-29T08:51:00.905Z" MIMETYPE="text/xml" FORMAT_URI="info:fedora/fedora-system:FedoraRELSExt-1.0" SIZE="784">
              <foxml:xmlContent>
                  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

                      <rdf:Description rdf:about="info:fedora/easy-file:2707296">
                          <isSubordinateTo xmlns="http://dans.knaw.nl/ontologies/relations#" rdf:resource="info:fedora/easy-dataset:46287"></isSubordinateTo>
                          <isMemberOf xmlns="http://dans.knaw.nl/ontologies/relations#" rdf:resource="info:fedora/easy-folder:142970"></isMemberOf>
                          <hasModel xmlns="info:fedora/fedora-system:def/model#" rdf:resource="info:fedora/easy-model:EDM1FILE"></hasModel>
                          <hasModel xmlns="info:fedora/fedora-system:def/model#" rdf:resource="info:fedora/dans-container-item-v1"></hasModel>
                        { derivedFrom.map(id =>

                          <wasDerivedFrom xmlns="https://www.w3.org/TR/2012/CR-prov-o-20121211/#" rdf:resource={s"info:fedora/easy-file:$id"}></wasDerivedFrom>
                      ).getOrElse(new Text(""))
                        }
                      </rdf:Description>

                  </rdf:RDF>
              </foxml:xmlContent>
          </foxml:datastreamVersion>
      </foxml:datastream>
    </foxml:digitalObject>
  }
}
