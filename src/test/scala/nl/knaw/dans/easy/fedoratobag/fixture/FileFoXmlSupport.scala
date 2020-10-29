package nl.knaw.dans.easy.fedoratobag.fixture

import nl.knaw.dans.easy.fedoratobag.XmlExtensions

import scala.xml.Elem

trait FileFoXmlSupport {

  def fileFoXml(id: Int = 35,
                location: String = "original",
                name: String = "something.txt",
                mimeType: String = "text/plain",
                size: Long = 30,
                accessibleTo: String = "RESTRICTED_REQUEST"): Elem = {
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
              <foxml:contentDigest TYPE="SHA-1" DIGEST="dd466d19481a28ba8577e7b3f029e496027a3309"/>
              <foxml:contentLocation TYPE="INTERNAL_ID" REF={s"easy-file:$id+EASY_FILE+EASY_FILE.0"}/>
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
                      <creatorRole>DEPOSITOR</creatorRole>
                      <visibleTo>ANONYMOUS</visibleTo>
                      <accessibleTo>{accessibleTo}</accessibleTo>
                  </fimd:file-item-md>
              </foxml:xmlContent>
          </foxml:datastreamVersion>
      </foxml:datastream>
    </foxml:digitalObject>
  }
}
