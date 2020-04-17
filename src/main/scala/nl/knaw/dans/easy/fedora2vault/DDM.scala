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
package nl.knaw.dans.easy.fedora2vault

import nl.knaw.dans.common.lang.dataset.AccessCategory._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import nl.knaw.dans.pf.language.emd.types.EmdConstants.DateScheme
import nl.knaw.dans.pf.language.emd.types._
import nl.knaw.dans.pf.language.emd.{ EasyMetadataImpl, EmdRights }

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val dansLicense = "http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf"
  val cc0 = "http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf"

  def apply(emdNode: Node)(implicit fedoraProvider: FedoraProvider): Try[Elem] = {

    for {
      emd <- Try(new EmdUnmarshaller(classOf[EasyMetadataImpl]).unmarshal(emdNode.serialize))
      fedoraIDs = emd.getEmdAudience.getDisciplines.asScala.map(_.getValue)
      disciplines <- fedoraIDs.map(getAudience).collectResults
    } yield {
      //    println(new EmdMarshaller(emd).getXmlString)

      val dateMap: Map[String, Iterable[Elem]] = getDateMap(emd)
      val dateCreated = dateMap("created")
      val dateAvailable = {
        val elems = dateMap("available")
        if (elems.isEmpty) dateCreated
        else elems
      }
      <ddm:DDM
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:dcterms="http://purl.org/dc/terms/"
        xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
        xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
        xmlns:gml="http://www.opengis.net/gml"
        xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
        xmlns:ddm={schemaNameSpace}
        xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
        xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
      >
        <ddm:profile>
          { emd.getEmdTitle.getDcTitle.asScala.map(bs => <dc:title xml:lang={ lang(bs) }>{ bs.getValue }</dc:title>) }
          { emd.getEmdDescription.getDcDescription.asScala.map(bs => <dcterms:description xml:lang={ lang(bs) }>{ bs.getValue }</dcterms:description>) }
          { emd.getEmdDescription.getTermsAbstract.asScala.map(bs => <dcterms:description xml:lang={ lang(bs) } descriptionType='Abstract'>{ bs.getValue }</dcterms:description>) }
          { emd.getEmdDescription.getTermsTableOfContents.asScala.map(bs => <dcterms:description xml:lang={ lang(bs) } descriptionType='TableOfContent'>{ bs.getValue }</dcterms:description>) }
          { /* instructions for reuse not specified as such in EMD */ }
          { emd.getEmdCreator.getDAIAuthors.asScala.map(bs => notImplemented("dai creator") /* TODO */) }
          { emd.getEmdCreator.getDcCreator.asScala.map(bs => notImplemented("dc creator")) }
          { emd.getEmdCreator.getEasCreator.asScala.map(author => <dcx-dai:creatorDetails>{ toXml(author)} </dcx-dai:creatorDetails>) }
          { dateCreated.map(node =>  <ddm:created>{ node.text }</ddm:created>) }
          { dateAvailable.map(node =>  <ddm:available>{ node.text }</ddm:available>) }
          { disciplines.map(code => <ddm:audience>{ code }</ddm:audience>) }
          <ddm:accessRights>{ emd.getEmdRights.getAccessCategory }</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          { emd.getEmdIdentifier.getDcIdentifier.asScala.filter(isDdmId).map(bi => <dcterms:identifier xsi:type={ xsiType(bi.getScheme) }>{ bi.getValue }</dcterms:identifier>) }
          { emd.getEmdTitle.getTermsAlternative.asScala.map(str => <dcterms:alternative>{ str }</dcterms:alternative>) }
          { emd.getEmdRelation.getDCRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
          { emd.getEmdRelation.getRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
          { emd.getEmdContributor.getDAIAuthors.asScala.map(bs => notImplemented("dai contributor")) }
          { emd.getEmdContributor.getDcContributor.asScala.map(bs => notImplemented("dc contributor") /* TODO */) }
          { emd.getEmdContributor.getEasContributor.asScala.map(author => <dcx-dai:contributorDetails>{ toXml(author)} </dcx-dai:contributorDetails>) }
          { /* easy-desposit-api creates authors once more as rightsHolders TODO ? */ }
          { emd.getEmdPublisher.getDcPublisher.asScala.map(bs => <dcterms:publisher xml:lang={ lang(bs) }>{ bs.getValue }</dcterms:publisher>) }
          { emd.getEmdSource.getDcSource.asScala.map(bs => <dc:source xml:lang={ lang(bs) }>{ bs.getValue }</dc:source>) }
          { emd.getEmdType.getDcType.asScala.map(bs => <dcterms:type xsi:type={ xsiType(bs.getScheme) }>{ bs.getValue }</dcterms:type>) }
          { emd.getEmdFormat.getDcFormat.asScala.map(bs => <dcterms:format xsi:type={ xsiType(bs.getScheme) }>{ bs.getValue }</dcterms:format>) }
          { emd.getEmdFormat.getTermsExtent.asScala.map(bs => notImplemented("extent format")) }
          { emd.getEmdFormat.getTermsMedium.asScala.map(bs => notImplemented("medium formt")) }
          { emd.getEmdSubject.getDcSubject.asScala.map(bs => notImplemented("dc subjects")) }
          { emd.getEmdCoverage.getDcCoverage.asScala.map(bs => notImplemented("dc coverage")) }
          { emd.getEmdCoverage.getTermsSpatial.asScala.map(bs => notImplemented("spatial coverage")) }
          { emd.getEmdCoverage.getTermsTemporal.asScala.map(bs => notImplemented("temporal coverage")) }
          { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPlace == null).map(spatial => notImplemented("places")) }
          { dateMap.filter(isOtherDate).map { case (key, values) => values.map(_.withLabel(dateLabel(key))) } }
          { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getBox == null).map(spatial => notImplemented("boxes")) }
          { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPoint == null).map(spatial => notImplemented("points")) }
          { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPolygons == null).map(spatial => notImplemented("polygons")) }
          <dcterms:license xsi:type="dcterms:URI">{ toUri(emd.getEmdRights) }</dcterms:license>
          { emd.getEmdLanguage.getDcLanguage.asScala.map(bs => <dcterms:language>{ bs.getValue }</dcterms:language>) }
        </ddm:dcmiMetadata>
      </ddm:DDM>
    }
  }

  private def notImplemented(polygons: DatasetId) = throw new NotImplementedError(polygons)

  /** a null value skips rendering the attribute */
  private def lang(bs: BasicString) = Option(bs.getLanguage).map(_.replace("/", "-")).orNull

  private def toXml(author: Author): Seq[Node] = {
    val surname = author.getSurname
    if (surname == null || surname.trim.isEmpty)
      Option(author.getOrganization).toSeq.map(toXml(_, Option(author.getRole)))
    else
      <dcx-dai:author>
        { seq(author.getTitle).map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>) }
        { seq(author.getInitials).map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { seq(author.getPrefix).map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        <dcx-dai:surname>{ surname }</dcx-dai:surname>
        { seq(author.getEntityId).map(str => { <label>{ str }</label>.withLabel(???) }) /* TODO one by one getIsni/getOrcid/getDai */}
        { Option(author.getRole).toSeq.map(role =>  <dcx-dai:role>{ role.getRole /* TODO scheme? */ }</dcx-dai:role>) }
        { seq(author.getOrganization).map(toXml(_, maybeRole = None)) }
      </dcx-dai:author>
  }

  private def toXml(value: IsoDate) = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  private def toXml(value: BasicDate) = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  def orNull(dateScheme: DateScheme): String = Option(dateScheme).map("dcterms:" + _.toString).orNull

  private def isOtherDate(kv: (String, Iterable[Elem])) = !Seq("created", "available").contains(kv._1)

  private def dateLabel(key: String) = {
    if (key.isBlank) "dcterms:date"
    else "dcterms:" + key
  }

  private def getDateMap(emd: EasyMetadataImpl) = {
    val basicDates = emd.getEmdDate.getAllBasicDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    val isoDates = emd.getEmdDate.getAllIsoDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    (basicDates.toSeq ++ isoDates.toSeq)
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))
  }

  private def getAudience(id: String)(implicit fedoraProvider: FedoraProvider) = {
    fedoraProvider.loadFoXml(id).map(foXml =>
      (foXml \\ "discipline-md" \ "OICode").text
    )
  }

  private def toRelationXml(key: String, rel: Relation) = {
    <label scheme={ relationType(rel) }
           href={ rel.getSubjectLink.toURL.toString }
           xml:lang={ rel.getSubjectTitle.getLanguage }
    >{ rel.getSubjectTitle.getValue }</label>
  }.withLabel(relationLabel("ddm:", key))

  private def toRelationXml(key: String, bs: BasicString) = {
    <label xsi:type={ xsiType(bs.getScheme) }
           xml:lang={ bs.getLanguage }
    >{ bs.getValue }</label>
  }.withLabel(relationLabel("dcterms:", key))

  private def relationType(rel: Relation) = {
    rel.getSubjectLink.getAuthority match {
      case "persistent-identifier.nl" => "id-type:URN"
      case "doi.org" => "id-type:DOI"
      case _ => null
    }
  }

  private def relationLabel(prefix: String, key: String): String = prefix + {
    if (key.isBlank) "relation"
    else key
  }

  private def xsiType(scheme: String) = {
    Option(scheme).map {
      case "DCMI" => "dcterms:DCMIType"
      case s => "id-type:" + s
    }.orNull
  }

  /** @return an empty Seq for a null or blank String */
  private def seq(s: String): Seq[String] = Option(s).flatMap(_.trim.toOption).toSeq

  private def toXml(organization: String, maybeRole: Option[Author.Role]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name>{ organization }</dcx-dai:name> }
        { maybeRole.toSeq.map(role => <dcx-dai:role>{ role.getRole }</dcx-dai:role>) }
      </dcx-dai:organization>

  private def toUri(emdRights: EmdRights) = {
    emdRights.getTermsLicense.asScala
      .find(_.getValue.startsWith("http"))
      .getOrElse(emdRights.getAccessCategory match {
        case ANONYMOUS_ACCESS | OPEN_ACCESS | FREELY_AVAILABLE | OPEN_ACCESS_FOR_REGISTERED_USERS => cc0
        case _ => dansLicense
      })
  }

  private def isDdmId(bi: BasicIdentifier) = {
    // these ID's are generated by easy-ingest-flow, not converted from DDM
    !Seq("PID", "DMO_ID").contains(bi.getScheme)
  }

  /** @param elem XML element to be adjusted */
  private implicit class RichElem(val elem: Elem) extends AnyVal {
    /** @param str the desired label, optionally with name space prefix */
    def withLabel(str: String): Elem = {
      str.split(":", 2) match {
        case Array(label) if label.nonEmpty => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
      }
    }
  }
}
