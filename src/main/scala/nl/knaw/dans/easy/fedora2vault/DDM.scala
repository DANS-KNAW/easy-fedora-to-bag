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

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import nl.knaw.dans.pf.language.emd.types.EmdConstants.DateScheme
import nl.knaw.dans.pf.language.emd.types.{ Author, BasicDate, BasicString, IsoDate }

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

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

      // a null value skips rendering the attribute
      val emdLang: String = emd.getEmdLanguage.getDcLanguage.asScala.headOption.map(_.getValue).orNull // TODO getTerms
      def lang(bs: BasicString) = Option(bs.getLanguage).getOrElse(emdLang)

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
          { emd.getEmdCreator.getDAIAuthors.asScala.map(bs => ???) }
          { emd.getEmdCreator.getDcCreator.asScala.map(bs => ???) }
          { emd.getEmdCreator.getEasCreator.asScala.map(author => <dcx-dai:creatorDetails>{ toXml(author, emdLang)} </dcx-dai:creatorDetails>) }
          { dateCreated.map(node =>  <ddm:created>{ node.text }</ddm:created>) }
          { dateAvailable.map(node =>  <ddm:available>{ node.text }</ddm:available>) }
          { disciplines.map(code =>
          <ddm:audience>{ code }</ddm:audience>) }
          <ddm:accessRights>{ emd.getEmdRights.getAccessCategory }</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          { emd.getEmdIdentifier.getDcIdentifier.asScala.map(bi => <dcterms:identifier xsi:type={ Option(bi.getScheme).map("id-type:" + _).orNull }>{ bi.getValue }</dcterms:identifier>) }
          { emd.getEmdTitle.getTermsAlternative.asScala.map(str => <dcterms:alternative xml:lang={ emdLang }>{ str }</dcterms:alternative>) }
          { /* TODO relations */ }
          { emd.getEmdContributor.getDAIAuthors.asScala.map(bs => ???) }
          { emd.getEmdContributor.getDcContributor.asScala.map(bs => ???) }
          { emd.getEmdContributor.getEasContributor.asScala.map(author => <dcx-dai:contributorDetails>{ toXml(author, emdLang)} </dcx-dai:contributorDetails>) }
          { /* TODO ... */ }
          { dateMap.filter(isOtherDate).map { case (key, values) => values.map(_.withLabel(dateLabel(key))) } }
          { /* TODO ... */ }
        </ddm:dcmiMetadata>
      </ddm:DDM>
    }
  }

  private def toXml(author: Author, lang: String): Seq[Node] = {
    if (Option(author.getSurname).toSeq.filter(!_.isBlank).isEmpty)
      Option(author.getOrganization).toSeq.map(toXml(_, lang, Option(author.getRole)))
    else
      <dcx-dai:author>
        { seq(author.getTitle).map(str => <dcx-dai:titles xml:lang={ lang }>{ str }</dcx-dai:titles>) }
        { seq(author.getInitials).map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { seq(author.getPrefix).map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        { seq(author.getSurname).map(str => <dcx-dai:surname>{ str }</dcx-dai:surname>) }
        { /* TODO  author.getEntityId.map(src => { <label>{ src.value.orEmpty }</label>.withLabel(daiLabel(src.scheme)) })*/ }
        { Option(author.getRole).toSeq.map(role =>  <dcx-dai:role>{ role.getRole /* TODO scheme? */ }</dcx-dai:role>) }
        { Option(author.getOrganization).toSeq.map(toXml(_, lang, maybeRole = None)) }
      </dcx-dai:author>
  }

  private def toXml(value: IsoDate) = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  private def toXml(value: BasicDate) = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  def orNull(dateScheme: DateScheme): String = Option(dateScheme).map("dcterms:"+_.toString).orNull

  private def isOtherDate(kv: (String, Iterable[Elem])) = !Seq("created", "available").contains(kv._1)

  private def dateLabel(key: String) = {
    if (key.isBlank) "dcterms:date"
    else "dcterms:"+key
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

  /** @return an empty Seq for a null or blank String */
  private def seq(s: String): Seq[String] = Option(s).flatMap(_.trim.toOption).toSeq

  private def toXml(organization: String, lang: String, maybeRole: Option[Author.Role]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name xml:lang={ lang }>{ organization }</dcx-dai:name> }
        { maybeRole.toSeq.map(role => <dcx-dai:role>{ role.getRole }</dcx-dai:role>) }
      </dcx-dai:organization>

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
