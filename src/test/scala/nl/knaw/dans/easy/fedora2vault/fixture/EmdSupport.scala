package nl.knaw.dans.easy.fedora2vault.fixture

import nl.knaw.dans.easy.fedora2vault._
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.scalatest.Assertions._

import scala.util.Try
import scala.xml.{ Elem, NodeSeq }

trait EmdSupport {
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def parseEmdContent(xml: NodeSeq): EasyMetadataImpl = Try(
    emdUnmarshaller.unmarshal(
        <emd:easymetadata xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/"
                          xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/"
                          xmlns:dct="http://purl.org/dc/terms/"
                          xmlns:dc="http://purl.org/dc/elements/1.1/"
                          emd:version="0.1"
        >{ xml }</emd:easymetadata>.serialize)
  ).getOrElse(fail("could not load test EMD"))

  def emd2ddm(emd: EasyMetadataImpl): Elem = DDM(emd, Seq.empty)
    .getOrElse(fail("could not create DDM from test EMD"))
}
