package nl.knaw.dans.easy.fedoratobag.fixture

import java.net.URI

import better.files.File
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.{ Configuration, DatasetId, DatasetInfo, EasyFedoraToBagApp, Options, VersionInfo }
import org.scalamock.scalatest.MockFactory

import scala.util.Try

trait DelegatingApp extends MockFactory {

  /* delegate most of createBag to a mock to test the rest of the class and/or application */
  def delegatingApp(staging: File, createBagExpects: Seq[(String, Try[DatasetInfo])]): EasyFedoraToBagApp = new EasyFedoraToBagApp(
    new Configuration("testVersion", null, null, new URI(""), staging, null)
  ) {
    // mock requires a constructor without parameters
    class MockEasyFedoraToBagApp() extends EasyFedoraToBagApp(null)

    private val delegate = mock[MockEasyFedoraToBagApp]
    createBagExpects.foreach { case (id, result) =>
      (delegate.createBag(_: DatasetId, _: File, _: Options, _: Option[VersionInfo])
        ) expects(id, *, *, *) returning result
    }

    override def createBag(datasetId: DatasetId, bagDir: File, options: Options, firstVersionInfo: Option[VersionInfo] = None): Try[DatasetInfo] = {
      // mimic a part of the real method, the tested caller wants to move the bag
      DansV0Bag.empty(bagDir).map { bag =>
        firstVersionInfo.foreach(_.addVersionOf(bag))
        bag.save()
      }.getOrElse(s"mock of createBag failed for $datasetId")
      // mock the outcome of the method
      delegate.createBag(datasetId, bagDir, options, firstVersionInfo)
    }
  }
}
