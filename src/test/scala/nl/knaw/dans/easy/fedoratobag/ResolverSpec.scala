package nl.knaw.dans.easy.fedoratobag

import java.net.{ SocketTimeoutException, UnknownHostException }

import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import scala.util.{ Failure, Success }

class ResolverSpec extends TestSupportFixture {

  "getDatasetId" should "return input" in {
    Resolver.getDatasetId("easy-dataset:123") shouldBe
      Success("easy-dataset:123")
  }
  it should "find doi" in {
    Resolver.getDatasetId("10.17026/dans-zjf-522e") match {
      case Success(s) => s shouldBe "easy-dataset:34340"
      case Failure(e) => assume(serviceAvailable(e))
    }
  }

  it should "find urn" in {
    Resolver.getDatasetId("urn:nbn:nl:ui:13-2ajw-cq") match {
      case Success(s) => s shouldBe "easy-dataset:46789"
      case Failure(e) => assume(serviceAvailable(e))
    }
  }
  it should "not find garbage doi" in {
    val doi = "10.17026/does-not-exist"
    Resolver.getDatasetId(doi) match {
      case Success(_) => fail("not expecting success")
      case Failure(e) => assume(serviceAvailable(e))
        e.getMessage should startWith(
          s"Not expected response code from 'https://doi.org/$doi' 404"
        )
    }
  }
  it should "not find garbage urn" in {
    val urn = "urn:nbn:nl:ui:13-does-not-exist"
    Resolver.getDatasetId(urn) match {
      case Success(_) => fail("not expecting success")
      case Failure(e) => assume(serviceAvailable(e))
        e.getMessage should startWith(
          s"Not expected response code from 'http://www.persistent-identifier.nl/?identifier=$urn' 200"
        )
    }
  }

  private def serviceAvailable(e: Throwable) = {
    !e.isInstanceOf[UnknownHostException] &&
      !e.isInstanceOf[SocketTimeoutException]
  }
}
