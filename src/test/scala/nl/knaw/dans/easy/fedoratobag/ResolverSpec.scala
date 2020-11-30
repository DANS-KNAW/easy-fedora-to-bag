package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import scala.util.{ Failure, Success }

class ResolverSpec extends TestSupportFixture {
  "getDatasetId" should "find doi" in {
    Resolver.getDatasetId("10.17026/dans-zjf-522e") shouldBe
      Success("easy-dataset:34340")
  }
  it should "find urn" in {
    Resolver.getDatasetId("urn:nbn:nl:ui:13-2ajw-cq") shouldBe
      Success("easy-dataset:46789")
  }
  it should "not find garbage doi" in {
    Resolver.getDatasetId("10.17026/does-not-exist") should matchPattern {
      case Failure(e) if e.getMessage.startsWith(
        s"Not expected response code from 'https://doi.org/10.17026/does-not-exist' 404"
      ) =>
    }
  }
  it should "not find garbage urn" in {
    Resolver.getDatasetId("urn:nbn:nl:ui:13-does-not-exist") should matchPattern {
      case Failure(e)
        if e.getMessage.startsWith(s"Not expected response code from 'http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-does-not-exist' 200")
        && e.getMessage.contains("""value="urn:nbn:nl:ui:13-does-not-exist"""")
        && e.getMessage.contains("""<li>No locations found with this identifier</li>""") =>
    }
  }
  it should "return input" in {
    Resolver.getDatasetId("easy-dataset:123") shouldBe
      Success("easy-dataset:123")
  }
}
