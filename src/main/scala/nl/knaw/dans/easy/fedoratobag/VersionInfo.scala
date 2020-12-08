package nl.knaw.dans.easy.fedoratobag

import java.util.UUID

import nl.knaw.dans.bag.v0.DansV0Bag

case class VersionInfo(
                        doi: String,
                        urn: String,
                        packageId: UUID,
                      ) {
  def addVersionOf(bag: DansV0Bag): DansV0Bag = {
    bag.withIsVersionOf(packageId)
      // the following keys should match easy-fedora-to-bag
      .addBagInfo("Base-DOI", doi)
      .addBagInfo("Base-URN", urn)
  }
}
object VersionInfo {
  def apply(datasetInfo: DatasetInfo, packageID: UUID): VersionInfo = {
    VersionInfo(datasetInfo.doi, datasetInfo.urn, packageID)
  }
}
