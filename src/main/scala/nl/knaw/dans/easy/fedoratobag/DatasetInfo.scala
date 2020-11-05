package nl.knaw.dans.easy.fedoratobag

case class DatasetInfo(
                        maybeFilterViolations: Option[String],
                        doi: String,
                        depositor: Depositor,
                        nextFileInfos: Seq[FileInfo],
                      ){

}
