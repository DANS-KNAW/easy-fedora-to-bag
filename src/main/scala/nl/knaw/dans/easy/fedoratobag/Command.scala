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
package nl.knaw.dans.easy.fedoratobag

import better.files.File
import nl.knaw.dans.easy.fedoratobag.OutputFormat._
import nl.knaw.dans.easy.fedoratobag.TransformationType._
import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.rogach.scallop.ScallopOption

import scala.language.reflectiveCalls
import scala.util.Try
import scala.util.control.NonFatal

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  private val configuration = Configuration(File(System.getProperty("app.home")))
  private val app = new EasyFedoraToBagApp(configuration)
  private val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }

  private val europeana = commandLine.europeana()
  private val csvLogFile = commandLine.logFile()

  runSubcommand(app)
    .doIfSuccess(msg => println(s"OK: $msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(app: EasyFedoraToBagApp): Try[FeedBackMessage] = {
    val isAip = commandLine.outputFormat.isSupplied && commandLine.outputFormat() == AIP
    Try(commandLine.transformation() match {
      case ORIGINAL_VERSIONED if !isAip => SimpleDatasetFilter(allowOriginalAndOthers = true)
      case THEMA if isAip => ThemaDatasetFilter(allowOriginalAndOthers = europeana, targetIndex = app.bagIndex)
      case SIMPLE if isAip => SimpleDatasetFilter(allowOriginalAndOthers = europeana, targetIndex = app.bagIndex)
      case SIMPLE => SimpleDatasetFilter(allowOriginalAndOthers = europeana)
      case _ => throw new NotImplementedError(s"${ commandLine.args } not implemented")
    }).flatMap { datasetFilter =>
      val printer = CsvRecord.printer(csvLogFile)
      printer(app.createExport2(
        commandLine.datasetId.map(di => List(InputFileRecord(di))).getOrElse(loadInputFile(commandLine.inputFile()).get),
        commandLine.skipDatasets.toOption.map(skipDatasets => readDatasetIds(skipDatasets).toSeq).getOrElse(Seq.empty),
        commandLine.outputDir(),
        Options(datasetFilter, commandLine.transformation(), commandLine.strictMode(), europeana, commandLine.noPayload(), commandLine.cutoff()),
        commandLine.outputFormat(),
      ))
    }
  }.map(msg => s"$msg, for details see ${ csvLogFile.toJava.getAbsolutePath }")

  private def readDatasetIds(file: File) = {
    file
      .lineIterator
      .filterNot(line => line.startsWith("#") || line.trim.isEmpty)
  }
}
