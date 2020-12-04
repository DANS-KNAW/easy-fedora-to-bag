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
package nl.knaw.dans.easy.fedoratobag

import better.files.File
import nl.knaw.dans.easy.fedoratobag.OutputFormat._
import nl.knaw.dans.easy.fedoratobag.TransformationType._
import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.easy.fedoratobag.versions.Versions
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  private val configuration = Configuration(File(System.getProperty("app.home")))
  private val app = new EasyFedoraToBagApp(configuration)
  private val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }

  runSubcommand(app)
    .doIfSuccess(msg => println(s"OK: $msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(app: EasyFedoraToBagApp): Try[FeedBackMessage] = {
    lazy val ids = commandLine
      .datasetId.map(Iterator(_))
      .getOrElse(commandLine.inputFile()
        .lineIterator
        .filterNot(_.startsWith("#"))
      )
    lazy val outputDir = commandLine.outputDir()
    lazy val europeana = commandLine.europeana()
    lazy val outputFormat = commandLine.outputFormat()
    lazy val printer = CsvRecord.printer(commandLine.logFile())
    commandLine.transformation() match {
      case FEDORA_VERSIONED if commandLine.outputDir.isSupplied =>
        Failure(new NotImplementedError(s"only DRY RUN implemented for $FEDORA_VERSIONED"))
      case FEDORA_VERSIONED if !europeana =>
        new Versions() {
          override val fedoraProvider: FedoraProvider = app.fedoraProvider
        }.findChains(ids).map { families =>
          commandLine.logFile().printLines(families.map(_.mkString(",")))
          s"DRY RUN --- produced IDs of bag sequences per CSV line"
        }
      case ORIGINAL_VERSIONED if outputFormat == SIP && !europeana =>
        printer.apply(app.createExport(ids, outputDir, Options(SimpleDatasetFilter(), commandLine), outputFormat))
      case SIMPLE if outputFormat == SIP =>
        printer.apply(app.createExport(ids, outputDir, Options(SimpleDatasetFilter(), commandLine), outputFormat))
      case SIMPLE if outputFormat == AIP =>
        printer.apply(app.createExport(ids, outputDir, Options(SimpleDatasetFilter(app.bagIndex), commandLine), outputFormat))
      case THEMA if outputFormat == AIP =>
        printer.apply(app.createExport(ids, outputDir, Options(ThemaDatasetFilter(app.bagIndex), commandLine), outputFormat))
      case transformation =>
        Failure(new NotImplementedError(s"${commandLine.args} not implemented"))
    }
  }.map(msg => s"$msg, for details see ${ commandLine.logFile().toJava.getAbsolutePath }")
}
