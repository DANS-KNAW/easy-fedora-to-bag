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
import nl.knaw.dans.easy.fedoratobag.versions.FedoraVersions
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

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

  runSubcommand(app)
    .doIfSuccess(msg => println(s"OK: $msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(app: EasyFedoraToBagApp): Try[FeedBackMessage] = {
    lazy val isAip = commandLine.outputFormat.isSupplied && commandLine.outputFormat() == AIP
    Try(commandLine.transformation() match {
      case FEDORA_VERSIONED if !commandLine.europeana() && !isAip => FedoraVersionedFilter()
      case ORIGINAL_VERSIONED if !isAip => SimpleDatasetFilter()
      case THEMA if isAip => ThemaDatasetFilter(app.bagIndex)
      case SIMPLE if isAip => SimpleDatasetFilter(app.bagIndex)
      case SIMPLE => SimpleDatasetFilter()
      case _ => throw new NotImplementedError(s"${ commandLine.args } not implemented")
    }).flatMap { datasetFilter =>
      if (!commandLine.outputDir.isSupplied)
        dryRunFedoraVersioned(app)
      else runExport(app, datasetFilter)
    }
  }.map(msg => s"$msg, for details see ${ commandLine.logFile().toJava.getAbsolutePath }")

  private def dryRunFedoraVersioned(app: EasyFedoraToBagApp) = {
    FedoraVersions(app.fedoraProvider)
      .findChains(datasetIds).map { families =>
      commandLine.logFile().printLines(families.map(_.mkString(",")))
      s"DRY RUN --- produced IDs of bag sequences per CSV line"
    }
  }

  private def runExport(app: EasyFedoraToBagApp, datasetFilter: SimpleDatasetFilter) = {
    val options = Options(datasetFilter, commandLine.transformation(), commandLine.strictMode(), commandLine.europeana())
    val printer = CsvRecord.printer(commandLine.logFile())
    if (commandLine.transformation() == FEDORA_VERSIONED)
      printer.apply(app.createSequences(datasetIds, commandLine.outputDir(), options))
    else printer.apply(app.createExport(datasetIds, commandLine.outputDir(), options, commandLine.outputFormat()))
  }

  private def datasetIds = {
    commandLine
      .datasetId.map(Iterator(_))
      .getOrElse(commandLine
        .inputFile()
        .lineIterator
        .filterNot(_.startsWith("#"))
      )
  }
}
