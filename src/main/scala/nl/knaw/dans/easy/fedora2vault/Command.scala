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

import better.files.File
import nl.knaw.dans.easy.fedora2vault.TransformationType._
import nl.knaw.dans.easy.fedora2vault.check._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  private val configuration = Configuration(File(System.getProperty("app.home")))
  private val app = new EasyFedora2vaultApp(configuration)
  private val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }

  runSubcommand(app)
    .doIfSuccess(msg => println(s"OK: $msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(app: EasyFedora2vaultApp): Try[FeedBackMessage] = {
    lazy val ids = commandLine
      .datasetId.map(Iterator(_))
      .getOrElse(commandLine.inputFile()
        .lineIterator
        .filterNot(_.startsWith("#"))
      )
    lazy val strict = commandLine.strictMode()
    lazy val writer = commandLine.logFile().newFileWriter(append = true)
    lazy val outputDir = commandLine.outputDir()

    commandLine.transformation() match {
      case SIMPLE_SIP =>
        // TODO construct a SIP in a staging directory, on success move to outputDir
        Failure(new NotImplementedError(s"transformation type $SIMPLE_SIP is not implemented"))
      case SIMPLE_AIP => app
        .simpleTransForms(ids, outputDir, strict, writer)(SimpleChecker(app.bagIndex))
      case THEMA => app
        .simpleTransForms(ids, outputDir, strict, writer)(ThemaChecker(app.bagIndex))
    }
  }.map(msg => s"$msg, for details see ${ commandLine.logFile().toJava.getAbsolutePath }")
}
