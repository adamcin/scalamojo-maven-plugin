/*
 * Copyright 2012 Mark Adamcin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.adamcin.maven.scalamojo.extractor

import org.apache.maven.project.MavenProject
import java.io.File
import org.apache.maven.plugin.descriptor.MojoDescriptor
import scala.collection.JavaConversions._

import tools.nsc._
import tools.nsc.reporters._
import doc.{DocFactory, Universe, Settings}
import util.{FakePos, NoPosition}
import org.slf4j.LoggerFactory
import org.apache.maven.tools.plugin.PluginToolsRequest
import org.apache.maven.tools.plugin.util.PluginUtils

/**
 *
 * @since 1.0
 * @author Mark Adamcin
 */
class ScalaDocExtractorCompiler(request: PluginToolsRequest) {
  val log = LoggerFactory.getLogger(getClass)

  def decorate(universe: Option[Universe])(descriptor: MojoDescriptor): MojoDescriptor = {
    if (descriptor.getGoal == "help") {
      descriptor
    } else {
      def findClass(p: doc.model.Package, c: String): Option[doc.model.Class] = {
        val dotIndex = c.indexOf(".")
        if (dotIndex < 0) {
          p.templates.find((t) => t.isClass && t.name == c) match {
            case Some(entity) => Some(entity.asInstanceOf[doc.model.Class])
            case None => None
          }
        } else {
          val pName = c.substring(0, dotIndex)
          p.packages.find((sp) => sp.name == pName) match {
            case Some(sp) => findClass(sp, c.substring(dotIndex + 1, c.length))
            case None => None
          }
        }
      }

      import ScaladocStringer._
      universe match {
        case None => ()
        case Some(u) => findClass(u.rootPackage, descriptor.getImplementation) match {
          case None => ()
          case Some(c) => {
            descriptor.setLanguage("scala")

            c.comment match {
              case None => ()
              case Some(comment) => {
                comment.body.summary match {
                  case None => ()
                  case Some(summary) => {
                    val description = toHtmlString(inlineToHtml(summary))
                    descriptor.setDescription(description)
                  }
                }
                comment.since match {
                  case None => ()
                  case Some(body) => body.summary match {
                    case None => ()
                    case Some(summary) => {
                      val since = toHtmlString(inlineToHtml(summary))
                      descriptor.setSince(since)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    descriptor
  }

  def extractDescriptorDecorator(sourceFiles: List[String]): (MojoDescriptor) => (MojoDescriptor) = {

    val project: Option[MavenProject] = Option(request.getProject)

    def initialize: (Settings, Reporter) = {
      var reporter: Reporter = null
      val docSettings = new doc.Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"))
      docSettings.classpath.value = getClasspath(project)
      docSettings.stop.tryToSetColon(List("constructors"))
      reporter = new MojoReporter(docSettings, quiet = true)
      (docSettings, reporter)
    }

    val (settings, reporter) = initialize

    decorate(new DocFactory(reporter, settings).makeUniverse(sourceFiles))_
  }

  def getClasspath(p : Option[MavenProject]): String = {
    val classpath = p match {
      case None => ""
      case Some(project) => {
        val baseClasspath = project.getCompileClasspathElements.mkString(File.pathSeparator)
        Option(project.getExecutionProject) match {
          case Some(exProject) => {
            if (exProject != project) getClasspath(Some(exProject)) else baseClasspath
          }
          case None => baseClasspath
        }
      }
    }
    classpath
  }

}

