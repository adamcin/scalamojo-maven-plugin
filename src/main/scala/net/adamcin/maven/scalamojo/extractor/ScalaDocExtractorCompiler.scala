/*
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to <http://unlicense.org/>
 */

package net.adamcin.maven.scalamojo.extractor

import org.apache.maven.project.MavenProject
import java.io.File
import org.apache.maven.plugin.descriptor.MojoDescriptor
import scala.collection.JavaConversions._

import tools.nsc._
import tools.nsc.reporters._
import doc.{DocFactory, Universe, Settings}
import util.FakePos
import org.slf4j.LoggerFactory
import org.apache.maven.tools.plugin.PluginToolsRequest

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

      import ScalaDocStringer._
      universe match {
        case None => ()
        case Some(u) => findClass(u.rootPackage, descriptor.getImplementation) match {
          case None => ()
          case Some(c) => {
            descriptor.setLanguage("scala")

            getDeprecated(c) match {
              case None => ()
              case Some(deprecated) => descriptor.setDeprecated(deprecated)
            }

            getDescription(c.comment) match {
              case None => ()
              case Some(description) => descriptor.setDescription(description)
            }

            getSince(c.comment) match {
              case None => ()
              case Some(since) => descriptor.setSince(since)
            }

            val memberMap = c.members.map {
              (entity) => (entity.name, entity)
            }.toMap

            descriptor.getParameters.foreach {
              (param) => {
                memberMap.get(param.getName) match {
                  case None => ()
                  case Some(member) => {
                    getDeprecated(member) match {
                      case None => ()
                      case Some(deprecated) => param.setDeprecated(deprecated)
                    }

                    val inheritor = commentInheritor(member)_
                    inheritor(getDescription) match {
                      case None => ()
                      case Some(description) => param.setDescription(description)
                    }

                    inheritor(getSince) match {
                      case None => ()
                      case Some(since) => param.setSince(since)
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

