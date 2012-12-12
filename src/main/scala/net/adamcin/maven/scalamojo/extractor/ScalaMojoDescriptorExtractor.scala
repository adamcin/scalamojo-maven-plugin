package net.adamcin.maven.scalamojo.extractor

import org.apache.maven.tools.plugin.extractor.MojoDescriptorExtractor
import org.apache.maven.tools.plugin.PluginToolsRequest
import org.apache.maven.project.MavenProject
import org.apache.maven.plugin.descriptor.{MojoDescriptor, PluginDescriptor}
import java.io.File
import tools.nsc.doc.{Settings, DocFactory}
import tools.nsc.reporters.ConsoleReporter
import tools.nsc.util.FakePos
import java.util.Collections

/**
 *
 * @version $Id: ScalaMojoDescriptorExtractor.java$
 * @author madamcin
 */
class ScalaMojoDescriptorExtractor extends MojoDescriptorExtractor {

  @deprecated
  def execute(p1: MavenProject, p2: PluginDescriptor) = null

  def execute(request: PluginToolsRequest): java.util.List[MojoDescriptor] = {
    val sources = new File(request.getProject.getBuild.getSourceDirectory)

    var reporter: MojoReporter = null
    val docSettings = new Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"))
    reporter = new MojoReporter(docSettings)

    //new DocFactory(reporter, docSettings) document command.files

    Collections.emptyList[MojoDescriptor]
  }
}