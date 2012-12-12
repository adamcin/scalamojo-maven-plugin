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
import org.codehaus.plexus.component.annotations.{Component, Requirement}
import collection.JavaConversions
import org.slf4j.LoggerFactory

/**
 *
 * @version $Id: ScalaMojoDescriptorExtractor.java$
 * @author madamcin
 */
@Component(role = classOf[MojoDescriptorExtractor], hint = "java-annotations-for-scala")
class ScalaMojoDescriptorExtractor extends MojoDescriptorExtractor {
  val log = LoggerFactory.getLogger(getClass)

  /**
   * bound requirement
   */
  @Requirement(role = classOf[MojoDescriptorExtractor], hint = "java-annotations")
  var javaAnnotationsExtractor: MojoDescriptorExtractor = null

  @deprecated
  def execute(project: MavenProject, pluginDescriptor: PluginDescriptor): java.util.List[MojoDescriptor] = {
    val descriptors = javaAnnotationsExtractor.execute(project, pluginDescriptor)
    JavaConversions.collectionAsScalaIterable(descriptors).foreach {
      (descriptor) => log.info("[execute(MavenProject, PluginDescriptor)] Got descriptor: {}", descriptor.getGoal)
    }
    descriptors
  }

  def execute(request: PluginToolsRequest): java.util.List[MojoDescriptor] = {
    val descriptors = javaAnnotationsExtractor.execute(request)
    JavaConversions.collectionAsScalaIterable(descriptors).foreach {
      (descriptor) => log.info("[execute(PluginToolsRequest)] Got descriptor: {}", descriptor.getGoal)
    }
    descriptors
    /*
    val sources = new File(request.getProject.getBuild.getSourceDirectory)

    var reporter: MojoReporter = null
    val docSettings = new Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"))
    reporter = new MojoReporter(docSettings)

    //new DocFactory(reporter, docSettings) document command.files

    Collections.emptyList[MojoDescriptor]
    */
  }
}