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

import org.apache.maven.tools.plugin.extractor.MojoDescriptorExtractor
import org.apache.maven.tools.plugin.{DefaultPluginToolsRequest, PluginToolsRequest}
import org.apache.maven.project.MavenProject
import org.apache.maven.plugin.descriptor.{PluginDescriptorBuilder, MojoDescriptor, PluginDescriptor}
import org.slf4j.LoggerFactory
import collection.JavaConversions._
import org.apache.maven.repository.RepositorySystem
import org.apache.maven.artifact.repository.{ArtifactRepository, RepositoryRequest}
import org.apache.maven.artifact.resolver.ArtifactResolutionRequest
import org.apache.maven.model.Dependency
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.resolver.filter.ScopeArtifactFilter
import java.io.{InputStreamReader, File}
import scalax.io.Resource
import org.apache.maven.tools.plugin.util.PluginUtils
import org.codehaus.plexus.component.annotations.{Requirement, Component}

object ScalaMojoDescriptorExtractor {
  final val ROLE = classOf[MojoDescriptorExtractor]
  final val ROLE_HINT = "java-annotations-and-scaladoc"
}
/**
 * MojoDescriptorExtractor implementation that delegates to the OOTB JavaAnnotationsExtractor
 * (<code>&lt;extractor&gt;java-annotations&lt;/extractor&gt;</code>) to extract
 * all the details it can from the standard java annotations, which are retained
 * in the .class files after compilation, and then decorates those extracted descriptors
 * with the details that can only be found in the javadoc/scaladoc comments in the
 * source files, namely: <code>deprecated</code>, <code>since</code>, and most importantly,
 * <code>description</code>
 *
 * @since 1.0
 * @author Mark Adamcin
 */
@Component(role = ScalaMojoDescriptorExtractor.ROLE, hint = ScalaMojoDescriptorExtractor.ROLE_HINT)
class ScalaMojoDescriptorExtractor extends MojoDescriptorExtractor {
  val log = LoggerFactory.getLogger(getClass)

  @Requirement(role = ScalaMojoDescriptorExtractor.ROLE, hint = "java-annotations")
  var javaAnnotationsExtractor: MojoDescriptorExtractor = null

  @deprecated
  def execute(project: MavenProject, pluginDescriptor: PluginDescriptor): java.util.List[MojoDescriptor] = {
    execute(new DefaultPluginToolsRequest(project, pluginDescriptor))
  }

  def execute(request: PluginToolsRequest): java.util.List[MojoDescriptor] = {
    val descriptors = javaAnnotationsExtractor.execute(request).toList

    val sourceFiles = (for {
      root <- getSourceRoots(Option(request.getProject))
      if ((new File(root.toString)).isDirectory)
      source <- PluginUtils.findSources(root.toString, "**/*.scala")
    } yield root + java.io.File.separator + source).toList

    useCompiler(request, sourceFiles, descriptors)
  }

  def getSourceRoots(p: Option[MavenProject]): List[String] = {
    val roots = p match {
      case None => Nil
      case Some(project) => {
        val baseRoots = project.getCompileSourceRoots.toList
        Option(project.getExecutionProject) match {
          case Some(exProject) => {
            if (exProject != project) getSourceRoots(Some(exProject)) else baseRoots
          }
          case None => baseRoots
        }
      }
    }

    roots
  }

  def useCompiler(request: PluginToolsRequest, sourceFiles: List[String], descriptors: List[MojoDescriptor]): java.util.List[MojoDescriptor] = {
    val compiler = new ScalaDocExtractorCompiler(request)
    val decorator = compiler.extractDescriptorDecorator(sourceFiles)
    descriptors.map { decorator }
  }
}