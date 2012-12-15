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
  final val ROLE_HINT = "java-annotations-for-scala"
}
/**
 *
 * @since 1.0
 * @author Mark Adamcin
 */
@Component(role = ScalaMojoDescriptorExtractor.ROLE, hint = ScalaMojoDescriptorExtractor.ROLE_HINT)
class ScalaMojoDescriptorExtractor extends MojoDescriptorExtractor {
  val log = LoggerFactory.getLogger(getClass)

  /**
   * bound requirement
   */

  @Requirement
  var repositorySystem: RepositorySystem = null

  @Requirement(role = ScalaMojoDescriptorExtractor.ROLE, hint = "java-annotations")
  var javaAnnotationsExtractor: MojoDescriptorExtractor = null

  @deprecated
  def execute(project: MavenProject, pluginDescriptor: PluginDescriptor): java.util.List[MojoDescriptor] = {
    execute(new DefaultPluginToolsRequest(project, pluginDescriptor))
  }

  def execute(request: PluginToolsRequest): java.util.List[MojoDescriptor] = {
    val descriptors = javaAnnotationsExtractor.execute(request).toList

    val alwaysCompile = true
    val sourceFiles = (for {
      root <- getSourceRoots(Option(request.getProject))
      if ((new File(root.toString)).isDirectory)
      source <- PluginUtils.findSources(root.toString, "**/*.scala")
    } yield root + java.io.File.separator + source).toList

    if (request.getDependencies.isEmpty || sourceFiles.isEmpty) {
      if (alwaysCompile) {
        log.info("[execute] always compile, sources: {}", sourceFiles)
        resolveDependenciesToRequest(request)
        useCompiler(request, sourceFiles, descriptors)
      } else {
        getExistingDescriptor(request.getProject) match {
          case Some(pluginDescriptor) => {
            log.info("[execute] use existing descriptor")
            pluginDescriptor.getMojos
          }
          case None => descriptors
        }
      }
    } else {
      log.info("[execute] compile normal")
      useCompiler(request, sourceFiles, descriptors)
    }
  }

  def useCompiler(request: PluginToolsRequest, sourceFiles: List[String], descriptors: List[MojoDescriptor]): java.util.List[MojoDescriptor] = {
    val compiler = new ScalaDocExtractorCompiler(request)
    val decorator = compiler.extractDescriptorDecorator(sourceFiles)
    descriptors.map { decorator }
  }

  def getExistingDescriptor(project: MavenProject): Option[PluginDescriptor] = {
    val pluginFile = new File(project.getBuild.getOutputDirectory, "META-INF/maven/plugin.xml")
    log.debug("[getExistingDescriptor] pluginFile={}", pluginFile)

    if (pluginFile.exists) {
      val builder = new PluginDescriptorBuilder

      Resource.fromFile(pluginFile).inputStream.acquireFor {
        (stream) => builder.build(new InputStreamReader(stream, "UTF-8"))
      } match {
        case Left(exs) => {
          log.error("[getExistingDescription] failed to read plugin.xml file: " + pluginFile, exs.head)
          None
        }
        case Right(descriptor) => {
          if (log.isDebugEnabled) {
            descriptor.getMojos.foreach { log.debug("[getExistingDescriptor] mojo={}", _) }
          }
          Option(descriptor)
        }
      }
    } else {
      None
    }
  }

  def resolveDependenciesToRequest(request: PluginToolsRequest) {
    val artifactRequest = new ArtifactResolutionRequest(getRepositoryRequest(request))
    artifactRequest.setResolveTransitively(true)

    def dependencyFilter(dep: Dependency): Boolean = dep.getScope == Artifact.SCOPE_COMPILE
    val artifacts = request.getProject.getDependencies.filter { dependencyFilter }.map {
      (dep: Dependency) => repositorySystem.createDependencyArtifact(dep)
    }

    artifacts.foreach {
      (artifact: Artifact) => {
        artifactRequest.setArtifact(artifact)
        artifactRequest.setCollectionFilter(new ScopeArtifactFilter(Artifact.SCOPE_COMPILE))
        repositorySystem.resolve(artifactRequest)
        request.getProject.getArtifacts.add(artifactRequest.getArtifact)
        Option(artifactRequest.getArtifactDependencies) match {
          case Some(deps) => request.getProject.getArtifacts.addAll(deps)
          case None => ()
        }
      }
    }
  }

  def getRepositoryRequest(request: PluginToolsRequest): RepositoryRequest = {
    new ArtifactResolutionRequest(new RepositoryRequest {
      def getLocalRepository = request.getLocal
      def setRemoteRepositories(remoteRepositories: java.util.List[ArtifactRepository]) = null
      def getRemoteRepositories = request.getRemoteRepos
      def setForceUpdate(forceUpdate: Boolean) = null
      def isOffline = false
      def setLocalRepository(localRepository: ArtifactRepository) = null
      def setOffline(offline: Boolean) = null
      def isForceUpdate = false
    })
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
}