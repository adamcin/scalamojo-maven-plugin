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

/**
 *
 * @version $Id: ScalaMojoDescriptorExtractor.java$
 * @author madamcin
 */
class ScalaMojoDescriptorExtractor extends MojoDescriptorExtractor {
  val log = LoggerFactory.getLogger(getClass)

  /**
   * bound requirement
   */
  var javaAnnotationsExtractor: MojoDescriptorExtractor = null

  var repositorySystem: RepositorySystem = null

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

    if (request.getDependencies.isEmpty || sourceFiles.isEmpty) {
      getExistingDescriptor(request.getProject) match {
        case Some(pluginDescriptor) => pluginDescriptor.getMojos
        case None => descriptors
      }
    } else {
      val compiler = new ScalaDocExtractorCompiler(request)
      val decorator = compiler.extractDescriptorDecorator(sourceFiles)

      descriptors.map { decorator }
    }
  }

  def getExistingDescriptor(project: MavenProject): Option[PluginDescriptor] = {
    val pluginFile = new File(project.getBuild.getOutputDirectory, "META-INF/maven/plugin.xml")
    log.info("[getExistingDescriptor] pluginFile={}", pluginFile)

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
          descriptor.getMojos.foreach { log.info("[getExistingDescriptor] mojo={}", _) }
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