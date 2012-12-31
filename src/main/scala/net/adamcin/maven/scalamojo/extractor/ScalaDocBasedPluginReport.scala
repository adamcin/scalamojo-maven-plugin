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

import org.apache.maven.plugin.plugin.PluginReport
import java.util.Locale
import org.apache.maven.plugins.annotations.{ResolutionScope, LifecyclePhase, Execute, Mojo}

import collection.JavaConversions._

/**
 * Maven report goal that overrides the maven-plugin-plugin report goal to limit the active extractors to
 * java-annotations-and-scaldoc.
 *
 * This is generally necessary for reliable scala plugin report generation using this
 * plugin's extractor, since the java-annotations extractor must always be on the classpath alongside it,
 * and the maven-plugin-plugin report does not expose a similar configuration to that of the xdoc goal.
 * @since 1.0
 * @author Mark Adamcin
 */
@Mojo(name = "report", threadSafe = true, requiresDependencyResolution = ResolutionScope.COMPILE)
@Execute(phase = LifecyclePhase.PROCESS_CLASSES)
class ScalaDocBasedPluginReport extends PluginReport {

  override def executeReport(locale: Locale) {
    mojoScanner.setActiveExtractors(Set(ScalaMojoDescriptorExtractor.ROLE_HINT))
    super.executeReport(locale)
  }
}