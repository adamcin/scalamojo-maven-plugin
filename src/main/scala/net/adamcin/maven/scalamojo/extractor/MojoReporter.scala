package net.adamcin.maven.scalamojo.extractor

import tools.nsc.reporters.AbstractReporter
import tools.nsc.util.{NoPosition, Position}
import org.apache.maven.plugin.logging.Log
import tools.nsc.reporters
import org.slf4j.LoggerFactory
import tools.nsc.doc.Settings

/**
 *
 * @version $Id: MojoReporter.java$
 * @author madamcin
 */
class MojoReporter(val settings: Settings, val quiet: Boolean) extends AbstractReporter {
  val log = LoggerFactory.getLogger(getClass)

  override def hasErrors = false

  def display(pos: Position, msg: String, severity: Severity) {
    val posIn =
      if (pos eq null) {
        NoPosition
      } else if (pos.isDefined) {
        pos.inUltimateSource(pos.source)
      } else {
        pos
      }

    if (!quiet) {
      severity match {
        case INFO => log.info(msg)
        case WARNING => log.warn(msg)
        case ERROR => log.error(msg)
      }
    }
  }

  def displayPrompt() {
    log.info("[displayPrompt]")
  }
}