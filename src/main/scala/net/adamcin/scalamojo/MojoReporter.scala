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

package net.adamcin.scalamojo

import tools.nsc.reporters.AbstractReporter
import scala.reflect.internal.util.{NoPosition, Position}
import org.slf4j.LoggerFactory
import tools.nsc.doc.Settings

/**
 * Implementation of Reporter for use by DocFactory compiler
 * @since 0.6.0
 * @author Mark Adamcin
 */
class MojoReporter(val settings: Settings, val quiet: Boolean) extends AbstractReporter {
  private val log = LoggerFactory.getLogger(getClass)

  override def hasErrors = false

  def display(pos: Position, msg: String, severity: Severity) {
    val posIn =
      if (pos eq null) {
        NoPosition
      } else if (pos.isDefined) {
        pos.finalPosition
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