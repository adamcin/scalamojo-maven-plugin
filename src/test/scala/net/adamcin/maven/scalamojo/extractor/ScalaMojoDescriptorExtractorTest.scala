package net.adamcin.maven.scalamojo.extractor

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.slf4j.LoggerFactory

/**
 *
 * @version $Id: ScalaMojoDescriptorExtractorTest.java$
 * @author madamcin
 */
@RunWith(classOf[JUnitRunner])
class ScalaMojoDescriptorExtractorTest extends FunSuite {
  val log = LoggerFactory.getLogger(getClass)

  test ("fake mojo") {
    val ext = new ScalaMojoDescriptorExtractor


  }
}