package src.main.scala

import org.apache.maven.plugin.AbstractMojo
import org.apache.maven.plugins.annotations.Mojo

/**
 * This is a description
 * @since 1.0
 * @author madamcin
 */
@Mojo(name = "fake")
class FakeMojo extends AbstractMojo {

  def execute() {}
}