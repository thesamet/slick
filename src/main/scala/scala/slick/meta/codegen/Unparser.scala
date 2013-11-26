package scala.slick.meta.codegen
import scala.reflect.api.Universe
import scala.sprinter.printers.PrettyPrinters

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import java.io._

/**
 * Unparser creating source code from Scala abstract syntax trees.
 * Wraps sprinter and adapts it to work with any Universe, not only
 * compiler trees.
 * Currently comes with a Scala compiler dependency (inherited from sprinter).
 */
object Unparser {
  private val global: Global = {
    val settings = new Settings()

    val COLON = System getProperty "path.separator"

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _ => System.getProperty("sun.boot.class.path")
    }

    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""

    val reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
    new Global(settings, reporter)
  }

  private val printers = PrettyPrinters(global)

  /** Unparse a given Scala tree to a Scala source code String */
  def unparse(tree: Universe#Tree): String = {
    printers.show(tree.asInstanceOf[Global#Tree], PrettyPrinters.AFTER_NAMER)
  }
}