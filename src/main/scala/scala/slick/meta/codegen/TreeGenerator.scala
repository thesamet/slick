package scala.slick.meta.codegen

import scala.slick.{meta => m}

import scalariform.formatter._
import scala.reflect.runtime.universe._

/**
 * A code generator generating Scala trees
 * using sprinter and scalariform for pretty printing.
 */
class TreeGenerator(model: m.Model)
                   extends AbstractTreeGenerator(model) with OutputHelpers{
  // "Tying the knot": making virtual class concrete
  type Table = TableDef
  def Table = new TableDef(_)
  class TableDef(meta: m.Table) extends super.TableDef(meta)
                                with DefaultColumn
                                with DefaultPrimaryKey
                                with DefaultForeignKey
                                with DefaultIndex

  def code = codeTrees.map(Unparser.unparse).mkString("\n\n")
  override def packageCode(driver: String, pkg: String, obj: String) = {
    try{
      ScalaFormatter.format(super.packageCode(driver, pkg, obj))
    } catch {
      case e:Exception => throw e
    }
  }
}
