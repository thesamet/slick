package scala.slick.meta.codegen

import scala.slick.{meta => m}
import scala.slick.lifted.ForeignKeyAction
import scala.reflect.runtime.universe._

/**
 * Basis for a code generator generating Scala trees.
 */
abstract class AbstractTreeGenerator(model: m.Model)
                   extends AbstractGenerator[Tree](model)
                   with TreeGeneratorHelpers{
  // virtual class pattern
  type Table <: TableDef
  def Table: m.Table => Table

  /** Generates a sequence of all code fragments */
  def codeTrees : Seq[Tree] = Seq(
    q"import scala.slick.lifted.ForeignKeyAction"
  ) ++ tables.flatMap(_.code)

  // Code generators for the different meta model entities
  abstract class TableDef(meta: m.Table) extends super.TableDef(meta){
    // virtual class pattern
    type Column     <: ColumnDef
    type PrimaryKey <: PrimaryKeyDef
    type ForeignKey <: ForeignKeyDef
    type Index      <: IndexDef
    def Column    : m.Column     => Column
    def PrimaryKey: m.PrimaryKey => PrimaryKey
    def ForeignKey: m.ForeignKey => ForeignKey
    def Index     : m.Index      => Index

    // * projection
    def star = {
      val allFields = compound(columns.map(_.name).map(newTermName).map(c=>q"$c"))
      val rhs = 
        if(mappingEnabled)
          q"$allFields <> ($factory, $extractor)"
        else
          allFields
      q"def * = $rhs"
    }

    // mapping
    def mappedType: Tree = q"${newTypeName(entityClassName)}"
    def factory:    Tree = q"${newTermName(entityClassName)}.tupled"
    def extractor:  Tree = q"${newTermName(entityClassName)}.unapply"

    def entityClassCode = {
      val fields = columns.toList.map(c=>
        c.default.map( v =>
          q"val ${newTermName(c.name)}: ${c.tpe} = ${v}"
        ).getOrElse(
          q"val ${newTermName(c.name)}: ${c.tpe}"
        )
      )
      q"case class ${newTypeName(entityClassName)}(..$fields)"
    }

    def tableClassCode = q"""
class ${newTypeName(tableClassName)}(tag: Tag) extends Table[$tpe](tag, ${meta.name.table}){
  ..${tableClassBody.flatten:Seq[Tree]}
}
      """

    def tableValueCode = q"""lazy val ${newTermName(tableValueName)} = TableQuery(new ${newTypeName(tableClassName)}(_))"""

    // generator classes
    class ColumnDef(meta: m.Column) extends super.ColumnDef(meta){
      final def primaryKeyColumnOption = q"O.PrimaryKey"
      final def dbTypeColumnOption = q"O.DBType(${dbTypeWithSize})"
      final def autoIncrementColumnOption = q"O.AutoInc"
      final def defaultValueColumnOption = default.map(d => q"O.Default(${d})")
      
      def default : Option[Tree] = {
        meta.default.collect{
          case Some(v:String) => q"$v"
          case Some(v:Int)    => q"$v"
          case Some(v:Double)  => q"$v"
          case None => q"""${newTermName("None")}"""
        }
      }
      
      def code = q"val ${newTermName(name)} = column[${tpe}](${meta.name},..$options)"
    }

    class PrimaryKeyDef(meta: m.PrimaryKey) extends super.PrimaryKeyDef(meta){
      def code = q"""val ${newTermName(name)} = primaryKey(${meta.name}, ${compound(columns.map(_.name).map(newTermName).map(c=>q"$c"))})"""
    }

    class ForeignKeyDef(meta: m.ForeignKey) extends super.ForeignKeyDef(meta){
      final def ruleString(action: ForeignKeyAction) = action match{
        case ForeignKeyAction.Cascade    => q"ForeignKeyAction.Cascade"
        case ForeignKeyAction.Restrict   => q"ForeignKeyAction.Restrict"
        case ForeignKeyAction.NoAction   => q"ForeignKeyAction.NoAction"
        case ForeignKeyAction.SetNull    => q"ForeignKeyAction.SetNull"
        case ForeignKeyAction.SetDefault => q"ForeignKeyAction.SetDefault"
      }
      
      final def onUpdate: Tree = ruleString(meta.onUpdate)
      final def onDelete: Tree = ruleString(meta.onDelete)
      def code = q"""val ${newTermName(name)} = foreignKey(${meta.name}, ${compound(referencingColumns.map(_.name).map(newTermName).map(c=>q"$c"))}, ${newTermName(referencedTable.tableValueName)})(t => ${compound(referencedColumns.map(_.name).map(c => q"t.${newTermName(c)}"))}, onUpdate=${onUpdate}, onDelete=${onDelete})"""
    }

    class IndexDef(meta: m.Index) extends super.IndexDef(meta){
      def code = q"""val ${newTermName(name)} = index(${meta.name}, ${compound(columns.map(_.name).map(newTermName).map(c=>q"$c"))}, unique=${meta.unique})"""
    }
  }
  // Default source code string generators
  /** Implements Column code generator using ColumnDef */
  trait DefaultColumn extends TableDef{
    type Column = ColumnDef
    def Column = new ColumnDef(_)
  }
  /** Implements PrimaryKey code generator using PrimaryKeyDef */
  trait DefaultPrimaryKey extends TableDef{
    type PrimaryKey = PrimaryKeyDef
    def PrimaryKey = new PrimaryKeyDef(_)
  }
  /** Implements ForeignKey code generator using ForeignKeyDef */
  trait DefaultForeignKey extends TableDef{
    type ForeignKey = ForeignKeyDef  
    def ForeignKey = new ForeignKeyDef(_)
  }
  /** Implements Index code generator using IndexDef */
  trait DefaultIndex extends TableDef{
    type Index = IndexDef  
    def Index = new IndexDef(_)
  }
}

trait TreeGeneratorHelpers extends scala.slick.meta.codegen.GeneratorHelpers[Tree]{
  def docWithCode(comment: Option[String], code:Tree): Tree = code // not available for reflection: comment.map(scala.tools.nsc.ast.DocDef(_,code)).getOrElse(code)
  def toOption(t: Tree) = q"""${newTypeName("Option")}[$t]"""
  def compound(valuesOrTypes: Seq[Tree]): Tree = {
    if( valuesOrTypes.length == 1 ) {
      valuesOrTypes.head
    } else {
      val components = valuesOrTypes.toList
      q"(..${components})"
    }
  }
  def sqlTypeToScala(dbType: Int): Tree = {
    val path = sqlTypeToClass(dbType).split("\\.")
    if(path.length > 1){
      val location = 
        path.tail.dropRight(1).foldLeft( q"${newTermName(path.head)}": Tree )(
          (from, selectee) => q"${from}.${newTermName(selectee)}"
        )
      q"${location}.${newTypeName(path.last)}"
    } else {
      q"${newTypeName(path.last)}"
    }
  }
}