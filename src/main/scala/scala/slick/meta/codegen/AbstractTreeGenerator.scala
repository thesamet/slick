package scala.slick.meta.codegen

import scala.slick.{meta => m}
import scala.slick.meta.ForeignKeyAction
import scala.slick.ast.ColumnOption
import scala.reflect.runtime.universe._
import scala.slick.SlickException

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
    q"import scala.slick.meta.ForeignKeyAction"
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
      def options: Iterable[Tree] = {
        import ColumnOption._
        meta.options.map{
          case PrimaryKey     => q"O.PrimaryKey"
          case Default(value) => q"O.Default(${default.get})" // .get is safe here
          case DBType(dbType) => q"O.DBType($dbType)"
          case AutoInc        => q"O.AutoInc"
          case NotNull|Nullable => throw new SlickException( s"[Code generation] Please don't use Nullable or NotNull column options. Use an Option type, respectively the nullable flag in Slick's meta model Column." )
          case o => throw new SlickException( s"[Code generation] Don't know how to render unexpected ColumnOption $o." )
        }
      }

      /** Generates literal represenation of the default value */
      final def default: Option[Tree] = meta.options.collect{
        case ColumnOption.Default(value) =>
          val raw = value match {
            case s:String => q"$s"
            case None     => q"None"
            case v:Int    => q"$v"
            case v:Double => q"$v"
            case _ => throw new SlickException( s"[Code generation] Dont' know how to render default value $value of ${value.getClass}" )
          }
          if(meta.nullable && raw != q"None") q"Some($raw)"
          else raw
      }.headOption
      
      def code = q"val ${newTermName(name)} = column[${tpe}](${meta.name},..$options)"
    }

    class PrimaryKeyDef(meta: m.PrimaryKey) extends super.PrimaryKeyDef(meta){
      def code = q"""val ${newTermName(name)} = primaryKey(${dbName}, ${compound(columns.map(_.name).map(newTermName).map(c=>q"$c"))})"""
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
      def code = q"""val ${newTermName(name)} = foreignKey(${dbName}, ${compound(referencingColumns.map(_.name).map(newTermName).map(c=>q"$c"))}, ${newTermName(referencedTable.tableValueName)})(t => ${compound(referencedColumns.map(_.name).map(c => q"t.${newTermName(c)}"))}, onUpdate=${onUpdate}, onDelete=${onDelete})"""
    }

    class IndexDef(meta: m.Index) extends super.IndexDef(meta){
      def code = q"""val ${newTermName(name)} = index(${dbName}, ${compound(columns.map(_.name).map(newTermName).map(c=>q"$c"))}, unique=${meta.unique})"""
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