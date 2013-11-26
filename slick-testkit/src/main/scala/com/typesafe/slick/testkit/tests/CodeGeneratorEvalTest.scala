package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.meta.codegen._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.reflect.runtime.universe._

class CodeGeneratorEvalTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {
    case class Category(id: Int, name: String)
    class Categories(tag: Tag) extends Table[Category](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name) <> (Category.tupled,Category.unapply)
      def idx = index("IDX_NAME",name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "posts") {
      def id = column[Int]("id")
      def title = column[String]("title")
      def category = column[Option[Int]]("category")
      def * = (id, title, category)
      def categoryFK = foreignKey("category_fk", category, categories)(_.id)
    }
    val posts = TableQuery[Posts]


    class TypeTest(tag: Tag) extends Table[(Boolean,Byte,Short,Int,Long,Float,Double,String,java.sql.Date,java.sql.Time,java.sql.Timestamp,java.sql.Blob)](tag, "TYPE_TEST") {
      def Boolean = column[Boolean]("Boolean",O.Default(true))
      def Byte = column[Byte]("Byte")
      def Short = column[Short]("Short")
      def Int = column[Int]("Int",O.Default(5))
      def Long = column[Long]("Long",O.Default(5L))
      //def java_math_BigInteger = column[java.math.BigInteger]("java_math_BigInteger")
      def Float = column[Float]("Float",O.Default(9.999F))
      def Double = column[Double]("Double",O.Default(9.999))
      //def java_math_BigDecimal = column[java.math.BigDecimal]("java_math_BigDecimal")
      def String = column[String]("String",O.Default("someDefaultString"))
      def java_sql_Date = column[java.sql.Date]("java_sql_Date")
      def java_sql_Time = column[java.sql.Time]("java_sql_Time")
      def java_sql_Timestamp = column[java.sql.Timestamp]("java_sql_Timestamp")
      def java_sql_Blob = column[java.sql.Blob]("java_sql_Blob")
      def java_sql_Clob = column[java.sql.Clob]("java_sql_Clob")
      def * = (Boolean,Byte,Short,Int,Long,Float,Double,String,java_sql_Date,java_sql_Time,java_sql_Timestamp,java_sql_Blob)
      def pk = primaryKey("PK", (Int,Long))
    }
    val typeTest = TableQuery[TypeTest]

    val ddl = posts.ddl ++ categories.ddl ++ typeTest.ddl
    //println("-"*80)
    //println(ddl.createStatements.mkString("\n"))
    //println("=====>")
    ddl.create
    val metaModel = tdb.profile.metaModel
    ddl.drop

    val driver = {
      val path = tdb.profile.getClass.toString.drop(6).dropRight(1).split("(\\.|\\#)")
      if(path.length > 1){
        val location = 
          path.tail.dropRight(1).foldLeft( q"${newTermName(path.head)}": Tree )(
            (from, selectee) => q"${from}.${newTermName(selectee)}"
          )
        q"${location}.${newTermName(path.last)}"
      } else {
        q"${newTermName(path.last)}"
      }
    }

    new TreeGenerator(metaModel){
      val codeWithImport = q"""
        import ${driver}.simple._
        object Tables{
          ..$codeTrees
        }

        val ddl = Tables.Posts.ddl ++ Tables.Categories.ddl ++ Tables.TypeTest.ddl
        //println(ddl.createStatements.mkString("\n"))

        val db = Database.forURL(${tdb.url}, driver=${tdb.jdbcDriver})
        db.withSession{ implicit session =>
          ddl.create
          Tables.Categories.insert( Tables.CategoriesRow(1,"cat") )
          Tables.Posts.insertAll(
            Tables.PostsRow(1,"post 1",Some(1)),
            Tables.PostsRow(2,"post 2",Some(1)),
            Tables.PostsRow(3,"post 3",Some(1))
          )
          Tables.Categories.insert( Tables.CategoriesRow(2,"cat") )
          ( Tables.Posts.length.run, Tables.Posts.filter(_.title =!= "post 1").map(_.title).run.toList )
        }
      """
      import scala.reflect.runtime.{currentMirror=>cm}
      import scala.tools.reflect._
      try{
          assertEquals((3,List("post 2","post 3")), cm.mkToolBox().eval(codeWithImport))
      } catch {
        case e:Exception => {println(Unparser.unparse(codeWithImport));throw e}
      }
    }    
  }
}
