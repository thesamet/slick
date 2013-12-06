package com.typesafe.slick.docs

import scala.slick.driver.H2Driver.simple._

class MigrationGuide {

  val myDB: Database = null
  implicit val session: Session = null

  {
    //#tabledef
    class Suppliers(tag: Tag) extends Table[(Int, String, String)](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey)
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def * = (id, name, street)
    }
    val suppliers = TableQuery[Suppliers]
    //#tabledef

    //#insert1
    suppliers.map(s => (s.name, s.street)) += ("foo", "bar")
    //#insert1
  }

  {
    //#insert2
    case class Supplier(id: Int, name: String, street: String)

    class Suppliers(tag: Tag) extends Table[Supplier](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey, O.AutoInc)
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      //#mappedprojection
      def * = (id, name, street) <> (Supplier.tupled, Supplier.unapply)
      //#mappedprojection
    }
    val suppliers = TableQuery[Suppliers]

    //#insert2
    val mySupplier: Supplier = null
    //#insert2
    suppliers.insert(mySupplier)
    //#insert2
  }

  {
    class Suppliers(tag: Tag) extends Table[Nothing](tag, "") {
      def * = ???
    }
    //#tablequery
    object suppliers extends TableQuery(new Suppliers(_)) {
      // put extra methods here
    }
    //#tablequery
  }

  {
    //#dynsession
    import scala.slick.jdbc.JdbcBackend.Database.dynamicSession

    myDB withDynSession {
      // use the implicit dynamicSession here
    }
    //#dynsession
  }

  {
    //#session
    myDB withSession { implicit session =>
      // use the implicit session here
    }
    //#session
  }

  {
    //#mappedcolumntype
    case class MyID(value: Int)

    implicit val myIDColumnType =
      MappedColumnType.base[MyID, Int](_.value, new MyID(_))
    //#mappedcolumntype
  }

  {
    //#mappedto
    case class MyID(value: Int) extends MappedTo[Int]

    // No extra implicit required any more
    //#mappedto
  }
}
