package slick
package migration.flyway

import org.scalatest.{ FreeSpec, Matchers }
import slick.driver.H2Driver.api._
import slick.jdbc.JdbcBackend
import slick.migration.api._
import org.flywaydb.core.Flyway
import slick.jdbc.meta.MTable
import slick.migration.flyway.flyway.{VersionedMigration, Resolver, sideEffect}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class FlywayAdapterSpecs extends FreeSpec with Matchers {
  // note, not using capital letters in the table/column names breaks the test
  class TestTable(tag: Tag) extends Table[(Int, Int, Int)](tag, "TESTTABLE") {
    def col1 = column[Int]("COL1")
    def col2 = column[Int]("COL2")
    def col3 = column[Int]("COL3", O.Default(3))
    def * = (col1, col2, col3)
  }

  val testTable = TableQuery[TestTable]

  implicit val dialect = new H2Dialect

  case class DBWrap(name: String) {
    val dbAddress = s"jdbc:h2:mem:$name"//;DB_CLOSE_DELAY=-1"
    val database = Database.forURL(dbAddress, driver = "org.h2.Driver")

    implicit lazy val session = database.createSession

    def executeQuerySync[R](a : slick.dbio.DBIOAction[R, slick.dbio.NoStream, scala.Nothing]) : R = {
      Await.result(database.run(a), Duration.Inf)
    }

    def getTable(name: String)(implicit session: JdbcBackend#Session) =
      executeQuerySync(MTable.getTables(name).map(x => x.toList)).find(_.name.name == name)

    def tableExists() = getTable(testTable.baseTableRow.tableName).isDefined

    def tableContents() = executeQuerySync(testTable.result)
  }

  "The flyway/slick migrations adapter" - {
    "apply slick migrations via a flyway object" in {
      val db = DBWrap("slick_migrate")
      import db._

      implicit lazy val session = db.session

      val m1 = TableMigration(testTable)
        .create
        .addColumns(_.col1, _.col2)

      val m2 = SqlMigration("insert into testtable (col1, col2) values (1, 2)")

      val migration1 = VersionedMigration("1", m1, m2)

     val m3 = TableMigration(testTable)
        .addColumns(_.col3)

      val m4 = SqlMigration("insert into testtable (col1, col2, col3) values (10, 20, 30)")

      val migration2 = VersionedMigration("2", m3, m4)

      val flyway = new Flyway()
      flyway.setDataSource(dbAddress, "", "")
      flyway.setLocations()

      flyway.setResolvers(Resolver(migration1, migration2))

      tableExists() shouldBe false

      flyway.migrate()

      tableExists() shouldBe true
      tableContents() shouldEqual List((1, 2, 3), (10, 20, 30))
    }

    "progressively apply slick migrations via flyway objects" in {
      val db = DBWrap("slick_step_migrate")
      import db._

      val threeColumns = TableMigration(testTable)
        .create
        .addColumns(_.col1, _.col2, _.col3)

      val dataLine1 = SqlMigration("insert into testtable (col1, col2) values (1, 2)")

      val addThreeColumnsAnd1RowOfData = VersionedMigration("1", threeColumns, dataLine1)

      val dataLine2 = SqlMigration("insert into testtable (col1, col2, col3) values (10, 20, 30)")

      val addnotherRow = VersionedMigration("2", dataLine2)

      val flyway1 = new Flyway()
      flyway1.setDataSource(dbAddress, "", "")
      flyway1.setLocations()

      tableExists() shouldBe false

      flyway1.setResolvers(Resolver(addThreeColumnsAnd1RowOfData))
      flyway1 migrate()

      tableExists() shouldBe true
      tableContents() shouldEqual List((1, 2, 3))

      val flyway2 = new Flyway()
      flyway2.setDataSource(dbAddress, "", "")
      flyway2.setLocations()

      flyway2.setResolvers(Resolver(addThreeColumnsAnd1RowOfData, addnotherRow))
      flyway2.migrate()

      tableExists() shouldBe true
      tableContents() shouldEqual List((1, 2, 3), (10, 20, 30))
    }

    "apply general side effects via a flyway object" in {
      val db = DBWrap("side_effect_migrate")
      import db._

      val m1 = sideEffect { implicit s =>
        TableMigration(testTable)
          .create
          .addColumns(_.col1, _.col2)
          .apply()
      }

      val m2 = sideEffect { implicit s =>
        SqlMigration("insert into testtable (col1, col2) values (1, 2)")
          .apply()
      }

      val migration1 = VersionedMigration("1", m1, m2)

      val m3 = sideEffect { implicit s =>
        TableMigration(testTable)
          .addColumns(_.col3)
          .apply()
      }

      val m4 = sideEffect { implicit s =>
        SqlMigration("insert into testtable (col1, col2, col3) values (10, 20, 30)")
          .apply()
      }

      val migration2 = VersionedMigration("2", m3, m4)

      val flyway = new Flyway()
      flyway.setDataSource(dbAddress, "", "")
      flyway.setLocations()

      flyway.setResolvers(Resolver(migration1, migration2))

      tableExists() shouldBe false

      flyway.migrate()

      tableExists() shouldBe true
      tableContents() shouldEqual List((1, 2, 3), (10, 20, 30))
    }
  }
}