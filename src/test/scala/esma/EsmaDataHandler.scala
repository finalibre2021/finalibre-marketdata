package esma

import doobie.*
import doobie.implicits.*
import doobie.implicits.javasql.*
import doobie.h2.*
import doobie.h2.implicits.*
import doobie.util.ExecutionContexts
import cats.*
import cats.implicits.*
import cats.data.*
import cats.effect.*
import esma.EsmaConfiguration
import esma.model.{TransparencyData, TransparencyReport}

import scala.concurrent.ExecutionContext

object EsmaDataHandler:
  val dbConf = EsmaConfiguration.dbConfig


  given contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val xa = Transactor.fromDriverManager[IO](
    "org.h2.Driver",
    dbConf.url,
    dbConf.user,
    dbConf.password
  )


  object FITR:

    def insertReport(rep : TransparencyReport, entries : Seq[TransparencyData]) : Unit =
      val stat = for
        repId <- insertReportValue(rep)
        _ <- insertEntryValues(entries.map(en => en.copy(reportId = repId)))
      yield repId
      stat.transact(xa).unsafeRunSync()


    def insertReportValue(rep : TransparencyReport) : ConnectionIO[Long] =
      sql"""insert into fitr_report(reporttype, filename, created, periodfrom, periodto)
            values (${rep.reportType}, ${rep.fileName}, ${rep.created}, ${rep.periodFrom}, ${rep.periodTo})"""
        .update
        .withUniqueGeneratedKeys[Long]("REPORTID")

    def insertEntryValues(seq : Seq[TransparencyData]) : ConnectionIO[Int] =
      val stat = """insert into fitr_entry(reportid, recordid, instrumentid, fromdate, todate, liquid, method, classification, market, avgdailyturnover, largeinscale,
                   | avgdailynooftrans, standardmarketsize, avgtransvalue, largeinscalepre, largeinscalepost, instrumentspecificpre, instrumentspecificpost) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""".stripMargin
      Update[TransparencyData](stat).updateMany(seq)

    def loadFileNames : Seq[String] =
      sql"""select filename from fitr_report""".query[String].to[List].transact(xa).unsafeRunSync()


    def createTables() : Unit =
      (createReportTableStat, createEntryTableStat).mapN(_ + _).transact(xa).unsafeRunSync()

    val createReportTableStat =
      sql"""
           |create table FITR_REPORT (
           |  REPORTID identity primary key,
           |  REPORTTYPE varchar(50) not null,
           |  FILENAME varchar(500) not null,
           |  CREATED timestamp not null,
           |  PERIODFROM date not null,
           |  PERIODTO date not null
           |);
           |""".stripMargin.update.run


    val createEntryTableStat =
      sql"""
           |create table FITR_ENTRY (
           |  REPORTID bigint not null,
           |  RECORDID bigint not null,
           |  INSTRUMENTID varchar(50) not null,
           |  FROMDATE date,
           |  TODATE date,
           |  LIQUID int not null,
           |  METHOD varchar(20) not null,
           |  CLASSIFICATION varchar(50),
           |  MARKET varchar(20),
           |  AVGDAILYTURNOVER double,
           |  LARGEINSCALE double,
           |  AVGDAILYNOOFTRANS double,
           |  STANDARDMARKETSIZE double,
           |  AVGTRANSVALUE double,
           |  LARGEINSCALEPRE double,
           |  LARGEINSCALEPOST double,
           |  INSTRUMENTSPECIFICPRE double,
           |  INSTRUMENTSPECIFICPOST double,
           |  foreign key (REPORTID) references FITR_REPORT(REPORTID) on delete cascade
           |);
           |""".stripMargin.update.run

  object DataInvestigation:


    val createFileTableStat =
      sql"""
           |create table DATINV_FIL (
           |  REPORTID identity primary key,
           |  FILENAME varchar(500) not null
           |);
           |""".stripMargin.update.run


    val createDataTableStat =
      sql"""
           |create table DATINV_DATA (
           |  REPORTID bigint not null,
           |  TAG varchar(1500) not null,
           |  TAGVALUE varchar(100) not null,
           |  OCCURS bigint not null,
           |  foreign key (REPORTID) references DATINV_FIL(REPORTID) on delete cascade
           |);
           |""".stripMargin.update.run

    private def insertReportValue(filename : String) : ConnectionIO[Long] =
      sql"""insert into datinv_fil(filename)
            values ($filename)"""
        .update
        .withUniqueGeneratedKeys[Long]("REPORTID")

    private def insertDataValues(values : Seq[(Long, String, String, Long)]) : ConnectionIO[Int] =
      val stat = """insert into datinv_data(reportid, tag, tagvalue, occurs) values (?,?,?,?)""".stripMargin
      Update[(Long, String, String, Long)](stat).updateMany(values)

    def insertFileDate(filename : String, values : Seq[(String, String, Long)]) : Unit =
      val stat = for
        repId <- insertReportValue(filename)
        _ <- insertDataValues(values.map(en => (repId, en._1, if en._2.length > 100 then en._2.substring(0,100) else en._2, en._3)))
      yield repId
      stat.transact(xa).unsafeRunSync()


    def createTables() : Unit =
      (createFileTableStat, createDataTableStat).mapN(_ + _).transact(xa).unsafeRunSync()
