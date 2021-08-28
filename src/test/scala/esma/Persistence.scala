package esma

/*import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import esma.model._
import slick.jdbc.H2Profile.api.*

import java.sql.{Date, Timestamp}*/

object Persistence {
/*
  lazy val db = {
    val conf = EsmaConfiguration.dbConfig
    val dsConf = new HikariConfig() {
      setJdbcUrl(conf.url)
      setUsername(conf.user)
      setPassword(conf.password)
      setAutoCommit(true)
    }
    val ds = new HikariDataSource(dsConf)
    Database.forDataSource(ds,None)
  }



  object Tables {

    val reports = TableQuery[TransparencyReportTable]
    val entries = TableQuery[TransparencyDataTable]

    def unapplyReport(rep : TransparencyReport) = Some((rep.reportId, rep.reportType, rep.fileName, rep.created, rep.periodFrom, rep.periodTo))

    class TransparencyReportTable(tag : Tag) extends Table[TransparencyReport](tag, "FITR_REPORT"):
      def reportId = column[Long]("REPORTID", O.AutoInc)
      def reportType = column[String]("REPORTTYPE")
      def fileName = column[String]("FILENAME")
      def created = column[Timestamp]("CRETS")
      def periodFrom = column[Date]("PERIODFROM")
      def periodTo = column[Date]("PERIODTO")
      def * = (reportId, reportType, fileName, created, periodFrom, periodTo) <> ((TransparencyReport.apply _).tupled, unapplyReport)

      def pk = primaryKey("PK_FITR_REPORT", reportId)

    def unapplyData(dat : TransparencyData) = Some((dat.reportId, dat.recordId, dat.instrumentId, dat.fromDate, dat.toDate, dat.liquid, dat.method,
      dat.classification, dat.market, dat.avgDailyTurnover, dat.largeInScale, dat.avgDailyNoOfTrans, dat.standardMarketSize, dat.avgTransactionValue,
    dat.largeInScalePre, dat.largeInScalePost, dat.instrumentSpecificPre, dat.instrumentSpecificPost))

    class TransparencyDataTable(tag : Tag) extends Table[TransparencyData](tag, "FITR_ENTRY") {
      val reportId = column[Long]("REPORTID")
      val recordId = column[Long]("RECORTID")
      val instrumentId = column[String]("SECID")
      val fromDate = column[Option[Date]]("FROMDATE")
      val toDate = column[Option[Date]]("TODATE")
      val liquid =  column[Int]("LIQUID")
      val method = column[String]("METHOD")
      val classification = column[Option[String]]("CLASSIFICATION")
      val market = column[Option[String]]("MARKET")
      val avgDailyTurnover = column[Option[Double]]("AVGDAILYTURNOVER")
      val largeInScale = column[Option[Double]]("LARGEINSCALE")
      val avgDailyNoOfTrans = column[Option[Double]]("AVGDAILYNOTRANS")
      val standardMarketSize = column[Option[Double]]("STDMARKETSIZE")
      val avgTransactionValue = column[Option[Double]]("AVGTRANSVALUE")
      val largeInScalePre = column[Option[Double]]("LARGEINSCALEPRE")
      val largeInScalePost = column[Option[Double]]("LARGEINSCALEPOST")
      val instrumentSpecificPre = column[Option[Double]]("INSSPECPRE")
      val instrumentSpecificPost = column[Option[Double]]("INSSPECPOST")

      def * = (reportId, recordId, instrumentId, fromDate, toDate, liquid, method, classification, market, avgDailyTurnover, largeInScale,
        avgDailyNoOfTrans, standardMarketSize, avgTransactionValue, largeInScalePre, largeInScalePost, instrumentSpecificPre, instrumentSpecificPost) <> ((TransparencyData.apply _).tupled, unapplyData)

      def pk = primaryKey("PK_FITR_ENTRY", reportId)
      def fkReport = foreignKey("FK_FITR_ENTRY_REP", (reportId), reports)(_.reportId, onDelete = ForeignKeyAction.Cascade)

    }


  } */

}
