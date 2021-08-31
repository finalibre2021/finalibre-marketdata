import esma.EsmaDataHandler
import esma.model.{TransparencyData, TransparencyReport}
import finalibre.marketdata.esma.{FITR as mod}
import scala.util._
import java.io.File
import java.sql.{Date, Timestamp}
import java.time.LocalDate

object ExploreFITRSaving:

  def main(args: Array[String]): Unit =
    val fromDate = LocalDate.of(2021, 1, 1)
    val toDate = LocalDate.of(2021, 12, 12)
    val zipFolder = new File("""c:\temp\fitr""")
    val unZipFilder = new File("""c:\temp\fitr-unzipped""")
    val previouslyProcessed = EsmaDataHandler.FITR.loadFileNames
    val reports = mod.transparencyEntries(zipFolder, unZipFilder, previouslyProcessed)

    reports.foreach {
      case Left(errs) =>
        println("Following errors occured")
        errs.foreach(err => println(err))

      case Right(rep) => Try {
        println(s"Got report from: ${rep.fileName}")
        val loaded = rep.transparencyData()
        val mappedReport = mapReport(rep)
        val mappedEntries = loaded.map(mapData)
        EsmaDataHandler.FITR.insertReport(mappedReport, mappedEntries)
        println(s"Inserted: ${mappedEntries.size} entries for report: ${mappedReport.fileName} (${mappedReport.periodFrom} -> ${mappedReport.periodTo})")
      } match {
        case Success(value) =>
        case Failure(err) => println(s"Error occured: ${err.getMessage}")
      }
    }




    def mapReport(rep : mod.TransparencyReport) : TransparencyReport =
      TransparencyReport(-1L, rep.parseType.toString, rep.fileName, Timestamp.valueOf(rep.created), Date.valueOf(rep.periodFrom), Date.valueOf(rep.periodTo))

    def mapData(dat : mod.TransparencyData) : TransparencyData =
      val from = dat.fromDate.map(d => Date.valueOf(d))
      val to = dat.toDate.map(d => Date.valueOf(d))
      val liq = if(dat.liquid) then 1 else 0

      dat match {
        case mod.EquityTransparencyData(recordId, id, fromDate, toDate, liquid, method, classification, market, avgDailyTurnover, largeInScale, avgDailyNoOfTrans, standardMarketSize, avgTransactionValue) =>
          TransparencyData(-1L, dat.recordId, id, from, to, liq, method, classification, market, avgDailyTurnover, largeInScale, avgDailyNoOfTrans, standardMarketSize, avgTransactionValue, None, None, None, None )

        case mod.NonEquityTransparencyClass(recordId, id, fromDate, toDate, liquid, method, largeInScalePre, largeInScalePost, instrumentSpecificPre, instrumentSpecificPost) =>
          TransparencyData(-1L, recordId, id, from, to, liq, method, None, None, None,None, None, None, None, largeInScalePre, largeInScalePost, instrumentSpecificPre, instrumentSpecificPost)

      }

