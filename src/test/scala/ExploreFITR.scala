import java.io.File
import java.time.LocalDate
import finalibre.marketdata.esma.FITR
import finalibre.marketdata.esma.FITR.{EquityTransparencyData, NonEquityTransparencyClass, ParseType, TransparencyData, dataDateFormat, parseDouble}

object ExploreFITR {

  def main(args: Array[String]): Unit =
    useFITR()

  def useFITR() : Unit =
    val fromDate = LocalDate.of(2021, 1, 1)
    val toDate = LocalDate.of(2021, 12, 12)
    val zipFolder = new File("""c:\temp\fitr""")
    val unZipFilder = new File("""c:\temp\fitr-unzipped""")
    //val files = FITR.downloadZipFiles(tmpFolder, fromDate, toDate)
    //files.foreach(f => println(s"Downloaded file: ${f.getName}"))
    val reports = FITR.transparencyEntries(zipFolder, unZipFilder)
    val entryGroups = scala.collection.mutable.HashMap.empty[String, Int]
    def insert(entry : FITR.TransparencyData) : Unit = entryGroups.get(entry.id) match {
      case None => entryGroups(entry.id) = 1
      case Some(cnt) => entryGroups(entry.id) = cnt + 1
    }

    reports.foreach {
      case Left(errs) =>
        println("Following errors occured")
        errs.foreach(err => println(err))

      case Right(rep) =>
        println(s"Got report from: ${rep.fileName}")
        val loaded = rep.transparencyData()
        loaded.foreach(insert)
        val totalLoaded = entryGroups.map(_._2).sum
        println(s"Loaded ${loaded.size} from file: ${rep.fileName}. Total loaded: ${totalLoaded}. ${entryGroups.size} unique instruments")
        println(" Top 10")
        entryGroups.toList.sortBy(en => -en._2).take(10).foreach(p => println(s"  ${p._1}: ${p._2}"))
    }





}
