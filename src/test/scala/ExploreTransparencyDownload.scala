import finalibre.marketdata.util.XMLParsing
import org.apache.commons.io.{FileUtils, IOUtils}
import sttp.client3.*

import java.io.{File, FileInputStream}
import java.time.LocalDate
import java.util.zip.{ZipFile, ZipInputStream}
import scala.util.Try
import scala.xml.{Elem, NodeSeq, XML}

object ExploreTransparencyDownload:

  val outputFolder = new File("""c:\temp\esma-transparency-files""")

  def main(args: Array[String]): Unit =


    for
      fil <- outputFolder.listFiles().sortBy(en => -en.length()).take(1)
    do
      println(s"Doing file: $fil")
      val elemStr = unzip(fil)
      val rootShell = XMLParsing.extractElementShells(elemStr)
      println("Completed running through file")
    //val parsed = parse(elem \\ "")

  //println(parsed)



  def parse(nodeSeq : NodeSeq) : TradeReport =
    val id = (nodeSeq \ "Id").text
    val timInf = nodeSeq \ "RptgPrd" \ "FrDtToDt"
    val from = LocalDate.parse((timInf \ "FrDt").text)
    val to = LocalDate.parse((timInf \ "ToDt").text)
    val stats = nodeSeq \ "Sttstcs"

    val cur = stats \\ "AvrgDalyTrnvr" \@ "Ccy"
    val turnover = (stats \ "AvrgDalyTrnvr").text.toDouble
    val scale = (stats \ "LrgInScale").text.toDouble
    val noOfTrans = (stats \ "LrgInScale").text.toDouble
    TradeReport(id, from, to, cur, turnover, scale, noOfTrans)

  case class TradeReport(securityID : String, periodFrom : LocalDate, periodTo : LocalDate, cur : String, dailyTurnover : Double, scale : Double, noOfTrans : Double)


  def unzip(file : File) : String =
    val zipFile : ZipFile = ZipFile(file)
    val entry = zipFile.entries().nextElement()
    IOUtils.toString(zipFile.getInputStream(entry), "UTF-8")



  def downloadFiles : Unit =
    val url = uri"""https://registers.esma.europa.eu/solr/esma_registers_fitrs_files/select?q=*&fq=creation_date:%5B2017-11-24T00:00:00Z+TO+2021-11-24T23:59:59Z%5D&wt=xml&indent=true&start=0&rows=20000"""

    val backend = HttpURLConnectionBackend()
    val response : Response[Either[String, String]] = basicRequest
      .get(url).send(backend)

    response.body.toOption.foreach {
      case strRes =>
        val elem = XML.loadString(strRes)
        val urls = (elem \\ "str").map(en => (en.\@("name"),en)).filter(en => en._1 == "download_link").map(en => en._2.text)
        println(s"Found ${urls.length} URLs")
        urls.foreach {
          case fileUrl => Try {
            val fileResponse: Response[Either[String, Array[Byte]]] = basicRequest.response(asByteArray).get(uri"$fileUrl").send(backend)
            fileResponse.body.toOption.foreach {
              case bytes => {
                val outputFilename = fileUrl.replaceAll(".*fitrs/", "")
                val outFile = new File(outputFolder, outputFilename)
                FileUtils.writeByteArrayToFile(outFile, bytes)
                println(s"Wrote: ${outFile.getAbsolutePath} (${bytes.length} bytes)")
              }
            }
          }

        }

    }









