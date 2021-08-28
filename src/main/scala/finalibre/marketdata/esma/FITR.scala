package finalibre.marketdata.esma

import finalibre.marketdata.util.XMLParsing
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.File
import java.time.{LocalDate, LocalDateTime, ZoneId, ZoneOffset}
import sttp.client3.*

import java.time.format.DateTimeFormatter
import java.util.zip.ZipFile
import scala.collection.mutable as mut
import scala.util.Try
import scala.xml.{NodeSeq, XML}

object FITR {
  private val localDateQueryFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  private val dataTimestampFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  private val dataDateFormat = DateTimeFormatter.ISO_DATE

  private def parseDouble(ns : NodeSeq) = ns.text.trim match {
    case "" => None
    case str => Some(str.toDouble)
  }


  private enum ParseType:
    case Equity, NonEquty


  case class TransparencyReport(fileName : String, created : LocalDateTime, periodFrom : LocalDate, periodTo :LocalDate, transparencyData : () => LazyList[TransparencyData])
  trait TransparencyData(recordId : Long, id : String, fromDate : LocalDate, toDate : LocalDate)
  case class EquityTransparencyData(
                                     recordId : Long,
                                     id : String,
                                     fromDate : LocalDate,
                                     toDate : LocalDate,
                                     liquid : Boolean,
                                     method : String,
                                     avgDailyTurnover : Option[Double],
                                     largeInScale : Option[Double],
                                     avgDailyNoOfTrans : Option[Double]
                                   ) extends TransparencyData(recordId, id, fromDate, toDate)

  case class NonEquityTransparencyClass(
                                         recordId : Long,
                                         id : String,
                                         fromDate : LocalDate,
                                         toDate : LocalDate,
                                         liquid : Boolean,
                                         method : String,
                                         largeInScalePre : Option[Double],
                                         largeInScalePost : Option[Double],
                                         instrumentSpecificPre : Option[Double],
                                         instrumentSpecificPost : Option[Double]
                                       ) extends TransparencyData(recordId,id,fromDate,toDate)

  def transparencyEntries(zipDirectory : File, splitDirectory : File) : LazyList[Either[Seq[String],TransparencyReport]] =
    val zipFiles = zipDirectory.listFiles().filter(f => f.getName.matches("(?i)ful.cr.*zip"))
    zipFiles.to(LazyList).map {
      case f =>
        val str = unzip(f)
        val parseType = if f.getName.matches("(?i)fulecr.*") then ParseType.Equity else ParseType.NonEquty
        val splitTag = if parseType == ParseType.Equity then "EqtyTrnsprncyData" else "NonEqtyTrnsprncyData"
        val parseResult = XMLParsing.splitXmlString(str, splitDirectory, splitTag, 2_000)
        parseResult.flatMap {
          case res =>
            val containingElem = scala.xml.XML.loadFile(res.root)
            val createdAt = LocalDateTime.parse((containingElem \\ "CreDt").text, dataTimestampFormat)
            val periodFrom = LocalDate.parse((containingElem \\ "FinInstrmRptgEqtyTradgActvtyRslt" \\ "FrDt").text, dataDateFormat)
            val periodTo = LocalDate.parse((containingElem \\ "FinInstrmRptgEqtyTradgActvtyRslt" \\ "ToDt").text, dataDateFormat)

            val entriesLoader = () =>
              res.files.to(LazyList).flatMap {
                case file =>
                  val entriesXML = scala.xml.XML.loadFile(file)
                  val elems = (entriesXML \\ splitTag).map {
                    case el =>
                      val recordId = (el \\ "TechRcrdId").text.toLong
                      val id = (el \\ "Id").text
                      val from = LocalDate.parse((el \\ "FrDtToDt" \\ "FrDt").text, dataDateFormat)
                      val to = LocalDate.parse((el \\ "FrDtToDt" \\ "ToDt").text, dataDateFormat)
                      parseType match {
                      case ParseType.Equity =>
                        val liquid = (el \\ "Lqdty").text.toBoolean
                        val method = (el \\ "Mthdlgy").text
                        val avgDailyTurnover = parseDouble(el \\ "AvrgDalyTrnvr")
                        val largeInScale = parseDouble(el \\ "LrgInScale")
                        val avgNoOfTrans = parseDouble(el \\ "AvrgDalyNbOfTxs")
                        EquityTransparencyData(recordId, id, from, to, liquid,method,avgDailyTurnover,largeInScale,avgNoOfTrans)
                    }

                  }
              }
              res.cleanUp()

        }
    }



  def downloadZipFiles(tempFolder : File, dateFrom : LocalDate, dateTo : LocalDate, startFrom : Int = 0, maxNumberOfRows : Int = 20_000) : LazyList[File] =
    val fromTs = dateFrom.atStartOfDay().atZone(ZoneOffset.UTC)
    val toTs = dateTo.atTime(23,59,29).atZone(ZoneOffset.UTC)
    val url = uri"""https://registers.esma.europa.eu/solr/esma_registers_fitrs_files/select?q=*&fq=creation_date:%5B${fromTs.format(localDateQueryFormat)}+TO+${toTs.format(localDateQueryFormat)}%5D&wt=xml&indent=true&start=0&rows=$maxNumberOfRows"""
    val backend = HttpURLConnectionBackend()
    val response : Response[Either[String, String]] = basicRequest
      .get(url).send(backend)
    val returnee = mut.ArrayBuffer.empty[File]

    response.body.toOption.foreach {
      case strRes =>
        val elem = XML.loadString(strRes)
        val urls = (elem \\ "str").map(en => (en.\@("name"),en)).filter(en => en._1 == "download_link").map(en => en._2.text).filter(_.contains("FUL"))
        val urlsAndFiles = urls.map(url => (url,new File(tempFolder, url.replaceAll(".*fitrs/", ""))))
        val (exists, notExists) = urlsAndFiles.partition((url, file) => file.exists)
        exists.foreach(p => returnee += p._2)
        notExists.foreach {
          case (url, file) => Try {
            val fileResponse: Response[Either[String, Array[Byte]]] = basicRequest.response(asByteArray).get(uri"$url").send(backend)
            fileResponse.body.toOption.foreach {
              case bytes => {
                FileUtils.writeByteArrayToFile(file, bytes)
                returnee += file
              }
            }
          }

        }
    }
    returnee.to(LazyList)

  private def unzip(file : File) : String =
    val zipFile : ZipFile = ZipFile(file)
    val entry = zipFile.entries().nextElement()
    IOUtils.toString(zipFile.getInputStream(entry), "UTF-8")


}
