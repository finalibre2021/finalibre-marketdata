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
  val localDateQueryFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  val dataTimestampFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  val dataDateFormat = DateTimeFormatter.ISO_DATE

  def parseDouble(ns : NodeSeq) = ns.text.trim match {
    case "" => None
    case str => Some(str.toDouble)
  }

  def parseDate(ns : NodeSeq) : Option[LocalDate] = ns.text.trim match {
    case "" => None
    case str => Some(LocalDate.parse(str, dataDateFormat))
  }

  def parseString(ns : NodeSeq) : Option[String] = ns.text.trim match {
    case "" => None
    case str => Some(str)
  }


  enum ParseType:
    case Equity, NonEquty


  case class TransparencyReport(fileName : String, created : LocalDateTime, periodFrom : LocalDate, periodTo :LocalDate, transparencyData : () => List[TransparencyData])
  trait TransparencyData(val recordId : Long, val id : String, val fromDate : Option[LocalDate], val toDate : Option[LocalDate], val liquid : Boolean, val method : String)
  case class EquityTransparencyData(
                                     override val recordId : Long,
                                     override val id : String,
                                     override val fromDate : Option[LocalDate],
                                     override val toDate : Option[LocalDate],
                                     override val liquid : Boolean,
                                     override val method : String,
                                     classification : Option[String],
                                     market : Option[String],
                                     avgDailyTurnover : Option[Double],
                                     largeInScale : Option[Double],
                                     avgDailyNoOfTrans : Option[Double],
                                     standardMarketSize : Option[Double],
                                     avgTransactionValue : Option[Double]
                                   ) extends TransparencyData(recordId, id, fromDate, toDate, liquid, method)

  case class NonEquityTransparencyClass(
                                         override val recordId : Long,
                                         override val id : String,
                                         override val fromDate : Option[LocalDate],
                                         override val toDate : Option[LocalDate],
                                         override val liquid : Boolean,
                                         override val method : String,
                                         largeInScalePre : Option[Double],
                                         largeInScalePost : Option[Double],
                                         instrumentSpecificPre : Option[Double],
                                         instrumentSpecificPost : Option[Double]
                                       ) extends TransparencyData(recordId,id,fromDate,toDate, liquid, method)

  def transparencyEntries(zipDirectory : File, splitDirectory : File) : LazyList[Either[Seq[String],TransparencyReport]] =
    val zipFiles = zipDirectory.listFiles().filter(f => f.getName.matches("(?i)ful.cr.*zip"))
    zipFiles.to(LazyList).map {
      case f =>
        println(s"Working on: ${f.getAbsolutePath}")
        val str = unzip(f)
        val parseType = if f.getName.matches("(?i)fulecr.*") then ParseType.Equity else ParseType.NonEquty
        val splitTag = if parseType == ParseType.Equity then "EqtyTrnsprncyData" else "NonEqtyTrnsprncyData"
        val parseResult = XMLParsing.splitXmlString(str, splitDirectory, splitTag, 2_000)
        val mappedResult : Either[Seq[String], TransparencyReport] = parseResult.flatMap {
          case res =>
            val containingElem = scala.xml.XML.loadFile(res.root)
            val createdAt = LocalDateTime.parse((containingElem \\ "CreDt").text, dataTimestampFormat)
            val periodFrom = parseDate(containingElem \\ "FinInstrmRptgEqtyTradgActvtyRslt" \\ "FrDt").get
            val periodTo = parseDate(containingElem \\ "FinInstrmRptgEqtyTradgActvtyRslt" \\ "ToDt").get

            val entriesLoader = () =>
              val records : List[TransparencyData] = res.files.toList.flatMap {
                case file =>
                  val entriesXML = scala.xml.XML.loadFile(file)
                  val elems = (entriesXML \\ splitTag).map {
                    case el =>
                      val recordId = (el \ "TechRcrdId").text.toLong
                      val id = (el \ "Id").text
                      val from = parseDate(el \ "RptgPrd" \ "FrDtToDt" \ "FrDt")
                      val to = parseDate(el \ "RptgPrd" \ "FrDtToDt" \ "ToDt")
                      val liquid = (el \ "Lqdty").text.toBoolean
                      val method = (el \ "Mthdlgy").text

                      parseType match {
                        case ParseType.Equity =>
                          val classification = parseString(el \ "FinInstrmClssfctn")
                          val market = parseString(el \"RlvntMkt" \ "Id")
                          val avgDailyTurnover = parseDouble(el \ "Sttstcs" \ "AvrgDalyTrnvr")
                          val largeInScale = parseDouble(el \ "Sttstcs" \ "LrgInScale")
                          val avgNoOfTrans = parseDouble(el \ "Sttstcs" \ "AvrgDalyNbOfTxs")
                          val marketSize = parseDouble(el \ "Sttstcs" \ "StdMktSz")
                          val avgTransactionValue = parseDouble(el \ "Sttstcs" \ "AvrgTxVal")
                          EquityTransparencyData(recordId, id, from, to, liquid, method, classification, market, avgDailyTurnover, largeInScale, avgNoOfTrans, marketSize, avgTransactionValue)

                        case ParseType.NonEquty =>
                          val largeInScalePre = parseDouble(el \ "PreTradLrgInScaleThrshld" \ "Amt")
                          val largeInScalePost = parseDouble(el \ "PstTradLrgInScaleThrshld" \ "Amt")
                          val instrumentSpecificPre = parseDouble(el \ "PreTradInstrmSzSpcfcThrshld" \ "Amt")
                          val instrumentSpecificPost = parseDouble(el \ "PstTradInstrmSzSpcfcThrshld" \ "Amt")
                          NonEquityTransparencyClass(recordId, id, from, to, liquid, method, largeInScalePre, largeInScalePost, instrumentSpecificPre, instrumentSpecificPost)

                    }

                  }
                  elems.to(LazyList)
              }
              res.cleanUp()
              records

            Right(TransparencyReport(f.getAbsolutePath,createdAt,periodFrom,periodTo, entriesLoader))

        }
        mappedResult
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
