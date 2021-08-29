package esma

import sttp.client3.*
import finalibre.marketdata.util.XMLParsing
import org.apache.commons.io.{FileUtils, IOUtils}
import sttp.client3.{HttpURLConnectionBackend, Response}

import java.io.File
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.util.Try
import scala.xml.XML

object ExploreFIDRS:
  def main(args: Array[String]): Unit =
    downloadFiles




  def downloadFiles : Unit =
    val outputFolder = new File("C:\\temp\\fidrs-download")
    val url = uri"""https://registers.esma.europa.eu/solr/esma_registers_firds_files/select?q=*&fq=publication_date:%5B2021-01-25T00:00:00Z+TO+2021-06-01T23:59:59Z%5D&wt=xml&indent=true&start=0&rows=100"""
    // Reference instruments:
    val backend = HttpURLConnectionBackend()
    val response : Response[Either[String, String]] = basicRequest
      .get(url).send(backend)

    response.body.toOption.foreach {
      case strRes =>
        println(strRes)
        val elem = XML.loadString(strRes)
        val urls = (elem \\ "str").map(en => (en.\@("name"),en)).filter(en => en._1 == "download_link").map(en => en._2.text).filter(_.contains("FUL"))
        val notExists = urls.map(f => (f, f.replaceAll(".*firds/", ""))).filter(p => !(new File(outputFolder, p._2)).exists()).map(_._1)
        println(s"Found ${notExists.length} URLs")
        notExists.foreach {
          case fileUrl => Try {
            val fileResponse: Response[Either[String, Array[Byte]]] = basicRequest.response(asByteArray).get(uri"$fileUrl").send(backend)
            fileResponse.body.toOption.foreach {
              case bytes => {
                val outputFilename = fileUrl.replaceAll(".*firds/", "")
                val outFile = new File(outputFolder, outputFilename)
                FileUtils.writeByteArrayToFile(outFile, bytes)
                println(s"Wrote: ${outFile.getAbsolutePath} (${bytes.length} bytes)")
              }
            }
          }

        }

    }


  private def extractFile() : Unit =
    val inputFile = new File("""c:\temp\FULINS_C_20210612_01of01.xml""")
    val str = FileUtils.readFileToString(inputFile, "UTF-8")
    XMLParsing.extractShells(str) match {
      case Left(errs) => errs.foreach(println)
      case Right(res) =>
        val elements = mutable.HashMap.empty[(String, Int), Int]
        def visit(sh : XMLParsing.ElementShell) : Unit =
          val key = (sh.name, sh.level)
          if !elements.contains(key) then elements(key) = 1 else elements(key) = elements(key) + 1
          sh.children.foreach(visit)
        visit(res)
        elements.toList.sortBy(en => (en._1._2, en._1._2)).foreach(en => println(s"${en._1._1} (${en._1._2}): ${en._2}"))

    }



  private def unzip(file : File) : String =
    val zipFile : ZipFile = ZipFile(file)
    val entry = zipFile.entries().nextElement()
    IOUtils.toString(zipFile.getInputStream(entry), "UTF-8")
