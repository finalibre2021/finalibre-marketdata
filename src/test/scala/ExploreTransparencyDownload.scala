import org.apache.commons.io.FileUtils
import sttp.client3.*

import java.io.File
import scala.util.Try
import scala.xml.XML

object ExploreTransparencyDownload:
  def main(args: Array[String]): Unit =
    val outputFolder = new File("""c:\temp\esma-transparency-files""")

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










