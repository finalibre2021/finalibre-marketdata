package esma

import sttp.client3.*
import finalibre.marketdata.util.XMLParsing
import org.apache.commons.io.{FileUtils, IOUtils}
import sttp.client3.{HttpURLConnectionBackend, Response}

import java.io.File
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.util.{Failure, Try}
import scala.xml.{NodeSeq, XML}

object ExploreFIDRS:
  def main(args: Array[String]): Unit =
    //EsmaDataHandler.DataInvestigation.createTables()
    findUniqueElementData
    //println(s"${transformers.size} vs. ${entryNames.size}")





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


  private def findElementNames() : Unit =
    val allElements = mutable.HashSet.empty[String]
    val folder = new File("""C:\temp\fidrs-download""")
    folder.listFiles().filter(f => f.getName.endsWith("zip")).map(f => (f.getName.substring(7,8),f)).groupBy(p => p._1).foreach {
      case (prefix, ents) =>
        val rep = ents.map(_._2).maxBy(_.length())
        val unzipped = unzip(rep)
        //val str = FileUtils.readFileToString(rep, "UTF-8")
        XMLParsing.extractShells(unzipped) match {
          case Left(errs) => errs.foreach(println)
          case Right(res) =>
            def visit(sh : XMLParsing.ElementShell, prefix : Option[String] = None) : Unit =
              val nam = prefix.map(p => p + "/").getOrElse("") + sh.name
              allElements += nam
              sh.children.foreach(c => visit(c, Some(nam)))
            visit(res)
            println(s"Parsed group: $prefix through:${rep.getName}")
        }


    }
    allElements.toList.sorted.foreach(println)

  private def unzip(file : File) : String =
    val zipFile : ZipFile = ZipFile(file)
    val entry = zipFile.entries().nextElement()
    IOUtils.toString(zipFile.getInputStream(entry), "UTF-8")


  def findUniqueElementData : Unit =
    val folder = new File("""C:\temp\fidrs-download""")
    val unzipFolder = new File("""C:\temp\firds-unzip""")
    folder.listFiles().filter(f => f.getName.endsWith("zip")).sortBy(_.length).foreach {
      case file =>
        Try {
          val unzipped = unzip(file)
          XMLParsing.splitXmlString(unzipped, unzipFolder, "RefData", 1000) match {
            case Left(errs) => errs.foreach(println)
            case Right(res) =>
              val results = mutable.HashMap.empty[(String, String), Long]
              val extractors = entryNames.zip(transformers)
              res.files.foreach {
                case fil => Try {
                  val elem = XML.loadFile(fil)
                  (elem \ "RefData").foreach {
                    case nod =>
                      extractors.foreach {
                        case (nam, func) =>
                          val exted = func(nod)
                          if(exted != null && exted.length > 0)
                          then
                            val key = (nam, exted)
                            if results.contains(key) then results(key) = results(key) +1 else results(key) = 1L
                      }
                  }
                }
              }
              val values = results.toSeq.map(en => (en._1._1, en._1._2, en._2))
              EsmaDataHandler.DataInvestigation.insertFileDate(file.getName, values)
              println(s"Inserted ${values.size} value-entries from ${file.getAbsolutePath}")
              res.cleanUp()

          }
        } match {
          case Failure(err) =>
          case _ =>
        }

    }






  val transformers = List(
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "DebtSnrty").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "BsisPtSprd").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "RefRate" \ "ISIN").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "RefRate" \ "Indx").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "RefRate" \ "Nm").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "Term" \ "Unit").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fltg" \ "Term" \ "Val").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "IntrstRate" \ "Fxd").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "MtrtyDt").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "NmnlValPerUnit").text,
    (en : NodeSeq) => (en  \ "DebtInstrmAttrbts" \ "TtlIssdNmnlAmt").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "FnlPricTp").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Dairy" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Dairy" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Frstry" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Frstry" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Grn" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Grn" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Grn" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "GrnOilSeed" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "GrnOilSeed" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "GrnOilSeed" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Ptt" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Ptt" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Sfd" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Sfd" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Soft" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Soft" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Agrcltrl" \ "Soft" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Envttl" \ "Emssns" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Envttl" \ "Emssns" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Envttl" \ "Emssns" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Envttl" \ "Wthr" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Envttl" \ "Wthr" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frght" \ "Dry" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frght" \ "Dry" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frtlzr" \ "Ammn" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frtlzr" \ "Ammn" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frtlzr" \ "UreaAndAmmnmNtrt" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Frtlzr" \ "UreaAndAmmnmNtrt" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "NonPrcs" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "NonPrcs" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "NonPrcs" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "Prcs" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "Prcs" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Metl" \ "Prcs" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "MultiCmmdtyExtc" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Elctrcty" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Elctrcty" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Elctrcty" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "NtrlGas" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "NtrlGas" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "NtrlGas" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Oil" \ "AddtlSubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Oil" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "Oil" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "RnwblNrgy" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Nrgy" \ "RnwblNrgy" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Othr" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "OthrC10" \ "Dlvrbl" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "OthrC10" \ "Dlvrbl" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Ppr" \ "Pulp" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Ppr" \ "Pulp" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Ppr" \ "RcvrdPpr" \ "BasePdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "Pdct" \ "Ppr" \ "RcvrdPpr" \ "SubPdct").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Cmmdty" \ "TxTp").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "FX" \ "FxTp").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "FX" \ "OthrNtnlCcy").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "FrstLegIntrstRate" \ "Fxd").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "IntrstRate" \ "RefRate" \ "Indx").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "IntrstRate" \ "RefRate" \ "Nm").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "IntrstRate" \ "Term" \ "Unit").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "IntrstRate" \ "Term" \ "Val").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrLegIntrstRate" \ "Fltg" \ "RefRate" \ "Indx").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrLegIntrstRate" \ "Fltg" \ "RefRate" \ "Nm").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrLegIntrstRate" \ "Fltg" \ "Term" \ "Unit").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrLegIntrstRate" \ "Fltg" \ "Term" \ "Val").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrLegIntrstRate" \ "Fxd").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "AsstClssSpcfcAttrbts" \ "Intrst" \ "OthrNtnlCcy").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "DlvryTp").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "OptnExrcStyle").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "OptnTp").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "PricMltplr").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "NoPric" \ "Ccy").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "NoPric" \ "Pdg").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "Pric" \ "BsisPts").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "Pric" \ "MntryVal" \ "Amt").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "Pric" \ "MntryVal" \ "Sgn").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "Pric" \ "Pctg").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "StrkPric" \ "Pric" \ "Yld").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Bskt" \ "ISIN").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Bskt" \ "LEI").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "ISIN").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "Indx" \ "ISIN").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "Indx" \ "Nm" \ "RefRate" \ "Indx").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "Indx" \ "Nm" \ "RefRate" \ "Nm").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "Indx" \ "Nm" \ "Term" \ "Unit").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "Indx" \ "Nm" \ "Term" \ "Val").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "UndrlygInstrm" \ "Sngl" \ "LEI").text,
    (en : NodeSeq) => (en  \ "DerivInstrmAttrbts" \ "XpryDt").text,
    (en : NodeSeq) => (en  \ "TechAttrbts" \ "PblctnPrd" \ "FrDt").text,
    (en : NodeSeq) => (en  \ "TechAttrbts" \ "RlvntCmptntAuthrty").text,
    (en : NodeSeq) => (en  \ "TechAttrbts" \ "RlvntTradgVn").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "AdmssnApprvlDtByIssr").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "FrstTradDt").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "Id").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "IssrReq").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "ReqForAdmssnDt").text,
    (en : NodeSeq) => (en  \ "TradgVnRltdAttrbts" \ "TermntnDt").text,
    (en : NodeSeq) => (en  \ "Issr").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "ClssfctnTp").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "CmmdtyDerivInd").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "FullNm").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "Id").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "NtnlCcy").text,
    (en : NodeSeq) => (en  \ "FinInstrmGnlAttrbts" \ "ShrtNm").text
  )
  val entryNames = List(
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/DebtSnrty",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/BsisPtSprd",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/RefRate/ISIN",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/RefRate/Indx",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/RefRate/Nm",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/Term/Unit",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fltg/Term/Val",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/IntrstRate/Fxd",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/MtrtyDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/NmnlValPerUnit",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DebtInstrmAttrbts/TtlIssdNmnlAmt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/FnlPricTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Dairy/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Dairy/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Frstry/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Frstry/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Grn/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Grn/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Grn/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/GrnOilSeed/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/GrnOilSeed/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/GrnOilSeed/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Ptt/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Ptt/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Sfd/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Sfd/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Soft/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Soft/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Agrcltrl/Soft/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Envttl/Emssns/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Envttl/Emssns/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Envttl/Emssns/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Envttl/Wthr/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Envttl/Wthr/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frght/Dry/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frght/Dry/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frtlzr/Ammn/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frtlzr/Ammn/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frtlzr/UreaAndAmmnmNtrt/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Frtlzr/UreaAndAmmnmNtrt/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/NonPrcs/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/NonPrcs/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/NonPrcs/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/Prcs/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/Prcs/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Metl/Prcs/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/MultiCmmdtyExtc/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Elctrcty/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Elctrcty/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Elctrcty/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/NtrlGas/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/NtrlGas/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/NtrlGas/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Oil/AddtlSubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Oil/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/Oil/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/RnwblNrgy/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Nrgy/RnwblNrgy/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Othr/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/OthrC10/Dlvrbl/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/OthrC10/Dlvrbl/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Ppr/Pulp/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Ppr/Pulp/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Ppr/RcvrdPpr/BasePdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/Pdct/Ppr/RcvrdPpr/SubPdct",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Cmmdty/TxTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/FX/FxTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/FX/OthrNtnlCcy",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/FrstLegIntrstRate/Fxd",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/IntrstRate/RefRate/Indx",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/IntrstRate/RefRate/Nm",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/IntrstRate/Term/Unit",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/IntrstRate/Term/Val",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrLegIntrstRate/Fltg/RefRate/Indx",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrLegIntrstRate/Fltg/RefRate/Nm",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrLegIntrstRate/Fltg/Term/Unit",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrLegIntrstRate/Fltg/Term/Val",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrLegIntrstRate/Fxd",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/AsstClssSpcfcAttrbts/Intrst/OthrNtnlCcy",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/DlvryTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/OptnExrcStyle",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/OptnTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/PricMltplr",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/NoPric/Ccy",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/NoPric/Pdg",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/Pric/BsisPts",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/Pric/MntryVal/Amt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/Pric/MntryVal/Sgn",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/Pric/Pctg",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/StrkPric/Pric/Yld",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Bskt/ISIN",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Bskt/LEI",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/ISIN",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/Indx/ISIN",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/Indx/Nm/RefRate/Indx",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/Indx/Nm/RefRate/Nm",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/Indx/Nm/Term/Unit",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/Indx/Nm/Term/Val",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/UndrlygInstrm/Sngl/LEI",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/DerivInstrmAttrbts/XpryDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TechAttrbts/PblctnPrd/FrDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TechAttrbts/RlvntCmptntAuthrty",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TechAttrbts/RlvntTradgVn",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/AdmssnApprvlDtByIssr",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/FrstTradDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/Id",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/IssrReq",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/ReqForAdmssnDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/TradgVnRltdAttrbts/TermntnDt",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/Issr",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/ClssfctnTp",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/CmmdtyDerivInd",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/FullNm",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/Id",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/NtnlCcy",
    "BizData/Pyld/Document/FinInstrmRptgRefDataRpt/RefData/FinInstrmGnlAttrbts/ShrtNm"
  )

