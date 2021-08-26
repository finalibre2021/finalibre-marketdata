import scala.xml.{Elem, Node, XML}

object ExploreLargeXML:
  def main(args: Array[String]): Unit =
    val fileName = """c:\temp\FULINS_C_20210612_01of01.xml"""
    val elem = XML.loadFile(fileName)
    val elemNames : Seq[String] = (elem \\ "_").collect {
      case el : Elem => el.label
    }
    elemNames.groupBy(x => x).map(grp => grp._1 -> grp.size).toList.sortBy(_._2).foreach(p => println(s"${p._1}: ${p._2}"))
