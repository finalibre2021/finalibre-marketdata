package finalibre.marketdata.util

import java.io.{File, FileOutputStream}
import scala.collection.mutable as mut
import mut.ArrayBuffer

object XMLParsing {

  case class ElementShell private (elementName : String, elementChildren : mut.ArrayBuffer[ElementShell], elementLevel : Int):
    def name = elementName
    def children = elementChildren.toSeq
    def level = elementLevel

  object ElementShell:
    def from(name : String, children : mut.ArrayBuffer[ElementShell], level : Int) = ElementShell(name, children, level)

  case class SplitResult(tmpFolder : File, splitBy : String, root : File, files : List[File]):
    private var isCleanedUp = false
    def cleanUp() : Unit =
      tmpFolder.listFiles().foreach(_.delete)
      tmpFolder.delete()
      isCleanedUp = true

    def rootElement : Option[scala.xml.Elem] =
      if isCleanedUp
      then None
      else
        val elem = scala.xml.XML.loadFile(root)
        Some(elem)

    def fileElements : Unit =
      files.to(LazyList).map {
        case f => scala.xml.XML.loadFile(f)
      }

  private enum ParsingState:
    case UnMatched, MatchedOpen, MatchedOpenQuestionMark, MatchedOpenSlash,
    MatchedElementNameCharacter, MatchedPrologCharacter, MatchedCloseSlash,
    MatchedCloseQuestionMark, InElement

  private val PrologParseStates = Set(ParsingState.MatchedOpenQuestionMark, ParsingState.MatchedPrologCharacter)
  private val ElementNameParseStates = Set(ParsingState.MatchedOpen, ParsingState.MatchedOpenSlash, ParsingState.MatchedElementNameCharacter)


  private val lockObj = "lock"
  private var currentId = 0L
  private def nextId : Long = lockObj.synchronized {
    currentId += 1L
    currentId
  }

  def extractShells(xmlString : String) : Either[Seq[String], ElementShell] =
    val parseErrors = mut.ArrayBuffer.empty[String]
    var root : Option[ElementShell] = None

    traverseXml(
      xmlString,
      (err) => parseErrors += err,
      (rootEl) => root = Some(rootEl)
    )

    (parseErrors, root) match {
      case (errs, _) if !errs.isEmpty => Left(errs.toSeq)
      case (_, Some(roo)) => Right(roo)
      case (_,_) => Left(Seq("Could not match root"))
    }

  def splitXmlString(str : String, masterTempDir : File, splitTag : String, elementsPerFile : Int) : Either[Seq[String], SplitResult] =
    val tempId = nextId
    val tmpDir = new File(masterTempDir, s"$tempId")
    if(!tmpDir.exists())
      tmpDir.mkdirs()
    tmpDir.listFiles().foreach(f => f.delete())
    val encapsulating = new StringBuilder()
    val buffer = new StringBuilder()

    val tagBuffer = new StringBuilder()
    var isParsingTag = false
    def flushTagBuffer() : Unit =
      buffer.append(tagBuffer)
      tagBuffer.clear()
      isParsingTag = false

    var nestingLevel = 0
    var bundleBuffer = new StringBuilder()
    var inCurrentBundle = 0

    val errors = mut.ArrayBuffer.empty[String]
    val files = mut.ArrayBuffer.empty[File]
    var root : Option[File] = None

    def writeBundle() : Unit =
      val id = nextId
      encapsulating.append(s"\r\n    <movedTo id=\"$id\"/>")
      val outFile = new File(tmpDir, s"${id}.xml")
      val toWrite = new StringBuilder("<container>\r\n").append(bundleBuffer).append("\r\n</container>")
      writeOutput(toWrite, outFile)
      bundleBuffer.clear()
      files += outFile
      inCurrentBundle = 0

    traverseXml(
      str,
      onError = err => errors += err,
      onAssembledRoot = (roo) => {
        if inCurrentBundle > 0
        then writeBundle()
        buffer.append(tagBuffer)
        tagBuffer.clear()
        encapsulating.append(buffer)
        buffer.clear()
        val rootFile = new File(tmpDir,"a_root.xml")
        writeOutput(encapsulating, rootFile)
        root = Some(rootFile)
      },
      onOpenTag = {
        case str if str == splitTag =>
          if nestingLevel == 0
          then
            val filteredBuffer = buffer.dropWhile(c => c == ' ' || c == '\r' || c == '\n')
            buffer.clear()
            encapsulating.append(filteredBuffer)
          flushTagBuffer()
          nestingLevel += 1
        case _ =>
          flushTagBuffer()
      },
      onCloseTag = {
        case str if str == splitTag =>
          flushTagBuffer()
          nestingLevel -= 1
          if nestingLevel == 0
          then
            bundleBuffer.append(buffer)
            buffer.clear()
            inCurrentBundle += 1
            if inCurrentBundle == elementsPerFile
            then writeBundle()
        case _ =>
          flushTagBuffer()
      },
      feed = {
        case '<' =>
          isParsingTag = true
          tagBuffer.append('<')
        case ch if isParsingTag =>
          tagBuffer.append(ch)
        case ch => buffer.append(ch)
      }
    )
    (errors, root) match {
      case (errs, _) if !errs.isEmpty => Left(errs.toSeq)
      case (_, Some(roo)) => Right(SplitResult(tmpDir, splitTag, roo, files.toList))
      case (_,_) => Left(Seq("Could not match root"))
    }


  private def writeOutput(builder : StringBuilder, file : File) : Unit =
    file.createNewFile()
    val outStream = new FileOutputStream(file)
    builder.foreach(ch => outStream.write(ch))
    outStream.close()


  private def traverseXml(
                           xmlString : String,
                           onError : String => Unit,
                           onAssembledRoot : ElementShell => Unit,
                           onOpenTag : String => Unit = str => (),
                           onCloseTag : String => Unit = str => (),
                           feed : Char => Unit = ch => ()
                         ) : Unit =
    var currentState = ParsingState.UnMatched
    val currentElementName = new StringBuilder()
    val openElements = mut.ArrayDeque.empty[ElementShell]
    var currentElementIsOpen = true
    var currentLevel = 0
    var isInAttributeString = false
    var spaceEncountered = false

    def createUnexpectedCharacterError(char : Char, currentIndex : Int, contextStringLength : Int) : String =
      val fromIndex = if currentIndex < contextStringLength then 0 else currentIndex - contextStringLength
      s"Unexpected $char at index: $currentIndex while in state: $currentState ... context: ${xmlString.substring(fromIndex, currentIndex)}"


    for
      i <- 0 until xmlString.length
    do
      feed(xmlString(i))
      (xmlString(i), currentState) match {
        case ('"',ParsingState.InElement) =>
        case ('"',_) =>
          isInAttributeString = !isInAttributeString
        case (ch, _) if isInAttributeString =>
        case ('<', ParsingState.UnMatched) | ('<', ParsingState.InElement) =>
          currentElementName.clear()
          currentState = ParsingState.MatchedOpen
          spaceEncountered = false
          currentElementIsOpen = true
        case ('<', _) =>
          onError(createUnexpectedCharacterError('<', i, 100))
        case ('/', ParsingState.MatchedOpen) =>
          currentState = ParsingState.MatchedOpenSlash
          currentElementIsOpen = false
        case ('/', ParsingState.MatchedElementNameCharacter) =>
          currentState = ParsingState.MatchedCloseSlash
          currentElementIsOpen = false
        case ('/', ParsingState.InElement) =>
        case ('/', _) =>
          onError(createUnexpectedCharacterError('/', i, 100))
        case ('?', ParsingState.MatchedOpen) =>
          currentState = ParsingState.MatchedOpenQuestionMark
        case ('?', ParsingState.MatchedPrologCharacter) =>
          currentState = ParsingState.MatchedCloseQuestionMark
        case ('>', ParsingState.MatchedCloseQuestionMark) =>
          currentState = ParsingState.UnMatched
        case ('>', ParsingState.MatchedElementNameCharacter) |('>', ParsingState.MatchedCloseSlash) =>
          val nam = currentElementName.toString.trim
          (currentElementIsOpen,openElements.headOption) match {
            case (false, Some(parent)) if currentState == ParsingState.MatchedCloseSlash =>
              val emptyItem = ElementShell.from(nam, mut.ArrayBuffer.empty[ElementShell], currentLevel)
              parent.elementChildren += emptyItem
            case (false, Some(el)) if el.name == nam =>
              openElements.dropInPlace(1)
              currentLevel -= 1
              openElements.headOption match {
                case Some(parent) =>
                  parent.elementChildren += el
                case None if i >= xmlString.length - 2 =>
                  onAssembledRoot(el)
                case _ =>
                  onError(s"Open element stack is empty for name: $nam at index: $i and can therefore not add element to parent")
              }
              onCloseTag(nam)
            case (false, Some(el)) =>
              onError(s"Failed to match open element on stack for: $nam (on stack: ${el.elementName}) at index: $i")
            case (false, None) =>
              onError(s"Cannot close element: $nam since open element stack is empty")
            case (true,_) =>
              currentLevel += 1
              val thisElement = ElementShell.from(nam, new ArrayBuffer[ElementShell], currentLevel)
              openElements.prepend(thisElement)
              onOpenTag(nam)
          }
          if currentElementIsOpen
          then currentState = ParsingState.InElement
          else  currentState = ParsingState.UnMatched

        case ('>',_) => onError(createUnexpectedCharacterError('>', i, 100))

        case (' ', _) =>
          spaceEncountered = true
        case (ch, st) if PrologParseStates(st) =>
          currentState = ParsingState.MatchedPrologCharacter
        case (ch, st) if ElementNameParseStates(st) =>
          if(!spaceEncountered)
            currentElementName.append(ch)
          currentState = ParsingState.MatchedElementNameCharacter
        case (ch, ParsingState.UnMatched) =>
        case (ch, ParsingState.InElement) =>
        case (ch, st) => onError(createUnexpectedCharacterError(ch, i, 100))
      }




}
