package finalibre.marketdata.util

import scala.collection.mutable as mut
import mut.ArrayBuffer

object XMLParsing {

  case class ElementShell private (elementName : String, elementChildren : mut.ArrayBuffer[ElementShell], elementLevel : Int):
    def name = elementName
    def children = elementChildren.toSeq
    def level = elementLevel

  object ElementShell:
    def from(name : String, children : mut.ArrayBuffer[ElementShell], level : Int) = ElementShell(name, children, level)

  private enum ParsingState:
    case UnMatched, MatchedOpen, MatchedOpenQuestionMark, MatchedOpenSlash, MatchedElementNameCharacter, MatchedPrologCharacter, MatchedCloseSlash, MatchedCloseQuestionMark

  private val PrologParseStates = Set(ParsingState.MatchedOpenQuestionMark, ParsingState.MatchedPrologCharacter)
  private val ElementNameParseStates = Set(ParsingState.MatchedOpen, ParsingState.MatchedOpenSlash, ParsingState.MatchedElementNameCharacter)


  def extractElementShells(xmlString : String) : Either[Seq[String], ElementShell] =
    //val charArr = xmlString.trim.toCharArray
    var currentState = ParsingState.UnMatched
    val currentElementName = new StringBuilder()
    val openElements = mut.ArrayDeque.empty[ElementShell]
    var currentElementIsOpen = true
    val parseErrors = mut.ArrayBuffer.empty[String]
    var root : Option[ElementShell] = None
    var currentLevel = 0
    var isInAttributeString = false
    var spaceEncountered = false

    def createUnexpectedCharacterError(char : Char, currentIndex : Int, contextStringLength : Int) : String =
      val fromIndex = if currentIndex < contextStringLength then 0 else currentIndex - contextStringLength
      s"Unexpected $char at index: $currentIndex while in state: $currentState ... context: ${xmlString.substring(fromIndex, currentIndex)}"


    for
      i <- 0 until xmlString.length
    do
      (xmlString(i), currentState) match {
        case ('"',_) =>
          isInAttributeString = !isInAttributeString
        case (ch, _) if isInAttributeString =>
        case ('<', ParsingState.UnMatched) =>
          currentElementName.clear()
          currentState = ParsingState.MatchedOpen
          spaceEncountered = false
          currentElementIsOpen = true

        case ('<', _) =>
          parseErrors += createUnexpectedCharacterError('<', i, 100)
        case ('/', ParsingState.MatchedOpen) =>
          currentState = ParsingState.MatchedOpenSlash
          currentElementIsOpen = false
        case ('/', ParsingState.MatchedElementNameCharacter) =>
          currentState = ParsingState.MatchedCloseSlash
          currentElementIsOpen = false
        case ('/', _) =>
          parseErrors += createUnexpectedCharacterError('/', i, 100)
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
                  root = Some(el)
                case _ =>
                  parseErrors += s"Open element stack is empty for name: $nam at index: $i and can therefore not add element to parent"
                  val restOfString = xmlString.substring(510062727, xmlString.length)
                  println(s"Rest of string: $restOfString")
              }
            case (false, Some(el)) =>
              parseErrors += s"Failed to match open element on stack for: $nam (on stack: ${el.elementName}) at index: $i"
            case (false, None) =>
              parseErrors += s"Cannot close element: $nam since open element stack is empty"
            case (true,_) =>
              currentLevel += 1
              val thisElement = ElementShell.from(nam, new ArrayBuffer[ElementShell], currentLevel)
              openElements.prepend(thisElement)
          }
          currentState = ParsingState.UnMatched

        case ('>',_) => parseErrors += createUnexpectedCharacterError('>', i, 100)

        case (' ', _) =>
          spaceEncountered = true
        case (ch, st) if PrologParseStates(st) =>
          currentState = ParsingState.MatchedPrologCharacter
        case (ch, st) if ElementNameParseStates(st) =>
          if(!spaceEncountered)
            currentElementName.append(ch)
          currentState = ParsingState.MatchedElementNameCharacter
        case (ch, ParsingState.UnMatched) =>
        case (ch, st) => parseErrors += createUnexpectedCharacterError(ch, i, 100)
      }

    (parseErrors, root) match {
      case (errs, _) if !errs.isEmpty => Left(errs.toSeq)
      case (_, Some(roo)) => Right(roo)
      case (_,_) => Left(Seq("Could not match root"))
    }


  /*    val tessa = scala.collection.mutable.ArrayDeque.empty[String]
      tessa.append("Append1")
      tessa.append("Append2")
      tessa.prepend("Prepend1")
      tessa.prepend("Prepend2")
      tessa.dropInPlace(1)
      println(s"Dropped 1, state: $tessa")
      tessa.dropRightInPlace(1)
      println(s"Dropped 1 right, state: $tessa")

      Dropped 1, state: ArrayDeque(Prepend1, Append1, Append2)
  Dropped 1 right, state: ArrayDeque(Prepend1, Append1)*/





}
