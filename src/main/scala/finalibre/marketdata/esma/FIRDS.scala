package finalibre.marketdata.esma

import java.time.LocalDateTime
import java.time.LocalDate

object FIRDS:

  case class InstrumentReport(bizMsgId : String, msgId : String, created : LocalDateTime, date : LocalDate, authority : String)

  case class Instrument(id : String, fullName : Option[String], shortName : Option[String], classification : Option[String], cur : Option[String], commDerInd : Option[Boolean],
                        issuer : Option[String], venueId : Option[String])

