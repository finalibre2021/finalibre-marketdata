package esma.model

import java.sql.Date

case class TransparencyData(
                             val reportId : Long,
                             val recordId : Long,
                             val instrumentId : String,
                             val fromDate : Option[Date],
                             val toDate : Option[Date],
                             val liquid : Int,
                             val method : String,
                             val classification : Option[String],
                             val market : Option[String],
                             val avgDailyTurnover : Option[Double],
                             val largeInScale : Option[Double],
                             val avgDailyNoOfTrans : Option[Double],
                             val standardMarketSize : Option[Double],
                             val avgTransactionValue : Option[Double],
                             val largeInScalePre : Option[Double],
                             val largeInScalePost : Option[Double],
                             val instrumentSpecificPre : Option[Double],
                             val instrumentSpecificPost : Option[Double]
                           )
