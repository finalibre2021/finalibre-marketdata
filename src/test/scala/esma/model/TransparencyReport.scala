package esma.model

import java.sql.Date
import java.sql.Timestamp

case class TransparencyReport(
                               reportId : Long,
                               reportType : String,
                               fileName : String,
                               created : Timestamp,
                               periodFrom : Date,
                               periodTo : Date)

