package esma

import com.typesafe.config.ConfigFactory

object EsmaConfiguration {
  lazy val esmaConf = ConfigFactory.load().getConfig("esma")

  lazy val dbConfig =
    val dbConf = esmaConf.getConfig("db")
    DbConfig(dbConf.getString("url"), dbConf.getString("user"), dbConf.getString("password"))

  case class DbConfig(url : String, user : String, password : String)

}
