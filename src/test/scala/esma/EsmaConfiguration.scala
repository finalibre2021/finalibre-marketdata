package esma

import com.typesafe.config.ConfigFactory

object EsmaConfiguration:
  case class DbConfig(val url : String, val user : String,val  password : String)

  lazy val esmaConf = ConfigFactory.load().getConfig("esma")

  lazy val dbConfig : DbConfig =
    val dbConf = esmaConf.getConfig("db")
    DbConfig(dbConf.getString("url"), dbConf.getString("user"), dbConf.getString("password"))

