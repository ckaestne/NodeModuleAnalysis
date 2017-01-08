package edu.cmu.cs.nodesec.tools

import java.io.File
import java.sql.{Connection, DriverManager, SQLException}

import scala.io.Source

/**
  * Created by ckaestne on 1/8/17.
  */
object DB {

  // connect to the database named "mysql" on the localhost
  val driver = "com.mysql.jdbc.Driver"
  val url = "jdbc:mysql://feature.isri.cmu.edu/npm"

  def loadCredentials() = {
    val c: String = if (new File(".credentials").exists())
      Source.fromFile(".credentials").getLines().next()
    else
      System.getenv("NPMCRED")

    val cr = c.split(":")
    (cr(0), cr(1))
  }

  val (username, password) = loadCredentials()

  // there's probably a better way to do this
  var connection: Connection = null

  // make the connection
  Class.forName(driver)
  connection = DriverManager.getConnection(url, username, password)
  connection.setAutoCommit(false)
  connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)

  //    // create the statement, and run the select query
  //    val statement = connection.createStatement()
  //    val resultSet = statement.executeQuery("SELECT host, user FROM user")
  //    while (resultSet.next()) {
  //      val host = resultSet.getString("host")
  //      val user = resultSet.getString("user")
  //      println("host, user = " + host + ", " + user)
  //    }

  case class Result(pkg: String, version: String, errorCode: Int, desc: String)

  def insertPackage(pkg: String, version: String) = {
    val stmt = connection.prepareStatement("insert into packages (name, version) values (?, ?)")
    stmt.setString(1, pkg)
    stmt.setString(2, version)
    stmt.execute()
  }

  def grabNextPackage(): Option[(String, String)] = try {
    var result = connection.createStatement().executeQuery("select `name`, `version` from packages where `name` not in (select package from results) limit 1")
    if (result.first()) {
      val pkg = result.getString(1)
      val ver = result.getString(2)
      val stmt = connection.prepareStatement("insert into results (package, version, result) values (?, ?, 0)")
      stmt.setString(1, pkg)
      stmt.setString(2, ver)
      stmt.execute()
      connection.commit()
      Some((pkg, ver))
    }
    else None
  } catch {
    case e: com.mysql.jdbc.exceptions.jdbc4.MySQLIntegrityConstraintViolationException => None
    case e: SQLException =>
      connection.rollback()
      return grabNextPackage()
  }

  def reportResult(r: Result) = {
    println(r)
    val rmstmt = connection.prepareStatement("delete from results where package=? and result=0")
    rmstmt.setString(1, r.pkg)
    rmstmt.execute()
    val stmt = connection.prepareStatement("insert into results (package, version, result, comment) values (?, ?, ?, ?)")
    stmt.setString(1, r.pkg)
    stmt.setString(2, r.version)
    stmt.setInt(3, r.errorCode)
    stmt.setString(4, r.desc)
    stmt.execute()
    connection.commit()
  }

}


object Test extends App {
  val entries = for (l <- Source.fromFile("packages.txt").getLines()) yield {
    assert(!(l contains "\""))
    val s = l.split(",")
    "(\"" + s(0) + "\", \"" + s(1) + "\")"
  }

  println("starting insert.")
  val sql = "insert ignore into packages (name, version) values "+entries.mkString(", ")
  DB.connection.createStatement().execute(sql)
  DB.connection.commit()

  println("done.")
}