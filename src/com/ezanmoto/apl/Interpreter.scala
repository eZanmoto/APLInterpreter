package com.ezanmoto.apl

import java.io.BufferedReader
import java.io.InputStreamReader

class MyString( string: String ) {

  def eat( c: Char ) =
    if ( string.head == c )
      string drop 1
    else
      throw new IllegalArgumentException(
          "Expected '" + c + "', got '" + string.head + "'" )

  val skipWhitespace = this skipWS string

  private def skipWS( s: String ): String =
    if ( s.head == ' ' || s.head == '\t' )
      this skipWS ( s drop 1 )
    else
      s
}

object String2MyString {
  implicit def string2MyString( s: String ) = new MyString( s )
}

import String2MyString._

object Character {
  def apply( c: Char ) = new Character( c )
  def unapply( c: Char ) = Some( c )
}

object Integer {
  def unapply( c: Char ) = if ( c isDigit ) Some( c ) else None
}

class APLInterpreter {

  var isRunning = true

  def evaluate( line: String ) = {
    println( this read line )
  }

  def read( line: String): String = {
    val in: String = line skipWhitespace
    var r: String = ""
    in.head match {
      case '\'' => r = this readString in
      case ':'  => r = this readCommand in
      case Integer( _ ) => r = ( this readInteger in ) toString
      case _    => r = this unexpected in
    }
    r
  }

  def unexpected( s: String ): String =
    error( "Unexpected input, '" + s.head + "'" )

  def error( message: String ): String = "[!] Error: " + message

  def readString( in: String ): String = {
    var line = in eat '\''
    var buffer = ""
    while ( line.length > 0 && line.head != '\'' ) {
      buffer = buffer + line.head
      line = line drop 1
    }
    if ( line.length == 0 )
      error( "Expected end of string" )
    else {
      line eat '\''
      buffer
    }
  }

  def readInteger( line: String ): Int = {
    var in = line skipWhitespace
    var buffer = ""
    while ( ( in.length > 0 ) && ( in.head isDigit ) ) {
      buffer = buffer + in.head
      in = in drop 1
    }
    buffer toInt
  }

  /*
  def readExpression( a: Int ): Int = {
    skipWhitespace()
    ( in peek ) match {
      case '+' => in eat '+'; a + readInteger()
      case '-' => in eat '-'; a - readInteger()
      case '*' => in eat '*'; a * readInteger()
      case '%' => in eat '%'; a / readInteger()
      case _   => error(); 0
    }
  }
  */

  def readCommand( line: String ): String = {
    val in = line eat ':'
    in.head match {
      case 'q' => isRunning = false; "Goodbye."
      case _   => this unexpected in
    }
  }
}

object Interpreter {

  private var running = true

  def main( args: Array[String] ) = {
    val reader = new InputStreamReader( System.in )
    val in = new BufferedReader( reader )
    val interpreter = new APLInterpreter
    println( "CLEAR WS" )
    while ( interpreter isRunning ) {
      print( "      " )
      val line = in.readLine()
      interpreter.evaluate( line )
    }
  }
}
