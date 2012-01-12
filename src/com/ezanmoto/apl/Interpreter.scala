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
    if ( s.length > 0 && ( s.head == ' ' || s.head == '\t' ) )
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
      case '~' | Integer( _ ) => r = ( this readExpression in ) toString
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

  def readExpression( line: String ): Int = {
    val r = readInteger( line )
    val a = r._1
    var in = r._2.skipWhitespace
    if ( in.length > 0 )
      in.head match {
        case '+' => a + readInteger( in eat '+' )._1
        case '-' => a - readInteger( in eat '-' )._1
        case '*' => a * readInteger( in eat '*' )._1
        case '%' => {
          val b = readInteger( in eat '%' )._1
          if ( b == 0 ) {
            println( domain( "Can't divide by 0" ) )
            0
          } else
            a / b
        }
        case _   => println( unexpected( in ) ); a
      }
    else
      a
  }

  def domain( s: String ) = "DOMAIN ERROR: " + s

  def readInteger( line: String ): (Int, String) = {
    var in = line skipWhitespace
    var buffer = ""
    if ( in.length > 0 ) {
      if ( in.head == '~' ) {
        buffer = buffer + "-"
        in = in drop 1
      }
      if ( in.length > 0 ) {
        do {
          buffer = buffer + in.head
          in = in drop 1
        } while ( ( in.length > 0 ) && ( in.head isDigit ) )
        ( buffer toInt, in )
      } else {
        println( syntax( "Expected integer" ) )
        ( 0, in )
      }
    } else
      println( syntax( "Expected integer" ) )
      ( 0, in )
  }

  def syntax( s: String ) = "SYNTAX ERROR: " + s

  def readCommand( line: String ): String = {
    val in = line eat ':'
    if ( in.length > 0 )
      in.head match {
        case 'q' => isRunning = false; "Goodbye."
        case _   => this unexpected in
      }
    else
      error( "Expected further input" )
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
