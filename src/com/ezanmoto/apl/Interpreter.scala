package com.ezanmoto.apl

import java.io.InputStream

/** Char is converted to Character explicitly */

object Char2Character {
  implicit def Char2Character( c: Char ) = Character( c )
}

class Character( c: Char ) {
  def isWhitespace( c: Char ) = c == ' ' || c == '\t'
}

object Character {
  def apply( c: Char ) = new Character( c )
  def unapply( c: Char ) = Some( c )
}

class CharStream( stream: InputStream ) {

  private var head: Option[Char] = None

  def peek() = {
    if ( None == head )
      head = getNext
    head get
  }

  private def getNext = Some( ( stream read ) toChar )

  def eat( c: Char ): Unit =
    if ( this.peek == c )
      head = getNext
    else
      throw new IllegalArgumentException(
        "Expected '" + c + "' but got '" + this.peek + "'" )

  def clear(): Unit = {
    while( stream.available > 0 )
      getNext
    head = None
  }
}

class APLInterpreter( in: CharStream ) {

  var isRunning = true

  def read() = {
    skipWhitespace()
    ( in peek ) match {
      case '\'' => readString()
      case ':'  => readCommand()
      case _    => error()
    }
  }

  def error() = {
    in.clear()
    println( "[!] Error" )
  }

  def readString() = {
    in eat '\''
    var buffer = ""
    var c = in.peek()
    while ( c != '\'' ) {
      buffer = buffer + c
      in eat c
      c = in.peek()
    }
    println( buffer )
    in eat '\''
  }

  def skipWhitespace() = {
    var c = in.peek()
    while ( c isWhitespace ) {
      in eat c
      c = in.peek()
    }
  }

  def readCommand() = {
    in eat ':'
    ( in peek ) match {
      case 'q' => in eat 'q'; println( "Goodbye." ); isRunning = false
      case _   => error()
    }
  }
}

object Interpreter {

  private var running = true

  def main( args: Array[String] ) = {
    val stream = new CharStream( System.in )
    val interpreter = new APLInterpreter( stream )
    println( "CLEAR WS" )
    while ( interpreter isRunning ) {
      print( "      " )
      interpreter.read()
    }
  }
}
