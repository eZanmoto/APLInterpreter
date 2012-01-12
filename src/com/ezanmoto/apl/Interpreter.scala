package com.ezanmoto.apl

import java.io.InputStream

object Character {
  def apply( c: Char ) = new Character( c )
  def unapply( c: Char ) = Some( c )
}

object Integer {
  def unapply( c: Char ) = if ( c isDigit ) Some( c ) else None
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
      case Integer( c ) => println( readInteger() )
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

  def readInteger(): Int = {
    skipWhitespace()
    var buffer = ""
    var c = in.peek()
    while ( c isDigit ) {
      buffer = buffer + c
      in eat c
      c = in.peek()
    }
    readExpression( buffer toInt )
  }

  def readExpression( a: Int ): Int = {
    skipWhitespace()
    ( in peek ) match {
      case '+' => in eat '+'; a + readInteger()
      case '-' => in eat '-'; a - readInteger()
      case '*' => in eat '*'; a * readInteger()
      case '%' => in eat '%'; a / readInteger()
      case '\n' | '\r' => in.clear(); a
      case _   => error(); 0
    }
  }

  def skipWhitespace() = {
    var c = in.peek()
    while ( c == ' ' || c == '\t' ) {
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
    print( "      " )
    while ( interpreter isRunning ) {
      interpreter.read()
      print( "      " )
    }
  }
}
