package com.ezanmoto.apl

import java.io.BufferedReader
import java.io.InputStreamReader

class LookaheadStream( private var string: String ) {

  def isEmpty = string.length == 0

  def peek: Char =
    if ( this isEmpty )
      throw new RuntimeException( "Cannot peek beyond end of stream" )
    else
      string.head

  def eat( c: Char ): Unit =
    if ( this.isEmpty )
      throw new IllegalArgumentException( "Expected '" + c + "'" )
    else if ( c == peek )
      string = string drop 1
    else
      throw new IllegalArgumentException(
          "Expected '" + c + "', got '" + peek + "'" )

  def eat( s: String ): Unit = s.foreach( c => this eat c )

  def drop(): Char = {
    val c = peek
    skip()
    c
  }

  def skip(): Unit = string = string drop 1

  def skipWhitespace(): Unit =
      while ( ! this.isEmpty && peek.isWhitespace )
        skip()
}

object Uppercase {
  def apply( c: Char ) = new Character( c )
  def unapply( c: Char ) = if ( c >= 'A' && c <= 'Z' ) Some( c ) else None
}

object Integer {
  def unapply( c: Char ) = if ( c isDigit ) Some( c ) else None
}

class APLInterpreter {

  private var env = Map[String, Int]()

  private var in: LookaheadStream = new LookaheadStream( "" )

  var isRunning = true

  def interpret( line: String ): Unit = {
    this.in = new LookaheadStream( line )
    interpret(): Unit
  }

  def interpret(): Unit = {
    in.skipWhitespace()
    in.peek match {
      case '\'' => println( readString() )
      case ':'  => runCommand()
      case '~' | Integer( _ ) => println( expression() )
      case Uppercase( _ ) => assignment()
      case _    => unexpected()
    }
  }

  def unexpected() = error( "Unexpected '" + in.peek + "'" )

  def readString(): String = {
    in eat '\''
    var buffer = ""
    while ( ! in.isEmpty && in.peek != '\'' )
      buffer = buffer + in.drop
    if ( in.isEmpty )
      error( "Expected string terminator" )
    else {
      in eat '\''
      buffer
    }
  }

  def error( s: String ) = throw new RuntimeException( "[!] Error: " + s )

  def runCommand(): Unit = {
    in eat ':'
    if ( in.isEmpty )
      error( "Expected further input" )
    else
      in.peek match {
        case 'q' => isRunning = false; println( "Goodbye." )
        case _   => unexpected()
      }
  }

  def expression(): Int = expression( readValue() )

  def expression( a: Int ): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      a
    else
      in.peek match {
        case '+' => in eat '+'; a + readValue()
        case '-' => in eat '-'; a - readValue()
        case 'x' => in eat 'x'; a * readValue()
        case '%' => {
          in eat '%'
          val b = readValue()
          if ( b == 0 )
            error( "Can't divide by 0" )
          else
            a / b
        }
        case _   => unexpected()
      }
  }

  def readValue(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected '~', identifier or integer" )
    else
      in.peek match {
        case Uppercase( _ ) => this valueOf readName()
        case Integer( _ ) | '~' => readInteger()
        case _ => error( "Expected '~', identifier or integer" )
      }
  }

  def readInteger(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected integer or '~'" )
    else {
      var isNegative = false
      if ( in.peek == '~' ) {
        isNegative = true
        in eat '~'
      }
      readNumber() * ( if ( isNegative ) -1 else 1 )
    }
  }

  def readNumber(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty || ( ! in.peek.isDigit ) )
      error( "Expected integer" )
    else {
      var buffer = ""
      do {
        buffer = buffer + in.peek
        in.skip()
      } while ( ! in.isEmpty && ( in.peek isDigit ) )
      buffer.toInt;
    }
  }

  def assignment() = {
    val name = readName()
    in.skipWhitespace()
    if ( in.isEmpty ) {
      println( valueOf( name ) )
    } else if ( in.peek == '<' ) {
      in.eat( "<-" )
      in.skipWhitespace()
      env = env + ( name -> expression() )
    } else {
      println( expression( valueOf( name ) ) )
    }
  }

  def valueOf( name: String ): Int = {
    val value = env get name
    if ( value == None )
      error( "'" + name + "' has not been declared" )
    else
      value.get
  }

  def readName() = {
    in.skipWhitespace()
    if ( in.isEmpty || ( ! in.peek.isUpper ) )
      error( "Expected identifier" )
    else {
      var buffer = ""
      do {
        buffer = buffer + in.peek
        in.skip()
      } while ( ! in.isEmpty && ( in.peek isUpper ) )
      buffer
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
      try {
        print( "      " )
        interpreter.interpret( in.readLine() )
      } catch {
        case e => println( e.getMessage )
      }
    }
  }
}
