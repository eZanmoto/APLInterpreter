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

  private var env = Map[String, Either[Int, List[Int]]]()

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
      case '~' | Integer( _ ) => printEither( expression() )
      case Uppercase( _ ) => assignment()
      case _    => unexpected()
    }
    in.skipWhitespace()
    if ( ! in.isEmpty )
      error( "Trailing characters" )
  }

  def unexpected() =
    if ( in.isEmpty )
      error( "Unexpected end of line" )
    else
      error( "Unexpected '" + in.peek + "'" )

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
        case 'q' => in eat 'q'; isRunning = false; println( "Goodbye." )
        case _   => unexpected()
      }
  }

  def expression(): Either[Int, List[Int]] = readValue() match {
    case Left( v )  => expression( v )
    case Right( v ) => expression( v )
  }

  def expression( a: Int ): Either[Int, List[Int]] = {
    in.skipWhitespace()
    if ( in.isEmpty )
      Left( a )
    else
      in.peek match {
        case '+' => in eat '+'; Left( add( a, readValue() ) )
        case '%' => in eat '%'; Left( div( a, readValue() ) )
        case Integer( _ ) => expression( readListAfter( a ) )
        case _   => unexpected()
      }
  }

  def add( a: Int, b: Either[Int, List[Int]] ): Int = b match {
    case Left( v )  => a + v
    case Right( v ) => error( "Can't add int to list" )
  }

  def div( a: Int, b: Either[Int, List[Int]] ): Int = b match {
    case Left( v )  => if ( v == 0 ) error( "Can't divide by 0" ) else a / v
    case Right( v ) => error( "Can't divide int by list" )
  }

  def expression( a: List[Int] ): Either[Int, List[Int]] = {
    in.skipWhitespace()
    if ( in.isEmpty )
      Right( a )
    else
      in.peek match {
        case '+' => in eat '+'; Right( add( a, readValue() ) )
        case _   => unexpected()
      }
  }

  /** I use this function in the overloaded add, sub, mul and div functions to
    * convert the lists of Int to lists of Either[Int, Nothing] so that I can
    * map over them using my previously defined add, sub, mul and div functions.
    */
  def convert( list: List[Int] ) = list map( Left( _ ) )

  def add( a: List[Int], b: Either[Int, List[Int]] ): List[Int] = b match {
    case Left( v ) => ( convert( a ) ) map ( add( v, _ ) )
    case Right( v ) => ( a, convert( v ) ).zipped map ( add( _, _ ) )
  }

  def readListAfter( a: Int ): List[Int] = {
    in.skipWhitespace()
    if ( in.isEmpty )
      unexpected()
    else {
      var list = a :: Nil
      while ( ! in.isEmpty && in.peek.isDigit ) {
        list = list ::: List( readInteger() )
        in.skipWhitespace()
      }
      list
    }
  }

  def readIntegerOrList(): Either[Int, List[Int]] = {
    val a = readInteger()
    in.skipWhitespace()
    if ( in.isEmpty )
      Left( a )
    else {
      var list = a :: Nil
      while ( ! in.isEmpty && in.peek.isDigit ) {
        list = list ::: List( readInteger() )
        in.skipWhitespace()
      }
      if ( list.length > 1 ) Right( list ) else Left( list head ) 
    }
  }

  def readValue(): Either[Int, List[Int]] = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected '~', identifier or integer" )
    else
      in.peek match {
        case Uppercase( _ ) => this valueOf readName()
        case Integer( _ ) | '~' => readIntegerOrList()
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
    if ( in.isEmpty || ! in.peek.isDigit )
      error( "Expected integer" )
    else {
      var buffer = ""
      do {
        buffer = buffer + in.peek
        in.skip()
      } while ( ! in.isEmpty && in.peek.isDigit )
      buffer.toInt;
    }
  }

  def assignment() = {
    val name = readName()
    in.skipWhitespace()
    if ( in.isEmpty ) { // Print value
      printEither( valueOf( name ) )
    } else if ( in.peek == '<' ) { // Assignment
      in.eat( "<-" )
      in.skipWhitespace()
      env = env + ( name -> expression() )
    } else { // Start of expression
      valueOf( name ) match {
        case Left( v )  => printEither( expression( v ) )
        case Right( v ) => printEither( expression( v ) )
      }
    }
  }

  def printEither( e: Either[Int, List[Int]] ) = e match {
    case Left( v )  => println( v )
    case Right( v ) => println( v )
  }

  def valueOf( name: String ): Either[Int, List[Int]] = {
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
