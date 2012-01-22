package com.ezanmoto.apl

import com.ezanmoto.apl.Type._

import java.io.BufferedReader
import java.io.InputStreamReader

class APLInterpreter {

  private var line = ""

  private var env = Map[String, Variable]()

  private var programs = Map[String, List[String]]()

  private var in = new LookaheadStream( line )

  var isRunning = true

  def interpret( l: String ): Unit = {
    line = l
    this.in = new LookaheadStream( line )
    interpret()
  }

  def interpret(): Unit = {
    in.skipWhitespace()
    if ( ! in.isEmpty )
      in.peek match {
        case Uppercase( _ ) => assignmentOrExpressionOrProgram()
        case '(' | '\'' | '~' | Integer( _ ) | 'i' | 'p' | '+' =>
          println( expression() )
        case ')' => in.eat( ')' ); command()
        case 'v' => program()
        case _ => unexpected()
      }
      in.skipWhitespace()
      if ( ! in.isEmpty ) error( "Trailing characters" )
  }

  def assignmentOrExpressionOrProgram() = {
    val name = readName()
    in.skipWhitespace()
    val index =
      if ( ! in.isEmpty && in.peek == '[' ) Some( readIndex() ) else None
    in.skipWhitespace()
    if ( in.isEmpty ) { // Print value
      if ( isProgram( name ) && isVariable( name ) )
        error( "Program and variable called '" + name + "'" )
      else if ( isProgram( name ) )
        call( name )
      else {
        var value: Variable = lookup( name )
        if ( None != index )
          value = value at index.get
        println( value )
      }
    } else if ( in.peek == ':' ) { // Assignment
      in.eat( ':' )
      in.skipWhitespace()
      if ( index == None )
        env = env + ( name -> expression() )
      else {
        val replacement = lookup( name ) replace ( index get, expression() )
        env = env + ( name -> replacement )
      }
    } else { // Start of expression
      var value: Variable = lookup( name )
      if ( None != index )
        value = value at index.get
      println( expressionAfter( value ) )
    }
  }

  def readIndex(): Variable = {
    in eat '['
    val i = expression()
    in.skipWhitespace()
    in eat ']'
    i
  }

  def isProgram( name: String ) = programs contains name

  def isVariable( name: String ) = env contains name

  def call( name: String ): Unit =
    for ( sourceLine <- ( programs get name ) get )
      this interpret sourceLine

  def lookup( name: String ): Variable = ( env get name ) match {
    case Some( x ) => x
    case None      => error( "'" + name + "' has not been declared" )
  }

  def readName(): String = {
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

  def expression(): Variable = {
    in.skipWhitespace()
    expressionAfter( value() )
  }

  def value(): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected '~', identifier or integer" )
    else
      in.peek match {
        case '(' => {
          in.eat( '(' )
          val e = expression()
          in.eat( ')' )
          expressionAfter( e )
        }
        case '\'' => Variable( string() )
        case Uppercase( _ ) => lookup( readName() )
        case '~' | Integer( _ ) => integerOrListAfter( signedInteger() )
        case _ => unaryFunction()
      }
  }

  def unaryFunction(): Variable = in.peek match {
    case 'i' => in.eat( 'i'  ); expression() interval
    case 'p' => in.eat( 'p'  ); Variable( expression() length )
    case '+' => in.eat( "+/" ); expression() sum
  }

  def integerOrListAfter( integer: Int ): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty || ( ! in.peek.isDigit && in.peek != '~' ) )
      Variable( integer )
    else {
      var list = integer :: Nil
      do {
        list = list ::: List( signedInteger() )
        in.skipWhitespace()
      } while ( ! in.isEmpty && ( in.peek.isDigit || in.peek == '~' ) )
      Variable( list )
    }
  }

  def signedInteger(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected integer or '~'" )
    else {
      var isNegative = in.peek == '~'
      if ( isNegative ) in.eat( '~' )
      integer() * ( if ( isNegative ) -1 else 1 )
    }
  }

  def integer(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected further input" )
    else if ( ! in.peek.isDigit )
      error( "Expected integer, got '" + in.peek + "'" )
    else {
      var buffer = ""
      do {
        buffer = buffer + in.peek
        in.skip()
      } while ( ! in.isEmpty && in.peek.isDigit )
      buffer.toInt;
    }
  }

  def expressionAfter( value: Variable ): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty )
      value
    else if ( in.peek == '[' ) {
      val v = value at readIndex()
      in.skipWhitespace()
      if ( in.isEmpty )
        v
      else
        arithmetic( v )
    } else
      arithmetic( value )
  }

  def arithmetic( a: Variable ): Variable = in.peek match {
    case '+' => in.eat( '+' ); expressionAfter( a + expression() )
    case '-' => in.eat( '-' ); expressionAfter( a - expression() )
    case 'x' => in.eat( 'x' ); expressionAfter( a * expression() )
    case '%' => in.eat( '%' ); expressionAfter( a / expression() )
    case '|' => in.eat( '|' ); expressionAfter( a % expression() )
    case _   => concatenation( a )
  }

  def concatenation( a: Variable ): Variable = in.peek match {
    case ',' => in.eat( ',' ); expressionAfter( a ++ expression() )
    case _   => comparison( a )
  }

  def comparison( a: Variable ): Variable = in.peek match {
    case '=' => in eat '='; expressionAfter( a == expression() )
    case 'n' => in eat 'n'; expressionAfter( a != expression() )
    case '<' => in eat '<'; expressionAfter( a <  expression() )
    case 'l' => in eat 'l'; expressionAfter( a <= expression() )
    case '>' => in eat '>'; expressionAfter( a >  expression() )
    case _   => minimax( a )
  }

  def minimax( a: Variable ): Variable = in.peek match {
    case 'r' => in eat 'r'; expressionAfter( a max expression() )
    case '_' => in eat '_'; expressionAfter( a min expression() )
    case _   => a
  }

  def string(): String = {
    in eat '\''
    var buffer = ""
    while ( ! in.isEmpty && in.peek != '\'' )
      buffer = buffer + in.drop()
    if ( in.isEmpty )
      error( "Expected string terminator" )
    else {
      in eat '\''
      buffer
    }
  }

  def unexpected() =
    if ( in.isEmpty )
      error( "Unexpected end of line" )
    else
      error( "Unexpected '" + in.peek + "'" )

  def error( s: String ) = {
    var pre = ""
    for ( i <- 1 to in.eaten ) pre += " "
    throw new RuntimeException(
      "\n" + line + "\n" + pre + "^\n[!] Error: " + s )
  }

  def command(): Unit =
    if ( in.isEmpty )
      error( "Expected further input" )
    else
      in.peek match {
        case 'O' => in.eat( "OFF" ); isRunning = false; println( "Goodbye." )
        case 'E' => in.eat( "ERASE" ); erase()
        case _   => unexpected()
      }

  def erase(): Unit = {
    do {
      in.skipWhitespace()
      val name = readName()
      if ( programs contains name )
        programs = programs - name
      else
        error( "No program named '" + name + "'" )
    } while ( ! in.isEmpty )
  }

  def program(): Unit = {
    in.eat( 'v' )
    in.skipWhitespace()
    val name = readName()
    in.skipWhitespace()
    if ( ! in.isEmpty && in.peek == '[' ) {
      in.eat( "[b]" )
      in.skipWhitespace()
      in.eat( 'v' )
      printProgram( name )
    } else if ( env.contains( name ) )
      error( "'" + name + "' is a variable" )
    else
      programs = programs + ( name -> readProgram() )
  }

  def printProgram( name: String ) = {
    if ( isProgram( name ) ) {
      println( "      v " + name )
      var i = 0
      for ( l <- ( programs get name ) get ) {
        i += 1
        println( "[" + i + "]   " + l )
      }
      println( "      v" )
    } else
      error( "No program named '" + name + "'" )
  }

  def readProgram(): List[String] = {
    val isr    = new InputStreamReader( System.in )
    val reader = new BufferedReader( isr )
    var finished = false
    var lines: List[String] = Nil
    var i = 0
    while ( ! finished ) {
      i += 1
      print( "[" + i + "]   " )
      val input = reader.readLine()
      finished = input startsWith "v"
      if ( ! finished )
        lines = lines ::: List( input )
    }
    lines
  }
}
