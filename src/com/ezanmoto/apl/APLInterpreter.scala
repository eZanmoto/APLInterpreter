package com.ezanmoto.apl

import com.ezanmoto.apl.Type._

import java.io.BufferedReader
import java.io.InputStreamReader

class APLInterpreter {

  private var line = ""

  private var env = Map[String, Variable]()

  private var programs = Map[String, Program]()

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
        call( ( programs get name ) get )
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

  def call( program: Program ): Unit = program.lines.foreach( this interpret _ )

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
      else if ( env contains name )
        env = env - name
      else
        error( "Nothing named '" + name + "'" )
    } while ( ! in.isEmpty )
  }

  def program(): Unit = {
    in.eat( 'v' )
    in.skipWhitespace()
    val name = readName()
    in.skipWhitespace()
    if ( ! in.isEmpty && in.peek == '[' ) {
      programs get name match {
        case Some( program ) => 
          programs = programs + ( name -> programEdit( program ) )
        case None => error( "No program named '" + name + "'" )
      }
      in.skipWhitespace()
      in.eat( 'v' )
    } else if ( env.contains( name ) )
      error( "'" + name + "' is a variable" )
    else {
      val program = programs get name match {
        case Some( p ) => p
        case None      => new APLProgram( name )
      }
      programs = programs + ( name -> readProgramLines( program ) )
    }
  }

  def programEdit( program: Program ): Program = {
    in.eat( '[' )
    in.skipWhitespace()
    in.peek match {
      case Integer( _ ) => programLineNumber( program, integer() )
      case _ => programEdit_( program )
    }
  }

  def programLineNumber( program: Program, n: Int ): Program = {
    in.skipWhitespace()
    // TODO implement prompted replacement
    in.peek match {
      case ']' => replaceProgramLine( program, n )
      case 'b' => {
        if ( n > 0 && n <= program.lines.size ) {
          println( program.lines( n - 1 ) )
          in.eat( 'b' )
          in.skipWhitespace()
          in.eat( ']' )
          in.skipWhitespace()
          program
        } else
          error( "Index must be in range [1.." + program.lines.size + "]" )
      }
      case _ => unexpected()
    }
  }

  def replaceProgramLine( program: Program, n: Int ): Program = {
    in.eat( ']' )
    in.skipWhitespace()
    program replace ( n, readProgramLine() )
  }

  def readProgramLine(): String = {
    val isr      = new InputStreamReader( System.in )
    val reader   = new BufferedReader( isr )
    var l = ""
    while ( in.peek != 'v' )
      l += in.drop()
    l
  }

  def programEdit_( program: Program ): Program = {
    val result = in.peek match {
      case 'u' => in.eat( 'u' ); program deleteLine integer()
      case 'b' => in.eat( 'b' ); println( program ); program
      case _ => unexpected()
    }
    in.skipWhitespace()
    in.eat( ']' )
    in.skipWhitespace()
    result
  }

  def readProgramLines( program: Program ): Program = {
    val isr      = new InputStreamReader( System.in )
    val reader   = new BufferedReader( isr )
    var finished = false
    var lines: List[String] = Nil
    while ( ! finished ) {
      print( "[" + ( lines.length + 1 ) + "]   " )
      val input = reader.readLine()
      finished = input startsWith "v"
      if ( ! finished )
        lines = lines ::: List( input )
    }
    program ++ lines
  }
}
