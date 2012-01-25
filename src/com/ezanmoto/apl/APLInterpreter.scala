package com.ezanmoto.apl

import com.ezanmoto.apl.Type._
import com.ezanmoto.apl.char.CharacterKey

import java.io.BufferedReader
import java.io.InputStreamReader

class APLInterpreter( private val key: CharacterKey ) {

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
        case '(' | '\'' | key.MACRON | Integer( _ )
           | key.IOTA | key.RHO | '+' => println( expression() )
        case ')' => in.eat( ')' ); command()
        case key.DEL => program()
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
      else if ( isNiladicProgram( name ) )
        call( ( programs get name ) get )
      else {
        var value: Variable = lookup( name )
        if ( None != index )
          value = value at index.get
        println( value )
      }
    } else if ( in.peek == key.LEFT_ARROW ) { // Assignment
      in.eat( key.LEFT_ARROW )
      in.skipWhitespace()
      if ( index == None )
        env = env + ( name -> expression() )
      else {
        val replacement = lookup( name ) replace ( index get, expression() )
        env = env + ( name -> replacement )
      }
    } else { // Start of expression
      var value: Variable = 
        if ( isProgram( name ) && isVariable( name ) )
          error( "Program and variable called '" + name + "'" )
        else if ( isMonadicProgram( name ) )
          call( programs get name get, expression() )
        else
          lookup( name )
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

  def isXProgram( isX: (Program) => Boolean )( name: String ) =
    programs get name match {
      case Some( program ) => isX( program )
      case None => false
    }

  val isNiladicProgram = isXProgram( _.isNiladic ) _
  val isMonadicProgram = isXProgram( _.isMonadic ) _
  val isDyadicProgram  = isXProgram( _.isDyadic  ) _

  def isProgram( name: String ) = programs contains name

  def isVariable( name: String ) = env contains name

  def call( program: Program ): Unit = program.lines.foreach( this interpret _ )

  def call( program: Program, parameter: Variable ): Variable = {
    val backup = env
    env = env + ( program.arg1Name -> parameter )
    call( program )
    val result = env get program.returnName match {
      case Some( value ) => value
      case None => error( "Return value '" + program.returnName + "' not set" )
    }
    env = backup
    result
  }

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
      error( "Expected '" + key.MACRON + "', identifier or integer" )
    else
      in.peek match {
        case '(' => {
          in.eat( '(' )
          val e = expression()
          in.eat( ')' )
          expressionAfter( e )
        }
        case '\'' => Variable( string() )
        case Uppercase( _ ) => {
          val name = readName()
          if ( isProgram( name ) && isVariable( name ) )
            error( "Program and variable called '" + name + "'" )
          else if ( isMonadicProgram( name ) )
            call( programs get name get, expression() )
          else
            lookup( name )
        }
        case key.MACRON | Integer( _ ) => integerOrListAfter( signedInteger() )
        case _ => unaryFunction()
      }
  }

  def unaryFunction(): Variable = in.peek match {
    case key.IOTA => in.eat( key.IOTA ); expression() interval
    case key.RHO  => in.eat( key.RHO  ); Variable( expression() length )
    case '+' => in.eat( "+/" ); expression() sum
    case _   => unexpected()
  }

  def integerOrListAfter( integer: Int ): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty || ( ! in.peek.isDigit && in.peek != key.MACRON ) )
      Variable( integer )
    else {
      var list = integer :: Nil
      do {
        list = list ::: List( signedInteger() )
        in.skipWhitespace()
      } while ( ! in.isEmpty && ( in.peek.isDigit || in.peek == key.MACRON ) )
      Variable( list )
    }
  }

  def signedInteger(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected integer or '" + key.MACRON + "'" )
    else {
      var isNegative = in.peek == key.MACRON
      if ( isNegative ) in.eat( key.MACRON )
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
    case key.MULTIPLY =>
      in.eat( key.MULTIPLY ); expressionAfter( a * expression() )
    case key.DIVIDE => in.eat( key.DIVIDE ); expressionAfter( a / expression() )
    case key.STILE  => in.eat( key.STILE  ); expressionAfter( a % expression() )
    case _   => concatenation( a )
  }

  def concatenation( a: Variable ): Variable = in.peek match {
    case ',' => in.eat( ',' ); expressionAfter( a ++ expression() )
    case _   => comparison( a )
  }

  def comparison( a: Variable ): Variable = in.peek match {
    case '=' => in eat '='; expressionAfter( a == expression() )
    case '<' => in eat '<'; expressionAfter( a <  expression() )
    case '>' => in eat '>'; expressionAfter( a >  expression() )
    case key.NOT_EQUAL =>
      in.eat( key.LESS_THAN_OR_EQUAL ); expressionAfter( a != expression() )
    case key.LESS_THAN_OR_EQUAL =>
      in.eat( key.LESS_THAN_OR_EQUAL ); expressionAfter( a <= expression() )
    case key.GREATER_THAN_OR_EQUAL =>
      in.eat( key.GREATER_THAN_OR_EQUAL ); expressionAfter( a >= expression() )
    case _   => minimax( a )
  }

  def minimax( a: Variable ): Variable = in.peek match {
    case key.UP_STILE =>
      in.eat( key.UP_STILE ); expressionAfter( a max expression() )
    case key.DOWN_STILE =>
      in.eat( key.DOWN_STILE ); expressionAfter( a min expression() )
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
    in.eat( key.DEL )
    in.skipWhitespace()
    var name = readName()
    in.skipWhitespace()
    if ( ! in.isEmpty && in.peek == '[' ) {
      programs get name match {
        case Some( program ) => 
          programs = programs + ( name -> programEdit( program ) )
        case None => error( "No program named '" + name + "'" )
      }
      in.skipWhitespace()
      in.eat( key.DEL )
    } else if ( env.contains( name ) )
      error( "'" + name + "' is a variable" )
    else {
      val program = programs get name match {
        case Some( p ) => p
        case None =>
          if ( ! in.isEmpty && in.peek == key.LEFT_ARROW ) {
            in.eat( key.LEFT_ARROW )
            in.skipWhitespace()
            val resultName = name
            name = readName()
            in.skipWhitespace()
            val argName = readName()
            new MonadicProgram( name, Nil, resultName, argName )
          } else
            new APLProgram( name )
      }
      programs = programs + ( name -> ( program ++ readProgramLines() ) )
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
      case key.QUAD => {
        if ( n > 0 && n <= program.lines.size ) {
          println( program.lines( n - 1 ) )
          in.eat( key.QUAD )
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
    while ( in.peek != key.DEL )
      l += in.drop()
    l
  }

  def programEdit_( program: Program ): Program = {
    val result = in.peek match {
      case key.DELTA => in.eat( key.DELTA ); program deleteLine integer()
      case key.QUAD  => in.eat( key.QUAD  ); println( program ); program
      case _ => unexpected()
    }
    in.skipWhitespace()
    in.eat( ']' )
    in.skipWhitespace()
    result
  }

  def readProgramLines(): List[String] = {
    val isr      = new InputStreamReader( System.in )
    val reader   = new BufferedReader( isr )
    var finished = false
    var lines: List[String] = Nil
    while ( ! finished ) {
      print( "[" + ( lines.length + 1 ) + "]   " )
      val input = reader.readLine()
      if ( input startsWith ( String valueOf key.DEL ) ) {
        finished = true
        if ( input.length > 1 )
          error( "Trailing characters after '" + key.DEL + "'" )
      } else if ( input endsWith ( String valueOf key.DEL ) ) {
        finished = true
        lines = lines ::: List( input take ( input.length - 1 ) )
      } else
        lines = lines ::: List( input )
    }
    lines
  }
}
