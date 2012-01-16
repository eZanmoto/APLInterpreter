package com.ezanmoto.apl

import com.ezanmoto.apl.Type._

class APLInterpreter {

  private var env = Map[String, Variable]()

  private var in: LookaheadStream = new LookaheadStream( "" )

  var isRunning = true

  def interpret( line: String ): Unit = {
    this.in = new LookaheadStream( line )
    interpret(): Unit
  }

  def interpret(): Unit = {
    in.skipWhitespace()
    in.peek match {
      case '\'' | '~' | Integer( _ ) => println( expression() )
      case ':'  => runCommand()
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

  def expression(): Variable = expressionAfter( readValue() )

  def expressionAfter( a: Variable ): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty )
      a
    else
      in.peek match {
        case '+' => in eat '+'; expressionAfter( a +  readValue() )
        case '-' => in eat '-'; expressionAfter( a -  readValue() )
        case 'x' => in eat 'x'; expressionAfter( a *  readValue() )
        case '%' => in eat '%'; expressionAfter( a /  readValue() )
        case ',' => in eat ','; expressionAfter( a ++ readValue() )
        case '[' => expressionAfter( a at readIndex() )
        case Integer( _ ) =>
          if ( a isInteger )
            expressionAfter( Variable( readListAfter( a integerValue ) ) )
          else
            unexpected()
        case _   => a
      }
  }

  def readIndex(): Variable = {
    in eat '['
    val i = expression()
    in.skipWhitespace()
    in eat ']'
    i
  }

  def readListAfter( a: Int ): List[Int] = {
    in.skipWhitespace()
    if ( in.isEmpty )
      unexpected()
    else {
      var list = a :: Nil
      while ( ! in.isEmpty && in.peek.isDigit ) {
        list = list ::: List( readSignedInteger() )
        in.skipWhitespace()
      }
      list
    }
  }

  def readIntegerOrList(): Variable = {
    val a = readSignedInteger()
    in.skipWhitespace()
    if ( in.isEmpty )
      Variable( a )
    else {
      var list = a :: Nil
      while ( ! in.isEmpty && in.peek.isDigit ) {
        list = list ::: List( readSignedInteger() )
        in.skipWhitespace()
      }
      if ( list.length > 1 ) Variable( list ) else Variable( list head ) 
    }
  }

  def readValue(): Variable = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected '~', identifier or integer" )
    else
      in.peek match {
        case '\'' => Variable( readString() )
        case Uppercase( _ ) => {
          var value = lookup( readName() )
          in.skipWhitespace()
          if ( ! in.isEmpty && in.peek == '[' )
            value at readIndex()
          else
            value
        }
        case Integer( _ ) | '~' => readIntegerOrList()
        case _ => error( "Expected '~', identifier, integer or string" )
      }
  }

  def readSignedInteger(): Int = {
    in.skipWhitespace()
    if ( in.isEmpty )
      error( "Expected integer or '~'" )
    else {
      var isNegative = false
      if ( in.peek == '~' ) {
        isNegative = true
        in eat '~'
      }
      readInteger() * ( if ( isNegative ) -1 else 1 )
    }
  }

  def readInteger(): Int = {
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

  def assignment(): Unit = {
    val name = readName()
    in.skipWhitespace()
    val index =
      if ( ! in.isEmpty && in.peek == '[' ) Some( readIndex() ) else None
    in.skipWhitespace()
    if ( in.isEmpty ) { // Print value
      var value: Variable = lookup( name )
      if ( None != index )
        value = value at index.get
      println( value )
    } else if ( in.peek == '<' ) { // Assignment
      in.eat( "<-" )
      in.skipWhitespace()
      if ( index == None )
        env = env + ( name -> readRHS() )
      else if ( lookup( name ) isInteger )
        error( "cannot index integer" )
      else
        env = env + ( name -> indexAssignment( lookup( name ), index get ) )
    } else { // Start of expression
      var value: Variable = lookup( name )
      if ( None != index )
        value = value at index.get
      println( expressionAfter( value ) )
    }
  }

  def indexAssignment( value: Variable, index: Variable ): Variable = {
    val rhs = readRHS()
    if ( ( value at index ).getType == rhs.getType )
      value replace ( index, rhs )
    else
      error( "'" + rhs.getType + "' cannot be assigned to variable of type '"
           + value.getType + "'" )
  }

  def readRHS(): Variable = in.peek match {
    case '\'' => Variable( readString() )
    case Uppercase( _ ) | '~' | Integer( _ ) => expression()
    case v => unexpected()
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
}
