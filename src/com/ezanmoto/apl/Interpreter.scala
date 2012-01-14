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

object Type extends Enumeration {
  type Type = Value
  val string, integer, list = Value
}

import com.ezanmoto.apl.Type._

trait Variable {
  /** Abstract */
  def getType: Type
  def +( v: Variable ): Variable
  def -( v: Variable ): Variable
  def *( v: Variable ): Variable
  def /( v: Variable ): Variable
  def ++( v: Variable ): Variable
  def before( index: Variable ): Variable
  def at( index: Variable ): Variable
  def after( index: Variable ): Variable

  /** Concrete */
  def isString  = getType == string
  def isInteger = getType == integer
  def isList    = getType == list
  def stringValue: String  = illegalCast( string  )
  def integerValue: Int    = illegalCast( integer )
  def listValue: List[Int] = illegalCast( list    )
  def illegalCast( t: Type ) = throw new RuntimeException(
    "Cannot cast from '" + getType + "' to '" + t + "'" )
}

object Variable {
  def apply( s: String    ) = new StringVariable( s )
  def apply( i: Int       ) = new IntegerVariable( i )
  def apply( l: List[Int] ) = new ListVariable( l )
}

class StringVariable( private val string: String ) extends Variable {
  val getType = Type.string
  override def stringValue = string
  def +( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def -( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def *( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def /( v: Variable ) = throw new RuntimeException( "Not implemented yet" )

  def ++( v: Variable ) = v match {
    case StringVariable( s ) => Variable( string + s )
    case _ => throw new RuntimeException( "Not implemented yet" )
  }

  private def get( f: ( Int => String ) )( index: Variable ): Variable =
  index match {
    case IntegerVariable( i ) =>
      if ( i <= string.length )
        Variable( f( i ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  def before( index: Variable ) = get( string substring ( 0, _ ) )( index )

  def at( index: Variable ) = index match {
    case IntegerVariable( i ) =>
      if ( string isDefinedAt ( i - 1 ) )
        Variable( String valueOf ( string charAt ( i - 1 ) ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  def after( index: Variable ) = get( i => string substring ( i + 1 ) )( index )

  override def toString = string
}

object StringVariable {
  def unapply( v: Variable ): Option[String] =
    if ( v isString ) Some( v stringValue ) else None
}

class IntegerVariable( private val integer: Int ) extends Variable {
  val getType = Type.integer
  override def integerValue = integer

  private def math( f: (Int, Int) => Int )( v: Variable ): Variable = v match {
    case StringVariable( s )  => throw new RuntimeException( "Not implemented" )
    case IntegerVariable( i ) => Variable( f( integer, i ) )
    case ListVariable( l )    => Variable( l map ( f( integer, _ ) ) )
  }
  def +( v: Variable ) = math( _ + _ )( v )
  def -( v: Variable ) = math( _ - _ )( v )
  def *( v: Variable ) = math( _ * _ )( v )
  def /( v: Variable ) = math( _ / _ )( v )

  def ++( v: Variable ) = v match {
    case StringVariable( _ )  => throw new RuntimeException( "Not implemented" )
    case IntegerVariable( i ) => Variable( List( integer, i ) )
    case ListVariable( l )    => Variable( integer :: l )
  }

  def before( index: Variable ) = throw new RuntimeException( "Can't index" )
  def at( index: Variable ) = throw new RuntimeException( "Can't index int" )
  def after( index: Variable ) = throw new RuntimeException( "Can't index int" )

  override def toString = integer toString
}

object IntegerVariable {
  def unapply( v: Variable ): Option[Int] =
    if ( v isInteger ) Some( v integerValue ) else None
}

class ListVariable( private val list: List[Int] ) extends Variable {
  val getType = Type.list
  override def listValue = list

  private def math( f: (Int, Int) => Int )( v: Variable ): Variable = v match {
    case StringVariable( s )  => throw new RuntimeException( "Not implemented" )
    case IntegerVariable( i ) => Variable( list map ( f( i, _ ) ) )
    case ListVariable( l ) => Variable( ( list, l ).zipped map ( f( _, _ ) ) )
  }
  def +( v: Variable ) = math( _ + _ )( v )
  def -( v: Variable ) = math( _ - _ )( v )
  def *( v: Variable ) = math( _ * _ )( v )
  def /( v: Variable ) = math( _ / _ )( v )

  def ++( v: Variable ) = v match {
    case StringVariable( _ )  => throw new RuntimeException( "Not implemented" )
    case IntegerVariable( i ) => Variable( list ::: List( i ) )
    case ListVariable( l )    => Variable( list ::: l )
  }

  private def get( f: ( Int => List[Int] ) )( index: Variable ): Variable =
  index match {
    case IntegerVariable( i ) =>
      if ( i <= list.length )
        Variable( f( i ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + list.length + "]" )
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  def before( index: Variable ) = get( i => list take ( i - 1 ) )( index )

  def at( index: Variable ) = index match {
    case IntegerVariable( i ) =>
      if ( list isDefinedAt ( i - 1 ) )
        Variable( list( i - 1 ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + list.length + "]" )
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  def after( index: Variable ) = get( list drop _ )( index )

  override def toString = list toString
}

object ListVariable {
  def unapply( v: Variable ): Option[List[Int]] =
    if ( v isList ) Some( v listValue ) else None
}

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
        case Uppercase( _ ) => lookup( readName() )
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
    if ( ( value at index ).getType == rhs.getType ) {
      rhs match {
        case StringVariable( s ) =>
          Variable( ( value before index ).stringValue + s
                  + ( value after index ).stringValue )
        case ListVariable( l ) => error( "Can't assign list to index" )
        case IntegerVariable( i ) =>
          Variable( ( value before index ).listValue
                  ::: ( i :: ( value after index ).listValue ) )
      }
    } else
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
