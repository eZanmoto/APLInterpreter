package com.ezanmoto.apl

import java.io.InputStream

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

  def clear(): Unit =
    while( stream.available > 0 )
      getNext
}

object Interpreter {

  private var running = true

  def main( args: Array[String] ) = {
    println( "CLEAR WS" )
    while ( running ) {
      print( "      " )
      read( new CharStream( System.in ) )
    }
  }

  def read( in: CharStream ) = {
    skipWhitespace( in )
    ( in peek ) match {
      case '\'' => this readString in; 
      case ':'  => this readCommand in
      case _    => this error in
    }
  }

  def error( in: CharStream ) = {
    in.clear()
    println( "[!] Error" )
  }

  def readString( in: CharStream ) = {
    in eat '\''
    var buffer = ""
    var c = in.peek
    while ( c != '\'' ) {
      buffer = buffer + c
      in eat c
      c = in.peek
    }
    println( buffer )
    in eat '\''
  }

  def skipLine( in: CharStream ) = {
    var c = in.peek
    while ( c != '\n' && c != '\r' ) {
      in eat c
      c = in.peek
    }
    in eat c
  }

  def skipWhitespace( in: CharStream ) = {
    var c = in.peek
    while ( this isWhitespace c ) {
      in eat c
      c = in.peek
    }
  }

  def isWhitespace( c: Char ) = c match {
    case ' ' | '\t' | '\n' => true
    case _ => false
  }

  def readCommand( in: CharStream ) = {
    in eat ':'
    ( in peek ) match {
      case 'q' => in eat 'q'; println( "Goodbye." ); running = false
      case _   => this error in
    }
  }
}

object Character {
  def unapply( c: Char ) = Some( c )
}
