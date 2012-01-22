package com.ezanmoto.apl

class LookaheadStream( private var string: String ) {

  var eaten = 0

  def isEmpty = string.length == 0

  def peek: Char =
    if ( this isEmpty )
      throw new RuntimeException( "Cannot peek beyond end of stream" )
    else
      string.head

  def eat( c: Char ): Unit =
    if ( this.isEmpty )
      throw new IllegalArgumentException( "Expected '" + c + "'" )
    else if ( c == peek ) {
      eaten += 1
      string = string drop 1
    } else
      throw new IllegalArgumentException(
          "Expected '" + c + "', got '" + peek + "'" )

  def eat( s: String ): Unit = s.foreach( c => this eat c )

  def drop(): Char = {
    val c = peek
    skip()
    c
  }

  def skip(): Unit = {
    eaten += 1
    string = string drop 1
  }

  def skipWhitespace(): Unit =
      while ( ! this.isEmpty && peek.isWhitespace )
        skip()
}

