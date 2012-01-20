package com.ezanmoto.apl

object APLInteger {
  def unapply( v: Variable ): Option[Int] =
    if ( v isInteger ) Some( v integerValue ) else None
}

class APLInteger( private val integer: Int ) extends Variable {
  val getType = Type.integer
  override def integerValue = integer

  private def math( f: (Int, Int) => Int )( v: Variable ): Variable = v match {
    case APLInteger( i ) => Variable( f( integer, i ) )
    case v => throw new RuntimeException( "Not implemented" )
  }

  def +( v: Variable ) = if ( v isList ) v + this else math( _ + _ )( v )
  def -( v: Variable ) = if ( v isList ) v - this else math( _ - _ )( v )
  def *( v: Variable ) = if ( v isList ) v * this else math( _ * _ )( v )
  def /( v: Variable ) = if ( v isList ) v / this else math( _ / _ )( v )
  def %( v: Variable ) = if ( v isList ) v % this else math( _ % _ )( v )

  def ++( v: Variable ) = v match {
    case APLString( _ )  => throw new RuntimeException( "Not implemented" )
    case APLInteger( i ) => Variable( List( integer, i ) )
    case APLList( l )    => Variable( integer :: l )
  }

  def at( index: Variable ) = throw new RuntimeException( "Can't index int" )

  def replace( i: Variable, v: Variable ) =
    throw new RuntimeException( "Can't index into int" )

  def ==( v: Variable ) = if ( v isList ) v == this else relation( _ == _, v )
  def !=( v: Variable ) = if ( v isList ) v == this else relation( _ != _, v )
  def < ( v: Variable ) = if ( v isList ) v == this else relation( _ <  _, v )
  def <=( v: Variable ) = if ( v isList ) v == this else relation( _ <= _, v )
  def > ( v: Variable ) = if ( v isList ) v == this else relation( _ >  _, v )
  def >=( v: Variable ) = if ( v isList ) v == this else relation( _ >= _, v )

  private def relation( f: ( (Int, Int) => Boolean ), v: Variable ) = v match {
    case APLInteger( i ) => Variable( if ( f( integer, i ) ) 1 else 0 )
    case v =>
      throw new RuntimeException( "Cannot compare integer to '" + v + "'" )
  }

  private def best( f: (Int, Int) => Int, v: Variable ) = v match {
    case APLInteger( i ) => Variable( f( integer, i ) )
    case v => throw new RuntimeException( "Can't compare int with '" + v + "'" )
  }

  def max( v: Variable ) =
    if ( v isList )
      v max this
    else
      best( ( a, b ) => if ( a > b ) a else b, v )

  def min( v: Variable ) =
    if ( v isList )
      v min this
    else
      best( ( a, b ) => if ( a < b ) a else b, v )

  def length = throw new RuntimeException( "Can't get length of integer" )

  def sum: Variable = this

  override val toString = integer toString
}
