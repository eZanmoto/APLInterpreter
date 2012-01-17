package com.ezanmoto.apl

object APLInteger {
  def unapply( v: Variable ): Option[Int] =
    if ( v isInteger ) Some( v integerValue ) else None
}

class APLInteger( private val integer: Int ) extends Variable {
  val getType = Type.integer
  override def integerValue = integer

  private def math( f: (Int, Int) => Int )( v: Variable ): Variable = v match {
    case APLString( s )  => throw new RuntimeException( "Not implemented" )
    case APLInteger( i ) => Variable( f( integer, i ) )
    case APLList( l )    => Variable( l map ( f( integer, _ ) ) )
  }
  def +( v: Variable ) = math( _ + _ )( v )
  def -( v: Variable ) = math( _ - _ )( v )
  def *( v: Variable ) = math( _ * _ )( v )
  def /( v: Variable ) = math( _ / _ )( v )

  def ++( v: Variable ) = v match {
    case APLString( _ )  => throw new RuntimeException( "Not implemented" )
    case APLInteger( i ) => Variable( List( integer, i ) )
    case APLList( l )    => Variable( integer :: l )
  }

  def at( index: Variable ) = throw new RuntimeException( "Can't index int" )

  def replace( i: Variable, v: Variable ) =
    throw new RuntimeException( "Can't index into int" )

  override def toString = integer toString
}