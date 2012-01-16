package com.ezanmoto.apl

object APLString {
  def unapply( v: Variable ): Option[String] =
    if ( v isString ) Some( v stringValue ) else None
}

class APLString( private val string: String ) extends Variable {
  val getType = Type.string
  override def stringValue = string
  def +( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def -( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def *( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def /( v: Variable ) = throw new RuntimeException( "Not implemented yet" )

  def ++( v: Variable ) = v match {
    case APLString( s ) => Variable( string + s )
    case _ => throw new RuntimeException( "Not implemented yet" )
  }

  private def get( f: ( Int => String ) )( index: Variable ): Variable =
  index match {
    case APLInteger( i ) =>
      if ( i <= string.length )
        Variable( f( i ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  def at( index: Variable ) = index match {
    case APLInteger( i ) => Variable( this at i )
    case APLList( indices )    => {
      var s = ""
      for ( i <- indices )
        s += this at i
      Variable( s )
    }
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  private def at( i: Int ): String = 
    if ( string isDefinedAt ( i - 1 ) )
      String valueOf ( string charAt ( i - 1 ) )
    else
      throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )

  def replace( index: Variable, value: Variable ) = index match {
    case APLInteger( i ) => this.replace( i, value )
    case APLList( l ) => throw new RuntimeException( "Not implemented yet" )
    case v => throw new RuntimeException( "Can't use '" + v + "' as an index" )
  }

  private def replace( i: Int, value: Variable ) = value match {
    case APLString( s ) =>
      if ( i <= string.length ) {
        Variable( string.substring( 0, i ) + s + string.substring( i + 1 ) )
      } else
        throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )
    case v => throw new RuntimeException( "Can't replace chr with '" + v + "'" )
  }

  override def toString = string
}

