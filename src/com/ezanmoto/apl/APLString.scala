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
    case APLList( l ) => this.replace( l, value )
    case v => throw new RuntimeException( "Can't use '" + v + "' as an index" )
  }

  private def replace( i: Int, value: Variable ): APLString =
  value match {
    case APLString( s ) =>
      if ( i > 0 && i <= string.length )
        Variable( string.substring( 0, i - 1 ) + s + string.substring( i ) )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + string.length + "]" )
    case v => throw new RuntimeException( "Can't replace chr with '" + v + "'" )
  }

  private def replace( indices: List[Int], value: Variable ): APLString =
  value match {
    case APLString( characters ) =>
      if ( characters.length == 1 ) {
        // TODO ensure this is the correct behaviour
        var c = Variable( characters )
        var result = this
        for ( i <- indices )
          result = result replace( i, c )
        result
      } else if ( indices.length == characters.length ) {
        var result = this
        for ( i <- 0 to ( indices.length - 1 ) ) {
          var c = Variable( characters substring ( i, i + 1 ) )
          result = result replace( indices( i ), c )
        }
        result
      } else
        throw new RuntimeException( "Number of indices does not match number "
                                  + "of replacements" )
    case v => throw new RuntimeException( "Can't replace chr with '" + v + "'" )
  }

  override def toString = string
}

