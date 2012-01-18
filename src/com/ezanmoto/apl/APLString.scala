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
  def %( v: Variable ) = throw new RuntimeException( "Not implemented yet" )

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
        var result = this
        for ( i <- indices )
          result = result replace( i, value )
        result
      } else if ( indices.length == characters.length ) {
        var result = this
        for ( i <- 0 to ( indices.length - 1 ) ) {
          val c = Variable( characters substring ( i, i + 1 ) )
          result = result replace( indices( i ), c )
        }
        result
      } else
        throw new RuntimeException( "Number of indices does not match number "
                                  + "of replacements" )
    case v => throw new RuntimeException( "Can't replace chr with '" + v + "'" )
  }

  def ==( v: Variable ) = relation( _ == _, v )
  def !=( v: Variable ) = relation( _ != _, v )
  def < ( v: Variable ) = relation( _ <  _, v )
  def <=( v: Variable ) = relation( _ <= _, v )
  def > ( v: Variable ) = relation( _ >  _, v )
  def >=( v: Variable ) = relation( _ >= _, v )

  // Nastiness personified in a function
  // This is just a project, don't feel too bad, you can clean it up later
  private def relation( f: ( (Char, Char) => Boolean ), v: Variable ) =
    relation_( ( a, b ) => if ( f( a, b ) ) 1 else 0, v )

  private def relation_( f: ( (Char, Char) => Int ), v: Variable ): Variable =
  v match {
    case APLString( characters ) =>
      if ( string.length == 1 && characters.length == 1 )
        Variable( f( string charAt 0, characters charAt 0 ) )
      if ( characters.length == 1 ) {
        val char = characters charAt 0
        var result: List[Int] = Nil
        for ( c <- stringValue )
          result = result ::: List( f( c, char ) )
        Variable( result )
      } else if ( string.length == 1 ) {
        // TODO ensure this is what is meant to happen
        new APLString( characters ) relation_( f, this )
      } else if ( string.length == characters.length ) {
        var result: List[Int] = Nil
        for ( i <- 0 to string.length - 1 )
          result = result ::: List( f( string charAt i, characters charAt i ) )
        Variable( result )
      } else
        throw new RuntimeException( "Number of indices does not match number "
                                  + "of replacements" )
    case v => throw new RuntimeException( "Can't compare chr with '" + v + "'" )
  }

  def max( v: Variable ) = throw new RuntimeException( "Not implemented yet" )
  def min( v: Variable ) = throw new RuntimeException( "Not implemented yet" )

  override def toString = string
}

