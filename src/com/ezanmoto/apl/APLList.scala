package com.ezanmoto.apl

object APLList {
  def unapply( v: Variable ): Option[List[Int]] =
    if ( v isList ) Some( v listValue ) else None
}

class APLList( private val list: List[Int] ) extends Variable {
  val getType = Type.list
  override def listValue = list

  private def math( f: (Int, Int) => Int )( v: Variable ): Variable = v match {
    case APLString( s )  => throw new RuntimeException( "Not implemented" )
    case APLInteger( i ) => Variable( list map ( f( i, _ ) ) )
    case APLList( l ) => Variable( ( list, l ).zipped map ( f( _, _ ) ) )
  }
  def +( v: Variable ) = math( _ + _ )( v )
  def -( v: Variable ) = math( _ - _ )( v )
  def *( v: Variable ) = math( _ * _ )( v )
  def /( v: Variable ) = math( _ / _ )( v )

  def ++( v: Variable ) = v match {
    case APLString( _ )  => throw new RuntimeException( "Not implemented" )
    case APLInteger( i ) => Variable( list ::: List( i ) )
    case APLList( l )    => Variable( list ::: l )
  }

  def at( index: Variable ) = index match {
    case APLInteger( i ) => Variable( this at i )
    case APLList( indices )    => {
      var l: List[Int] = Nil
      for ( i <- indices )
        l = l ::: List( this at i )
      Variable ( l )
    }
    case v => throw new RuntimeException( "can't use '" + v + "' as an index" )
  }

  private def at( i: Int ): Int = 
      if ( list isDefinedAt ( i - 1 ) )
        list( i - 1 )
      else
        throw new RuntimeException( "'" + i + "' ! [1.." + list.length + "]" )

  def replace( index: Variable, value: Variable ) = index match {
    case APLInteger( i ) => this.replace( i, value )
    case APLList( l ) => this.replace( l, value )
    case v => throw new RuntimeException( "Can't use '" + v + "' as an index" )
  }

  private def replace( i: Int, value: Variable ): APLList = value match {
    case APLInteger( v ) =>
      if ( i > 0 && i <= list.length ) {
        Variable( ( list take ( i - 1 ) ) ::: ( v :: ( list drop i ) ) )
      } else
        throw new RuntimeException( "'" + i + "' ! [1.." + list.length + "]" )
    case v => throw new RuntimeException( "Can't replace int with '" + v + "'" )
  }

  private def replace( indices: List[Int], value: Variable ): APLList =
  value match {
    case APLInteger( v ) => {
      var result = this
      for ( index <- indices )
        result = result replace ( index, value )
      result
    }
    case APLList( l ) =>
      if ( indices.length == l.length ) {
        var result = this
        for ( i <- 0 to ( indices.length - 1 ) )
          result = result replace( indices( i ), Variable( l( i ) ) )
        result
      } else
        throw new RuntimeException( "Number of indices does not match number "
                                  + "of replacements" )
  }

  override def toString = list toString
}
