package com.ezanmoto.apl

object Integer {
  def unapply( c: Char ) = if ( c isDigit ) Some( c ) else None
}
