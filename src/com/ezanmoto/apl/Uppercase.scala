package com.ezanmoto.apl

object Uppercase {
  def unapply( c: Char ) = if ( c >= 'A' && c <= 'Z' ) Some( c ) else None
}

