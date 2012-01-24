package com.ezanmoto.apl

trait CharacterKey {
  val QUAD: Char
  val RHO: Char
  val IOTA: Char
  val DEL: Char
  val DELTA: Char
  val MULTIPLY: Char
  val DIVIDE: Char
  val STILE: Char
  val MACRON: Char
  val UP_STILE: Char
  val DOWN_STILE: Char
  val LEFT_ARROW: Char
  val NOT_EQUAL: Char
  val LESS_THAN_OR_EQUAL: Char
  val GREATER_THAN_OR_EQUAL: Char
}

object QWERTY extends CharacterKey {
  val QUAD  = 'b'
  val RHO   = 'p'
  val IOTA  = 'i'
  val DEL   = 'v'
  val DELTA = 'u'
  val MULTIPLY  = 'x'
  val DIVIDE    = '%'
  val STILE     = '|'
  val MACRON    = '~'
  val UP_STILE   = 'r'
  val DOWN_STILE = '_'
  val LEFT_ARROW = ':'
  val NOT_EQUAL  = 'n'
  val LESS_THAN_OR_EQUAL    = 'l'
  val GREATER_THAN_OR_EQUAL = 'g'
}
