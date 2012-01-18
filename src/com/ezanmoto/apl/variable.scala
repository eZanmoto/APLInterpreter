package com.ezanmoto.apl

import com.ezanmoto.apl.Type._

trait Variable {
  /** Abstract */
  def getType: Type
  def +( v: Variable ): Variable
  def -( v: Variable ): Variable
  def *( v: Variable ): Variable
  def /( v: Variable ): Variable
  def %( v: Variable ): Variable
  def ++( v: Variable ): Variable

  def ==( v: Variable ): Variable
  def !=( v: Variable ): Variable
  def < ( v: Variable ): Variable
  def <=( v: Variable ): Variable
  def > ( v: Variable ): Variable
  def >=( v: Variable ): Variable

  def max( v: Variable ): Variable
  def min( v: Variable ): Variable

  def at( index: Variable ): Variable
  def replace( index: Variable, value: Variable ): Variable

  /** Concrete */
  def isString  = getType == string
  def isInteger = getType == integer
  def isList    = getType == list
  def stringValue: String  = illegalCast( string )
  def integerValue: Int    = illegalCast( integer )
  def listValue: List[Int] = illegalCast( list )
  def illegalCast( t: Type ) = throw new RuntimeException(
    "Cannot cast from '" + getType + "' to '" + t + "'" )
}

object Variable {
  def apply( s: String    ) = new APLString( s )
  def apply( i: Int       ) = new APLInteger( i )
  def apply( l: List[Int] ) = new APLList( l )
}

