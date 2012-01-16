package com.ezanmoto.apl

import java.io.BufferedReader
import java.io.InputStreamReader

object Interpreter {

  private var running = true

  def main( args: Array[String] ) = {
    val reader = new InputStreamReader( System.in )
    val in = new BufferedReader( reader )
    val interpreter = new APLInterpreter
    println( "CLEAR WS" )
    while ( interpreter isRunning ) {
      try {
        print( "      " )
        interpreter.interpret( in.readLine() )
      } catch {
        case e => println( e.getMessage )
      }
    }
  }
}
