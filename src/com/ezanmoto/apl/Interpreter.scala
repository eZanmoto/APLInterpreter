package com.ezanmoto.apl

import com.ezanmoto.apl.char.APLLayout
import com.ezanmoto.apl.char.CharacterKey
import com.ezanmoto.apl.char.QWERTY

import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.Scanner

object Interpreter {

  private val debugging = true

  private var running = true

  def main( args: Array[String] ) = {
    val interpreter = new APLInterpreter( getKeyType() )
    val reader = new InputStreamReader( System.in )
    val in = new BufferedReader( reader )
    println( "CLEAR WS" )
    while ( interpreter isRunning ) {
      try {
        print( "      " )
        interpreter.interpret( in.readLine() )
      } catch {
        case e => println( if ( debugging ) e else e.getMessage )
      }
    }
    in.close()
  }

  def getKeyType(): CharacterKey = {
    println( "Please enter keyboard type:\n\n"
           + "1: QWERTY\n"
           + "2: APL\n" )
    print( "> " )
    val scanner = new Scanner( System.in )
    if ( scanner hasNextInt ) {
      scanner nextInt match {
        case 1 => QWERTY
        case 2 => APLLayout
        case _ => throw new RuntimeException( "Not a keyboard type" );
      }
    } else {
      throw new RuntimeException( "Need an integer" );
    }
  }
}
