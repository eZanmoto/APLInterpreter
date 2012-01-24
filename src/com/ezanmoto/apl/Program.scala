package com.ezanmoto.apl

trait Program {
  val isNiladic = ! isMonadic && ! isDyadic
  def isMonadic: Boolean
  def isDyadic: Boolean
  def name: String
  def returnName: String = throw new UnsupportedOperationException(
    "This type of program does not support returns" )
  def arg1Name: String = throw new UnsupportedOperationException(
    "This type of program does not support arguments" )
  def arg2Name: String = throw new UnsupportedOperationException(
    "This type of program does not support two arguments" )
  def lines: List[String]
  def ++( lines: List[String] ): Program
  def replace( n: Int, line: String ): Program
  def deleteLine( n: Int ): Program
}

class APLProgram( val name: String, val lines: List[String] ) extends Program {

  def this( name: String ) = this( name, Nil )

  val isMonadic = false
  val isDyadic  = false

  def ++( ls: List[String] ) = new APLProgram( name, lines ::: ls )

  def replace( n: Int, line: String ) =
    new APLProgram( name, ( lines take ( n - 1 ) )
                      ::: ( line :: ( lines drop n ) ) )

  def deleteLine( n: Int ) =
    new APLProgram( name, ( lines take ( n - 1 ) ) ::: ( lines drop n ) )

  override def toString(): String = {
    var string = "      v " + name + "\n"
    var i = 0
    for ( line <- lines ) {
      i += 1
      string += "[" + i + "]   " + line + "\n"
    }
    string + "      v\n"
  }
}

class MonadicProgram private ( private val program: Program
                             , override val returnName: String
                             , override val arg1Name: String
                             ) extends Program {

  def this( name: String, lines: List[String], returnName: String,
            arg1Name: String ) =
    this( new APLProgram( name, lines ), returnName, arg1Name )

  def this( name: String, returnName: String, arg1Name: String ) =
    this( name, Nil, returnName, arg1Name )

  val isMonadic = true
  val isDyadic  = false
  val name      = program name
  val lines     = program lines

  def ++( ls: List[String] ): MonadicProgram =
    new MonadicProgram( program ++ ls, returnName, arg1Name )

  def replace( n: Int, line: String ): MonadicProgram =
    new MonadicProgram( program replace ( n, line ), returnName, arg1Name )

  def deleteLine( n: Int ): MonadicProgram =
    new MonadicProgram( program deleteLine n, returnName, arg1Name )

  override def toString(): String = {
    var string = "      v " + returnName + " : " + name + " " + arg1Name + "\n"
    var i = 0
    for ( line <- lines ) {
      i += 1
      string += "[" + i + "]   " + line + "\n"
    }
    string + "      v\n"
  }
}

class DyadicProgram private ( private val program: MonadicProgram
                            , override val arg2Name: String
                            ) extends Program {

  def this( name: String, lines: List[String], returnName: String,
            arg1Name: String, arg2Name: String ) =
    this( new MonadicProgram( name, lines, returnName, arg1Name ), arg2Name )

  def this( name: String, returnName: String, arg1Name: String,
            arg2Name: String ) =
    this( name, Nil, returnName, arg1Name, arg2Name )

  val isMonadic  = false
  val isDyadic   = true
  val name       = program name
  val lines      = program lines

  override val returnName = program returnName
  override val arg1Name   = program arg1Name

  def ++( ls: List[String] ): DyadicProgram =
    new DyadicProgram( program ++ ls, arg2Name )

  def replace( n: Int, line: String ): DyadicProgram =
    new DyadicProgram( program replace ( n, line ), arg2Name )

  def deleteLine( n: Int ): DyadicProgram =
    new DyadicProgram( program deleteLine n, arg2Name )
}
