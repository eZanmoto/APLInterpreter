package com.ezanmoto.apl

trait Program {
  val isNiladic = ! isMonadic && ! isDyadic
  def isMonadic: Boolean
  def isDyadic: Boolean
  def returnName: String = throw new UnsupportedOperationException()
  def arg1Name: String = throw new UnsupportedOperationException()
  def arg2Name: String = throw new UnsupportedOperationException()
  def lines: List[String]
  def ++( lines: List[String] ): Program
  def replace( n: Int, line: String ): Program
  def deleteLine( n: Int ): Program
}

class APLProgram( private var name: String,
                  var lines: List[String] ) extends Program {

  def this( name: String ) = this( name, Nil )

  val isMonadic = false

  val isDyadic = false

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

class MonadicProgram( private val name: String
                    , val lines: List[String]
                    , override val returnName: String
                    , override val arg1Name: String
                    ) extends Program {

  val isMonadic = true

  val isDyadic = false

  def ++( ls: List[String] ): MonadicProgram =
    new MonadicProgram( name, lines ::: ls, returnName, arg1Name )

  def replace( n: Int, line: String ): MonadicProgram =
    new MonadicProgram( name, ( lines take ( n - 1 ) )
                      ::: ( line :: ( lines drop n ) ), returnName, arg1Name )

  def deleteLine( n: Int ): MonadicProgram =
    new MonadicProgram( name, ( lines take ( n - 1 ) ) ::: ( lines drop n )
                      , returnName, arg1Name )

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

class DyadicProgram( private val name: String
                   , val lines: List[String]
                   , override val returnName: String
                   , override val arg1Name: String
                   , override val arg2Name: String
                   ) extends Program {

  override val isMonadic = false

  override val isDyadic = true

  def ++( ls: List[String] ): DyadicProgram =
    new DyadicProgram( name, lines ::: ls, returnName, arg1Name, arg2Name )

  def replace( n: Int, line: String ): DyadicProgram =
    new DyadicProgram( name
                     , ( lines take ( n - 1 ) ) ::: ( line :: ( lines drop n ) )
                     , returnName, arg1Name, arg2Name )

  def deleteLine( n: Int ): DyadicProgram =
    new DyadicProgram( name, ( lines take ( n - 1 ) ) ::: ( lines drop n )
                     , returnName, arg1Name, arg2Name )
}
