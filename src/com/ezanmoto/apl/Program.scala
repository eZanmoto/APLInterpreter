package com.ezanmoto.apl

trait Program {
  def lines: List[String]
  def ++( lines: List[String] ): Program
  def replace( n: Int, line: String ): Program
  def deleteLine( n: Int ): Program
}

class APLProgram( private var name: String,
                  var lines: List[String] ) extends Program {

  def this( name: String ) = this( name, Nil )

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
