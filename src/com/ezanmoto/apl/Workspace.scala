package com.ezanmoto.apl

trait Workspace extends scala.Serializable {
  def name: String
  def rename( name: String ): Workspace
  def contains( name: String ): Boolean
  def +( name: String, value: Variable ): Workspace
  def +( name: String, value: Program  ): Workspace
  def -( name: String ): Workspace
  def get( name: String ): Either[Variable, Program]
  def getVariable( name: String ): Option[Variable]
  def getProgram( name: String ): Option[Program]
  def getMonadicProgram( name: String ): Program
}

object Workspace {
  def apply() = new APLWorkspace( "CLEAR WS" )
}

class APLWorkspace( val name: String
    , private val workspace: Map[String, Either[Variable, Program]]
    ) extends Workspace {

  def this( name: String ) =
    this( name, Map[String, Either[Variable, Program]]() )

  def contains( name: String ) = workspace contains name

  def get( name: String ) = workspace get name match {
    case Some( entry ) => entry
    case None =>
      throw new RuntimeException( "'" + name + "' has not been declared" )
  }

  def getVariable( name: String ): Option[Variable] =
    workspace get name match {
      case Some( Left( variable ) ) => Some( variable )
      case _ => None
    }

  def getProgram( name: String ): Option[Program] =
    workspace get name match {
      case Some( Right( program ) ) => Some( program )
      case _ => None
    }

  def getMonadicProgram( name: String ) = getProgram( name ) match {
    case Some( program ) =>
      if ( program isMonadic )
        program
      else
        throw new RuntimeException( "'" + name + "' is not monadic" )
    case None => throw new RuntimeException( "'" + name + "' is not a program" )
  }

  def +( n: String, v: Variable ): APLWorkspace =
    new APLWorkspace( name, workspace + ( n -> Left( v ) ) )

  def +( n: String, p: Program ): APLWorkspace =
    new APLWorkspace( name, workspace + ( n -> Right( p ) ) )

  def -( name: String ): APLWorkspace =
    new APLWorkspace( name, workspace - name )

  def rename( name: String ): APLWorkspace =
    new APLWorkspace( name, workspace )
}
