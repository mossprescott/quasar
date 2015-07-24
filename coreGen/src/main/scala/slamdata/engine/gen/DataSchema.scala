package slamdata.engine.gen

import scala.collection.immutable.ListMap

case class DataSchema(
  pkg: String,
  imports: List[String],
  name: String,
  fixed: String,
  param: String,
  instances: List[Instance])
{
  def decl = name + paramDecl
  def paramDecl = "[" + param + "]"
}

sealed trait ParamType
object ParamType {
  case class Simple(name: String) extends ParamType
  case object Rec extends ParamType
  case class Generic(name: String) extends ParamType
}

case class Instance(
  name: String,
  syntax: String,
  params: ListMap[String, ParamType])
{
  def matchResult = params.toList match {
    case Nil           => "true"
    case (n, _) :: Nil => s"Some($n)"
    case ps            => "Some((" + ps.map(_._1).mkString(", ") + "))"
  }
  def noMatchResult = params.toList match {
    case Nil => "false"
    case _   => "None"
  }

  private def show(rec: String, pt: ParamType) = pt match {
    case ParamType.Simple(name) => name
    case ParamType.Rec => rec
    case ParamType.Generic(name) => name + "[" + rec + "]"
  }

  def unapplyType(rec: String) = {
    params.toList match {
      case Nil => "Boolean"
      case (_, t) :: Nil => "Option[" + show(rec, t) + "]"
      case ps => "Option[(" + ps.map { case (_, t) => show(rec, t) }.mkString(", ") + ")]"
    }
  }

  def paramArgs(rec: String) = {
    params.toList.map {
      case (n, t) => n + ": " + show(rec, t)
    }.mkString(", ")
  }
}
