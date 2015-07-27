package slamdata.engine.gen

import scala.collection.immutable.ListMap

import slamdata.engine.gen.txt.Data
import slamdata.engine.gen.txt.Fixpoint

/**
 * Inspired by https://github.com/dmitriy-yefremov/scala-code-gen
 */
object DataGen extends App {

  def generate(schema: DataSchema): String = Data(schema).toString()
  def generate(schema: FixpointSchema): String = Fixpoint(schema).toString()

  def handle(schema: DataSchema) = write(schema.name, generate(schema))
  def handle(schema: FixpointSchema) = write(schema.name, generate(schema))

  def write(name: String, content: String) = {
    val f = new java.io.File(args(0) + "/" + name + ".scala")

    f.getParentFile.mkdirs()

    val os = new java.io.FileOutputStream(f)
    val w = new java.io.OutputStreamWriter(os, "UTF-8")
    w.write(content)
    w.close()

    println(f.getAbsolutePath)
  }

  val exprOp = FixpointSchema(
    "slamdata.engine.physical.mongodb.exprop",
    List("scalaz._", "slamdata.engine.physical.mongodb.ExprOp.DocVar"),
    "ExprOpGen",
    // "ExpressionGen",
    "A",
    List(
      Instance("$include", ListMap()),
      Instance("$var",     ListMap("docVar" -> ParamType.Simple("DocVar"))),
      Instance("$and",     ListMap("values" -> ParamType.Generic("NonEmptyList"))),
      Instance("$add",     ListMap("left" -> ParamType.Rec, "right" -> ParamType.Rec))))

  val groupOp = DataSchema(
    "slamdata.engine.physical.mongodb.accumop",
    List(),
    "AccumOpGen",
    "A",
    List(
      Instance("$addToSet", ListMap("value" -> ParamType.Rec)),
      Instance("$push",     ListMap("value" -> ParamType.Rec)),
      Instance("$first",    ListMap("value" -> ParamType.Rec)),
      Instance("$last",     ListMap("value" -> ParamType.Rec)),
      Instance("$max",      ListMap("value" -> ParamType.Rec)),
      Instance("$min",      ListMap("value" -> ParamType.Rec)),
      Instance("$avg",      ListMap("value" -> ParamType.Rec)),
      Instance("$sum",      ListMap("value" -> ParamType.Rec))))


  handle(exprOp)
  handle(groupOp)
}
