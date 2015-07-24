package slamdata.engine.gen

import scala.collection.immutable.ListMap

import slamdata.engine.gen.txt.Data

/**
 * Inspired by https://github.com/dmitriy-yefremov/scala-code-gen
 */
object DataGen extends App {

  def generate(schema: DataSchema): String = Data(schema).toString()

  def write(schema: DataSchema) = {
    val content = generate(schema)

    val f = new java.io.File(args(0) + "/" + schema.name + ".scala")

    f.getParentFile.mkdirs()

    val os = new java.io.FileOutputStream(f)
    val w = new java.io.OutputStreamWriter(os, "UTF-8")
    w.write(content)
    w.close()

    f
  }

  val exprOp = DataSchema(
    "slamdata.engine.physical.mongodb",
    List("scalaz._"),
    "ExprOpGen",
    "ExpressionGen",
    "A",
    List(
      Instance("$include", ListMap()),
      Instance("$var",     ListMap("docVar" -> ParamType.Simple("ExprOp.DocVar"))),
      Instance("$and",     ListMap("values" -> ParamType.Generic("NonEmptyList"))),
      Instance("$add",     ListMap("left" -> ParamType.Rec, "right" -> ParamType.Rec))))

  val groupOp = DataSchema(
    "slamdata.engine",
    List(),
    "AccumOpGen",
    "AccumulatorGen",
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


  List(exprOp, groupOp).foreach { s =>
    println(write(s).getAbsolutePath())
  }
}
