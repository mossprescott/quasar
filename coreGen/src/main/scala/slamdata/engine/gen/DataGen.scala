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
      Instance("Include", "$instance", ListMap()),
      Instance("Var",     "$var",      ListMap("docVar" -> ParamType.Simple("ExprOp.DocVar"))),
      Instance("And",     "$and",      ListMap("values" -> ParamType.Generic("NonEmptyList"))),
      Instance("Add",     "$add",      ListMap("left" -> ParamType.Rec, "right" -> ParamType.Rec))))

  val groupOp = DataSchema(
    "slamdata.engine",
    List(),
    "AccumOpGen",
    "AccumulatorGen",
    "A",
    List(
      Instance("AddToSet", "$addToSet", ListMap("value" -> ParamType.Rec)),
      Instance("Push",     "$push",     ListMap("value" -> ParamType.Rec)),
      Instance("First",    "$first",    ListMap("value" -> ParamType.Rec)),
      Instance("Last",     "$last",     ListMap("value" -> ParamType.Rec)),
      Instance("Max",      "$max",      ListMap("value" -> ParamType.Rec)),
      Instance("Min",      "$min",      ListMap("value" -> ParamType.Rec)),
      Instance("Avg",      "$avg",      ListMap("value" -> ParamType.Rec)),
      Instance("Sum",      "$sum",      ListMap("value" -> ParamType.Rec))))


  List(exprOp, groupOp).foreach { s =>
    println(write(s).getAbsolutePath())
  }
}
