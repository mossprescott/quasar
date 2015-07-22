package slamdata.engine.gen

import scala.collection.immutable.ListMap

import slamdata.engine.gen.txt.Data

/**
 * Inspired by https://github.com/dmitriy-yefremov/scala-code-gen
 */
object DataGen extends App {

  def generate(schema: DataSchema): String = Data(schema).toString()

  /*
  To be real, this generator would have to write the resulting sources somewhere where the
  `core` compile would be able to see them. There seem to be two approaches:
  - Just write the files somewhere under core/target, and configure it to look there.
  - Actually invoke the generator from SBT, writing the result to a temp file, and hand
  the name of that file directly to scalac using SBT's `sourceGenerators` mechanism.
  See http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator

  Probably the second approach is cleaner, and as long as the files are *somewhere* so
  you can read them to debug the generator or just understand the generated types, it
  probably doesn't matter.

  One way or the other, it seems like the "schema" should be defined within the `core`
  project, which gets a bit tricky (because it needs to be loaded *before* the compiler
  runs). But this schema type is pretty stringly already, so some sort of simple hack
  would be sufficient.
   */

  // Silly demo:

  val exprOp = DataSchema(
    "slamdata.engine",
    "ExprOpGen",
    "ExpressionGen",
    "A",
    List(
      Instance("Include", "$instance", ListMap()),
      Instance("Var",     "$var",      ListMap("docVar" -> ParamType.Simple("DocVar"))),
      Instance("And",     "$and",      ListMap("left" -> ParamType.Rec, "right" -> ParamType.Rec))))

  println(generate(exprOp))

  /* Results:

package slamdata.engine

sealed trait ExprOpGen[A]

object ExprOpGen {
  type ExpressionGen = Term[ExprOpGen]

  object Types {
    final case class Include[A]() extends ExprOpGen[A]
    final case class Var[A](docVar: Simple(DocVar)) extends ExprOpGen[A]
    final case class And[A](left: Recright: Rec) extends ExprOpGen[A]
  }

  object DSL {
    object $instance {
      def apply(): ExpressionGen = Term($instanceF())
      def unapply(obj: ExpressionGen): Boolean = $instanceF.unapply(obj.unFix)
    }

    object $var {
      def apply(docVar: Simple(DocVar)): ExpressionGen = Term($varF(docVar))
      def unapply(obj: ExpressionGen): Option[DocVar] = $varF.unapply(obj.unFix)
    }

    object $and {
      def apply(left: Recright: Rec): ExpressionGen = Term($andF(leftright))
      def unapply(obj: ExpressionGen): Option[(A, A)] = $andF.unapply(obj.unFix)
    }
  }

  import Types._


  object $instanceF {
    def apply[A](): ExprOpGen[A] = Term($instanceF())
    def unapply[A](obj: ExprOpGen[A]): Boolean = obj.match {
      case Include() => true
      case _ => false
    }
  }

  object $varF {
    def apply[A](docVar: DocVar): ExprOpGen[A] = Term($varF(docVar))
    def unapply[A](obj: ExprOpGen[A]): Option[DocVar] = obj.match {
      case Var(docVar) => Some(docVar)
      case _ => None
    }
  }

  object $andF {
    def apply[A](left: A, right: A): ExprOpGen[A] = Term($andF(left, right))
    def unapply[A](obj: ExprOpGen[A]): Option[(A, A)] = obj.match {
      case And(left, right) => Some((left, right))
      case _ => None
    }
  }
}

  */

  val groupOp = DataSchema(
    "slamdata.engine",
    "AccumOpGen",
    "AccumlatorGen",
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

  println(generate(groupOp))
}
