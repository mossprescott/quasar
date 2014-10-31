package test

object Test9 extends App {

  import shapeless.{Data => _, :: => _, _}
  import syntax.singleton._
  import record._
  
  
  val book = 
    ("author" ->> "Benjamin Pierce") ::
    ("title" ->> "Types and Programming Languages") ::
    ("id" ->> 261992874) ::
    ('sym ->> 0) ::
    HNil

  println(book)
  
  val title: String = book("title")
  val id: Int = book("id")
  val sym: Int = book('sym)
  // val missing = book("missing")    // compile error: missing key
  
  println(book.keys)
  
  
  
  // Now try it with Phase:
  
  import slamdata.engine._
  import LogicalPlan._
  import std.MathLib.{Add}
  import analysis.fixplate._
  import fp._
  
  import scalaz._, Scalaz._
  
  val sum = Invoke(Add, List(
    Constant(Data.Int(1)),
    Constant(Data.Int(2))))
  
  val p1 = Phase[LogicalPlan, Unit, Int] { attr => 
      scanCata(attr) { (_, node: LogicalPlan[Int]) =>
        node.fold(
          read = _ => ???,
          constant = {
            case Data.Int(n) => n.toInt
            case _ => ???
          },
          join = (_, _, _, _, _, _) => ???,
          invoke = {
            case (`Add`, x :: y :: Nil) => x + y
            case _ => ???
          },
          free = _ => ???,
          let = (_, _, _) => ???)
      }
  }
  
  val summed: Attr[LogicalPlan, Int] = p1(attrUnit(sum))
  implicit val IntRT: RenderTree[Int] = new RenderTree[Int] { def render(v: Int) = Terminal(v.toString, List("Int")) }
  println(summed.show)
  
  
  val p2 = Phase[LogicalPlan, Int, String] { _.map((n: Int) => n.toString) }
  
  val stringed: Attr[LogicalPlan, String] = (p1 >>> p2)(attrUnit(sum))
  implicit val StringRT: RenderTree[String] = new RenderTree[String] { def render(v: String) = Terminal(v.toString, List("String")) }
  println(stringed.show)
  
  
  
  // def shapify[F[_], A](phase: Phase[F, Unit, A], key: shapeless.tag.Tagged[_]) = {
  //   Phase { (attr: Attr[F, HList]) =>
  //     val attr1: Attr[LogicalPlan, A] = phase(attr)
  //     attr1.map((a: A) => (key ->> n) :: HNil)
  //   }
  // }

  
  def summify(phase: Phase[LogicalPlan, Unit, Int]) = {
    Phase { (attr: Attr[LogicalPlan, Unit]) => 
      val a1: Attr[LogicalPlan, Int] = phase(attr)
      a1.map((n: Int) => ('sum ->> n) :: HNil)
    }
  }
  
  val p1a = summify(p1)
  
  val summedA = p1a(attrUnit(sum))
  
  println(summedA)
  
  // println(summedA.unFix.attr("foo"))  // compile error
  val i: Int = summedA.unFix.attr('sum)
  println(i)
  
}