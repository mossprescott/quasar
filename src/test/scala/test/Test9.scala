package test

object Test9 extends App {

  import shapeless.{Data => _, :: => #::, _}
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
  
  
  
  // A function expecting a "record" containing a certain key:
  
  // Defining a Witness is the only way to write the necessary type. See http://stackoverflow.com/a/19316446
  val titleW = Witness("title")
  
  def getTitle[L <: HList](xs: L)(implicit sel: ops.record.Selector[L, titleW.T]) = xs("title")
  
  val bookTitle: String = getTitle(book)
  val otherTitle: Int = getTitle(("title" ->> 0) :: HNil)
  
  val anotherRecord: Boolean with KeyTag[titleW.T, Boolean] #:: HNil = ("title" ->> true) :: HNil
  val anotherTitle: Boolean = getTitle(anotherRecord)
  
  
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
  
  
  // type SumRecord = Record.`'sum -> Int`.T  // doesn't compile; Record not found (WTF)
  
  /** Transforms a phase, putting its output annotation in a singleton HList under the key `'sum`. */
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
  
  
  
  /** Transforms a phase, adding its output annotation to the tree's HList under an arbitrary key. */
  def recordify[F[_], A, K](phase: Phase[F, Unit, A], k: Witness)
    (implicit F: Traverse[F], upd: ops.record.Updater[HNil, K]):
    Phase[F, HList, (A with KeyTag[k.T, A]) #:: HNil] = 
  {
    Phase { (attr: Attr[F, HList]) =>
      val attr1: Attr[F, A] = phase(attr.map(_ => ()))
      attr1.map((a: A) => HNil.updated(k, a))
    }
  }


  val wSum = Witness('sum)
  // val wSum2 = 'sum.witness
  type sumT = wSum.T
  // ops.record.Selector[L, sumT]
  
  val summedB = recordify[LogicalPlan, Int, sumT](p1, wSum)
  
  val recSum1: Int = summedB(attrK(sum, HNil)).unFix.attr('sum)
  
  val wTotal = 'total.witness
  type totalT = wTotal.T
  val recSum2: Int = recordify[LogicalPlan, Int, totalT](p1, 'total).apply(attrK(sum, HNil)).unFix.attr('total)
  
  println(recSum1 + "; " + recSum2)
  
}