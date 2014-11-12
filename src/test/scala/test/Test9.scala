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
  // val anotherRecord: Boolean with KeyTag[titleW.T, Boolean] #:: HNil = ((titleW.value: titleW.T) ->> true) :: HNil
  val anotherTitle: Boolean = getTitle(anotherRecord)
  
  def getOne[L <: HList](xs: L, k: Witness)(implicit sel: ops.record.Selector[L, k.T]) = xs(k)
  // def getTitle2[L <: HList, A](xs: L)(implicit sel: ops.record.Selector[L, titleW.T]): A = dumb(getOne(xs, titleW))
  // val bookTitle2: String = getTitle2(book)

  def getOne2[L <: HList, A](xs: L, k: Witness)(implicit sel: ops.record.Selector.Aux[L, k.T, A]): A = xs(k)
  
  val bookTitle3: String = getOne2(book, titleW)
  println("via getOne2: " + bookTitle3)
  
  
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
  def recordify0[F[_], A, K, L <: HList]
    (phase: Phase[F, Unit, A], k: Witness)
    (implicit F: Traverse[F], upd: ops.record.Updater[L, FieldType[k.T, A]]):
    Phase[F, L, upd.Out] = 
  {
    Phase { (attrL: Attr[F, L]) =>
      val attrA: Attr[F, A] = phase(attrL.map(_ => ()))
      val attrLA = unsafeZip2(attrL, attrA)
      attrLA.map { case (l, a) => l.updated(k, a)(upd) }
    }
  }

  val wSum = Witness('sum)
  // val wSum2 = 'sum.witness
  type sumT = wSum.T
  // ops.record.Selector[L, sumT]
  
  val summedB = recordify0[LogicalPlan, Int, sumT, HNil](p1, wSum)
  
  val recSum1: Int = summedB(attrK(sum, HNil)).unFix.attr('sum)
  
  val wTotal = 'total.witness
  // type totalT = wTotal.T
  val wSilly = 'foo.witness
  type sillyT = FieldType[wSilly.T, Int]
  val sillyVal = ('foo ->> 0) :: HNil
  val summedB2 = recordify0[LogicalPlan, Int, wTotal.T, sillyT #:: HNil](p1, 'total)
  val recSum2: Int = summedB2(attrK(sum, sillyVal)).unFix.attr('total)
  
  println(recSum1 + "; " + recSum2)
  println(summedB(attrK(sum, HNil)).unFix.attr.keys)
  println(summedB2(attrK(sum, sillyVal)).unFix.attr)
  println(summedB2(attrK(sum, sillyVal)).unFix.attr.keys)
  

  /**
   Transforms a phase consuming one input, found in the input list under 
   the given key, adding its output annotation to the tree's HList under 
   an arbitrary key.
   */
  def recordify1[F[_], A, B, KA, KB, L <: HList]
    (phase: Phase[F, A, B], ka: Witness, kb: Witness)
    (implicit F: Traverse[F],
      selA: ops.record.Selector.Aux[L, ka.T, A],
      updB: ops.record.Updater[L, FieldType[kb.T, B]]):
    Phase[F, L, updB.Out] =
  {
    Phase { (attrL: Attr[F, L]) =>
      val attrB: Attr[F, B] = phase(attrL.map(l => l(ka)))
      val attrLB = unsafeZip2(attrL, attrB)
      attrLB.map { case (l, b) => l.updated(kb, b)(updB) }
    }
  }

  /**
   Transforms a phase consuming two inputs, found in the input list under 
   the given keys, adding its output annotation to the tree's HList under 
   an arbitrary key.
   */
  def recordify2[F[_], A, B, C, KA, KB, KC, L <: HList]
    (phase: Phase[F, (A, B), C], ka: Witness, kb: Witness, kc: Witness)
    (implicit F: Traverse[F],
      selA: ops.record.Selector.Aux[L, ka.T, A],
      selB: ops.record.Selector.Aux[L, kb.T, B],
      updC: ops.record.Updater[L, FieldType[kc.T, C]]):
    Phase[F, L, updC.Out] =
  {
    Phase { (attrL: Attr[F, L]) =>
      val attrC: Attr[F, C] = phase(attrL.map(l => (l(ka), l(kb))))
      val attrLC = unsafeZip2(attrL, attrC)
      attrLC.map { case (l, c) => l.updated(kc, c)(updC) }
    }
  }

  val double = Phase[LogicalPlan, Int, Int] { _.map(_*2) }
  
  val wDouble = 'double.witness
  
  // val dp = recordify0(p1, wTotal) >>> recordify1(double, wTotal, wDouble)
  val p1r = recordify0[LogicalPlan, Int, wTotal.T, HNil](p1, wTotal)
  val p2r = p1r >>> recordify1(double, wTotal, wDouble)

  val p2ra = p2r(attrK(sum, HNil))
  println("p2ra: " + p2ra)
  println("  keys: " + p2ra.unFix.attr.keys)
  
  val combine = Phase[LogicalPlan, (Int, Int), Int] { _.map(t => t._1 + t._2) }
  
  val p3r = p2r >>> recordify2(combine, wTotal, wDouble, 'triple.witness)
  val p3ra = p3r(attrK(sum, HNil))
  println("p3ra: " + p3ra)
  println("  keys: " + p3ra.unFix.attr.keys)

  // import shapeless.syntax.std.tuple._
  //
  // val hl = "abc" :: 0 :: HNil
  // val hl0: String = hl(0)
  // val hl1: Int = hl(1)
  // // val hl2 = hl(2)
}