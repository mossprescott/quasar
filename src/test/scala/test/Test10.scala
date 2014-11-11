package test

import scalaz._
import Scalaz._

import shapeless.{Data => _, :: => #::, _}
import shapeless.syntax.singleton._

import slamdata.engine._
import slamdata.engine.fp._
import slamdata.engine.analysis.fixplate._
import slamdata.engine.std.MathLib.{Add}
import LogicalPlan._

object Test10 extends App {
  val hl = "abc" :: 0 :: HNil
  val hl0: String = hl(0)
  val hl1: Int = hl(1)
  // val hl2 = hl(2)
  
  def nth[L <: HList](l: L, index: Nat)(implicit at: shapeless.ops.hlist.At[L, index.N]): at.Out = l(at)
  def mapNth[L <: HList, A, B]
      (l: L, index: Nat, f: A => B)
      (implicit at: shapeless.ops.hlist.At.Aux[L, index.N, A]):
      B #:: L =
    f(l(index)) :: l
  
  val hl0a: String = nth(hl, 0)
  val hl0b = mapNth(hl, 0, (s: String) => s.length)
  val hl0b0: Int = hl0b(0)
  
  def listLPPhase0[L <: HList, A]
    (phase: Phase[LogicalPlan, Unit, A]):
    Phase[LogicalPlan, L, A #:: L] = 
  {
    Phase { (attrL: Attr[LogicalPlan, L]) =>
      val attrU: Attr[LogicalPlan, Unit] = attrL.map(_ => ())
      val attrA: Attr[LogicalPlan, A] = phase(attrU)
      val attrLA: Attr[LogicalPlan, (L, A)] = unsafeZip2(attrL, attrA)
      attrLA.map { t => t._2 :: t._1 }
    }
  }
  
  def listPhase0[F[_], L <: HList, A]
    (phase: Phase[F, Unit, A])
    (implicit F: Traverse[F]):
    Phase[F, L, A #:: L] = 
  {
    Phase { (attrL: Attr[F, L]) =>
      val attrU: Attr[F, Unit] = attrL.map(_ => ())
      val attrA: Attr[F, A] = phase(attrU)
      val attrLA: Attr[F, (L, A)] = unsafeZip2(attrL, attrA)
      attrLA.map { t => t._2 :: t._1 }
    }
  }

  def listPhase1Head[F[_], L <: HList, A, B]
    (phase: Phase[F, A, B])
    (implicit F: Traverse[F]):
    Phase[F, A #:: L, B #:: A #:: L] =
  {
    Phase { (attrL: Attr[F, A #:: L]) =>
      val attrA: Attr[F, A] = attrL.map(_.head)
      val attrB: Attr[F, B] = phase(attrA)
      val attrALB: Attr[F, (A #:: L, B)] = unsafeZip2(attrL, attrB)
      attrALB.map { t => t._2 :: t._1 }
    }
  }

  // /** Extracts a single parameter from an arbitrary index in the input list. */
  def listPhase1[F[_], L <: HList, A, B]
    (phase: Phase[F, A, B], index: Nat)
    (implicit F: Traverse[F], at: shapeless.ops.hlist.At.Aux[L, index.N, A]):
    Phase[F, L, B #:: L] =
  {
    Phase { (attrL: Attr[F, L]) =>
      val attrA: Attr[F, A] = attrL.map((l: L) => l(at))
      val attrB: Attr[F, B] = phase(attrA)
      val attrALB: Attr[F, (L, B)] = unsafeZip2(attrL, attrB)
      attrALB.map { t => t._2 :: t._1 }
    }
  }

  val expr = Invoke(Add, List(
    Constant(Data.Int(1)),
    Constant(Data.Int(2))))
  
  val sum = Phase[LogicalPlan, Unit, Int] { attr => 
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

  val sumList1 = listLPPhase0[HNil, Int](sum)
  val summed1 = sumList1(attrK(expr, HNil))
  println(summed1)
  
  val sumList2 = listPhase0[LogicalPlan, HNil, Int](sum)
  val summed2/*: Attr[LogicalPlan, Int #:: HNil]*/ = sumList2(attrK(expr, HNil))
  println(summed2)
  
  val stred = Phase[LogicalPlan, Int, String] { _.map("the sum is " + _) }
  
  val strList1 = listPhase1Head[LogicalPlan, HNil, Int, String](stred)
  val str1 = strList1(summed2)
  println(str1)
  
  val strList2 = listPhase1[LogicalPlan, Int #:: HNil, Int, String](stred, 0)
  // def strList2[L <: HList] = listPhase1(stred, 0)
  val str2 = strList2(summed2)
  println(str2)
  
  // val strList2 = listPhase1[LogicalPlan, Int #:: HNil, Int, String](stred, 0)
  // def strList2[L <: HList] = listPhase1(stred, 0)
  val str3 = listPhase1(stred, 0).apply(summed2)
  println(str3)
  
  // val n: String = 1.narrow
  // val w: String = 1.witness
  
}
