/*
 * Copyright 2014–2016 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package quasar.physical.mongodb.expression

import quasar.Predef._

import quasar.physical.mongodb.{Bson, BsonField}

import matryoshka.Recursive.ops._
import org.scalacheck._, Arbitrary.arbitrary
import org.specs2.mutable._
import org.specs2.scalaz._
import scalaz._
import scalacheck.ScalazArbitrary._

object ArbitraryExprOp {

  implicit val formatSpecifierArbitrary: Arbitrary[ExprOp.FormatSpecifier] = Arbitrary {
    import ExprOp.FormatSpecifier._
    Gen.oneOf(
      Year, Month, DayOfMonth,
      Hour, Minute, Second, Millisecond,
      DayOfYear, DayOfWeek, WeekOfYear)
  }

  implicit val formatStringArbitrary: Arbitrary[ExprOp.FormatString] = Arbitrary {
    arbitrary[List[String \/ ExprOp.FormatSpecifier]].map(s =>
      ExprOp.FormatString(s))
  }

  lazy val genExpr: Gen[Expression] =
    Gen.oneOf(
      arbitrary[Int].map(x => $literal(Bson.Int32(x))),
      arbitrary[ExprOp.FormatString].map(fmt =>
        $dateToString(fmt, $var(DocField(BsonField.Name("date"))))),
      Gen.alphaChar.map(c => $var(DocField(BsonField.Name(c.toString)))),
      genExpr.flatMap(x => Gen.oneOf(
        $sqrt(x),
        $abs(x),
        $log10(x),
        $ln(x),
        $trunc(x),
        $ceil(x),
        $floor(x))),
      for {
        x <- genExpr
        y <- genExpr
        expr <- Gen.oneOf(
        $log(x, y),
        $pow(x, y))
      } yield expr)
}

class ExpressionSpec extends Specification with DisjunctionMatchers with ScalazMatchers {

  "Expression" should {

    "escape literal string with $" in {
      val x = Bson.Text("$1")
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape literal string with no leading '$'" in {
      val x = Bson.Text("abc")
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape simple integer literal" in {
      val x = Bson.Int32(0)
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape simple array literal" in {
      val x = Bson.Arr(Bson.Text("abc") :: Bson.Int32(0) :: Nil)
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape string nested in array" in {
      val x = Bson.Arr(Bson.Text("$1") :: Nil)
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape simple doc literal" in {
      val x = Bson.Doc(ListMap("a" -> Bson.Text("b")))
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "escape string nested in doc" in {
      val x = Bson.Doc(ListMap("a" -> Bson.Text("$1")))
      $literal(x).cata(bsonƒ) must_== Bson.Doc(ListMap("$literal" -> x))
    }

    "render $$ROOT" in {
      DocVar.ROOT().bson must_== Bson.Text("$$ROOT")
    }

    "treat DocField as alias for DocVar.ROOT()" in {
      DocField(BsonField.Name("foo")) must_== DocVar.ROOT(BsonField.Name("foo"))
    }

    "render $foo under $$ROOT" in {
      DocVar.ROOT(BsonField.Name("foo")).bson must_== Bson.Text("$foo")
    }

    "render $foo.bar under $$CURRENT" in {
      DocVar.CURRENT(BsonField.Name("foo") \ BsonField.Name("bar")).bson must_== Bson.Text("$$CURRENT.foo.bar")
    }
  }

  "toJs" should {
    import org.threeten.bp._
    import quasar.jscore._

    "handle addition with epoch date literal" in {
      toJs(
        $add(
          $literal(Bson.Date(Instant.ofEpochMilli(0))),
          $var(DocField(BsonField.Name("epoch"))))) must beRightDisjunction(
        JsFn(JsFn.defaultName, New(Name("Date"), List(Select(Ident(JsFn.defaultName), "epoch")))))
    }
  }

  "FormatSpecifier" should {
    import ExprOp._, FormatSpecifier._

    def toBson(fmt: FormatString): Bson =
      $dateToString(fmt, $var(DocField(BsonField.Name("date"))))
        .cata(bsonƒ)

    def expected(str: String): Bson =
      Bson.Doc(ListMap(
        "$dateToString" -> Bson.Doc(ListMap(
          "format" -> Bson.Text(str),
          "date" -> Bson.Text("$date")))))

    "match first example from mongodb docs" in {
      toBson(Year :: "-" :: Month :: "-" :: DayOfMonth :: FormatString.empty) must_==
        expected("%Y-%m-%d")
    }

    "match second example from mongodb docs" in {
      toBson(Hour :: ":" :: Minute :: ":" :: Second :: ":" :: Millisecond :: FormatString.empty) must_==
        expected("%H:%M:%S:%L")
    }

    "escape `%`s" in {
      toBson(Hour :: "%" :: Minute :: "%" :: Second :: FormatString.empty) must_==
        expected("%H%%%M%%%S")
    }
  }
}
