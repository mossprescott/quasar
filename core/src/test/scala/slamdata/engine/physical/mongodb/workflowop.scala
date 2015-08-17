package slamdata.engine.physical.mongodb

import slamdata.Predef._
import slamdata.{RenderTree, Terminal, NonTerminal}
import slamdata.engine.TreeMatchers
import slamdata.fp._
import slamdata.engine.javascript._

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.specs2.mutable._
import org.specs2.scalaz._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazProperties._
import shapeless.contrib.scalaz.instances._

class WorkflowFSpec extends Spec {
  import Workflow._
  import IdHandling._

  implicit val arbIdHandling: Arbitrary[IdHandling] =
    Arbitrary(Gen.oneOf(ExcludeId, IncludeId, IgnoreId))

  checkAll("IdHandling", monoid.laws[IdHandling])

  implicit val arbCardinalExpr:
      Arbitrary ~> λ[α => Arbitrary[CardinalExpr[α]]] =
    new (Arbitrary ~> λ[α => Arbitrary[CardinalExpr[α]]]) {
      def apply[α](arb: Arbitrary[α]): Arbitrary[CardinalExpr[α]] =
        Arbitrary(arb.arbitrary.flatMap(a =>
          Gen.oneOf(MapExpr(a), FlatExpr(a))))
    }

  implicit val arbIntCardinalExpr = arbCardinalExpr(Arbitrary.arbInt)

  checkAll("CardinalExpr", traverse.laws[CardinalExpr])
  checkAll("CardinalExpr", comonad.laws[CardinalExpr])
}

class WorkflowSpec extends Specification with TreeMatchers {
  import slamdata.engine.physical.mongodb.accumulator._
  import slamdata.engine.physical.mongodb.expression._
  import Workflow._
  import IdHandling._

  val readFoo = $read(Collection("db", "foo"))

  "smart constructors" should {
    "put match before sort" in {
      val given = chain(
        readFoo,
        $sort(NonEmptyList(BsonField.Name("city") -> Descending)),
        $match(Selector.Doc(
          BsonField.Name("pop") -> Selector.Gte(Bson.Int64(1000)))))
      val expected = chain(
        readFoo,
        $match(Selector.Doc(
          BsonField.Name("pop") -> Selector.Gte(Bson.Int64(1000)))),
        $sort(NonEmptyList(BsonField.Name("city") -> Descending)))

      given must beTree(expected)
    }

    "choose smallest limit" in {
      val expected = chain(readFoo, $limit(5))
      chain(readFoo, $limit(10), $limit(5)) must_== expected
      chain(readFoo, $limit(5), $limit(10)) must_== expected
    }

    "sum skips" in {
      chain(readFoo, $skip(10), $skip(5)) must beTree(chain(readFoo, $skip(15)))
    }

    "flatten foldLefts when possible" in {
      val given = $foldLeft(
        $foldLeft(
          readFoo,
          $read(Collection("db", "zips"))),
        $read(Collection("db", "olympics")))
      val expected = $foldLeft(
        readFoo,
        $read(Collection("db", "zips")),
        $read(Collection("db", "olympics")))

      given must beTree(expected)
    }

    "flatten project into group/unwind" in {
      val given = chain(
        readFoo,
        $group(
          Grouped(ListMap(
            BsonField.Name("value") -> $push($field("rIght")))),
          -\/($field("lEft"))),
        $unwind(DocField(BsonField.Name("value"))),
        $project(Reshape(ListMap(
          BsonField.Name("city") -> -\/($field("value", "city")))),
          IncludeId))

      val expected = chain(
        readFoo,
        $group(
          Grouped(ListMap(
            BsonField.Name("city") -> $push($field("rIght", "city")))),
          -\/($field("lEft"))),
        $unwind(DocField(BsonField.Name("city"))))

      given must beTree(expected: Workflow)
    }.pendingUntilFixed("#536")

    "not flatten project into group/unwind with _id excluded" in {
      val given = chain(
        readFoo,
        $group(
          Grouped(ListMap(
            BsonField.Name("value") -> $push($field("rIght")))),
          -\/($field("lEft"))),
        $unwind(DocField(BsonField.Name("value"))),
        $project(Reshape(ListMap(
          BsonField.Name("city") -> -\/($field("value", "city")))),
          ExcludeId))

      given must beTree(given: Workflow)
    }

    "resolve `Include`" in {
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($include()))),
          ExcludeId),
        $project(Reshape(ListMap(
          BsonField.Name("_id") -> -\/($field("bar")))),
          IncludeId)) must_==
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("_id") -> -\/($field("bar")))),
          IncludeId))
    }

    "traverse `Include`" in {
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") ->
            -\/($divide(
              $field("baz"),
              $literal(Bson.Int32(92)))))),
          IncludeId),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($include()))),
          IncludeId)) must_==
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") ->
            -\/($divide($field("baz"), $literal(Bson.Int32(92)))))),
          IncludeId))
    }

    "resolve implied `_id`" in {
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($field("bar")),
          BsonField.Name("_id") ->
            -\/($field("baz")))),
          IncludeId),
        $project(Reshape(ListMap(
          BsonField.Name("bar") ->
            -\/($field("bar")))),
          IncludeId)) must_==
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($field("bar")),
          BsonField.Name("_id") ->
            -\/($field("baz")))),
          IncludeId))
    }

    "not resolve excluded `_id`" in {
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($field("bar")),
          BsonField.Name("_id") ->
            -\/($field("baz")))),
          IncludeId),
        $project(Reshape(ListMap(
          BsonField.Name("bar") ->
            -\/($field("bar")))),
          ExcludeId)) must_==
      chain($read(Collection("db", "zips")),
        $project(Reshape(ListMap(
          BsonField.Name("bar") ->
            -\/($field("bar")))),
          ExcludeId))
    }
  }

  "finalize" should {
    import JsCore._

    "coalesce previous projection into a map" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $project(Reshape(ListMap(
          BsonField.Name("value") -> -\/($$ROOT))),
          IncludeId),
        $map($Map.mapNOP, ListMap()))

      val expected = chain(
        readZips,
        $map($Map.compose(
          $Map.mapNOP,
          $Map.mapMap("value",
            Obj(ListMap("value" -> Ident("value").fix)).fix.toJs)),
          ListMap()))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "coalesce previous projection into a flatMap" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $project(Reshape(ListMap(
          BsonField.Name("value") -> -\/($$ROOT))),
          IncludeId),
        $flatMap(
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(
              Js.Ident("attr"),
              Select(Ident("value").fix, "value").fix.toJs,
              Call(
                Select(Ident("rez").fix, "push").fix,
                List(
                  Arr(List(
                    Call(Ident("ObjectId").fix, Nil).fix,
                    Access(
                      Select(Ident("value").fix, "value").fix,
                      Ident("attr").fix).fix)).fix)).fix.toJs),
            Js.Return(Js.Ident("rez")))),
          ListMap()))

      val expected = chain(
        readZips,
        $flatMap($Map.compose(
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(
              Js.Ident("attr"),
              Select(Ident("value").fix, "value").fix.toJs,
              Call(
                Select(Ident("rez").fix, "push").fix,
                List(
                  Arr(List(
                    Call(Ident("ObjectId").fix, Nil).fix,
                    Access(
                      Select(Ident("value").fix, "value").fix,
                      Ident("attr").fix).fix)).fix)).fix.toJs),
            Js.Return(Js.Ident("rez")))),
          $Map.mapMap("value",
            Obj(ListMap("value" -> Ident("value").fix)).fix.toJs)),
          ListMap()))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "convert previous projection before a reduce" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $project(Reshape(ListMap(
          BsonField.Name("value") -> -\/($$ROOT))),
          IncludeId),
        $reduce($Reduce.reduceNOP, ListMap()))

      val expected = chain(
        readZips,
        $map($Map.mapMap("value",
          Obj(ListMap("value" -> Ident("value").fix)).fix.toJs),
          ListMap()),
        $reduce($Reduce.reduceNOP, ListMap()))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "coalesce previous unwind into a map" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $unwind(DocVar.ROOT(BsonField.Name("loc"))),
        $map($Map.mapNOP, ListMap()))

      val expected = chain(
        readZips,
        $flatMap($FlatMap.mapCompose(
          $Map.mapNOP,
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(Js.Ident("elem"), Select(Ident("value").fix, "loc").fix.toJs,
              Js.Block(List(
                Js.VarDef(List(
                  "each0" -> Js.Call(Js.Ident("clone"), List(Js.Ident("value"))))),
                unsafeAssign(Select(Ident("each0").fix, "loc").fix, Access(Select(Ident("value").fix, "loc").fix, Ident("elem").fix).fix),
                Js.Call(Js.Select(Js.Ident("rez"), "push"),
                  List(
                    Js.AnonElem(List(
                      Js.Call(Js.Ident("ObjectId"), Nil),
                      Js.Ident("each0")))))))),
            Js.Return(Js.Ident("rez"))))),
          $SimpleMap.implicitScope(Set("clone"))))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "coalesce previous unwind into a flatMap" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $unwind(DocVar.ROOT(BsonField.Name("loc"))),
        $flatMap(
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(
              Js.Ident("attr"),
              Select(Ident("value").fix, "value").fix.toJs,
              Call(Select(Ident("rez").fix, "push").fix,
                List(
                  Arr(List(
                    Call(Ident("ObjectId").fix, Nil).fix,
                    Access(
                      Select(Ident("value").fix, "value").fix,
                      Ident("attr").fix).fix)).fix)).fix.toJs),
            Js.Return(Js.Ident("rez")))),
          ListMap()))

      val expected = chain(
        readZips,
        $flatMap($FlatMap.kleisliCompose(
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(
              Js.Ident("attr"),
              Select(Ident("value").fix, "value").fix.toJs,
              Call(Select(Ident("rez").fix, "push").fix,
                List(
                  Arr(List(
                    Call(Ident("ObjectId").fix, Nil).fix,
                    Access(
                      Select(Ident("value").fix, "value").fix,
                      Ident("attr").fix).fix)).fix)).fix.toJs),
            Js.Return(Js.Ident("rez")))),
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(Js.Ident("elem"), Select(Ident("value").fix, "loc").fix.toJs,
              Js.Block(List(
                Js.VarDef(List(
                  "each0" -> Js.Call(Js.Ident("clone"), List(Js.Ident("value"))))),
                unsafeAssign(Select(Ident("each0").fix, "loc").fix, Access(Select(Ident("value").fix, "loc").fix, Ident("elem").fix).fix),
                Js.Call(Js.Select(Js.Ident("rez"), "push"),
                  List(
                    Js.AnonElem(List(
                      Js.Call(Js.Ident("ObjectId"), Nil),
                      Js.Ident("each0")))))))),
            Js.Return(Js.Ident("rez"))))),
          $SimpleMap.implicitScope(Set("clone"))))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "convert previous unwind before a reduce" in {
      val readZips = $read(Collection("db", "zips"))
      val given = chain(
        readZips,
        $unwind(DocVar.ROOT(BsonField.Name("loc"))),
        $reduce($Reduce.reduceNOP, ListMap()))

      val expected = chain(
        readZips,
        $flatMap(
          Js.AnonFunDecl(List("key", "value"), List(
            Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
            Js.ForIn(Js.Ident("elem"), Select(Ident("value").fix, "loc").fix.toJs,
              Js.Block(List(
                Js.VarDef(List(
                  "each0" -> Js.Call(Js.Ident("clone"), List(Js.Ident("value"))))),
                unsafeAssign(Select(Ident("each0").fix, "loc").fix, Access(Select(Ident("value").fix, "loc").fix, Ident("elem").fix).fix),
                Js.Call(Js.Select(Js.Ident("rez"), "push"),
                  List(
                    Js.AnonElem(List(
                      Js.Call(Js.Ident("ObjectId"), Nil),
                      Js.Ident("each0")))))))),
            Js.Return(Js.Ident("rez")))),
          $SimpleMap.implicitScope(Set("clone"))),
        $reduce($Reduce.reduceNOP, ListMap()))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "patch $FoldLeft" in {
      val readZips = $read(Collection("db", "zips"))
      val given = $foldLeft(readZips, readZips)

      val expected = $foldLeft(
        chain(readZips, $project(Reshape(ListMap(
          BsonField.Name("value") -> -\/($$ROOT))),
          IncludeId)),
        chain(readZips, $reduce($Reduce.reduceFoldLeft, ListMap())))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "patch $FoldLeft with existing reduce" in {
      val readZips = $read(Collection("db", "zips"))
      val given = $foldLeft(
        readZips,
        chain(readZips, $reduce($Reduce.reduceNOP, ListMap())))

      val expected = $foldLeft(
        chain(
          readZips,
          $project(Reshape(ListMap(
            BsonField.Name("value") -> -\/($$ROOT))),
            IncludeId)),
        chain(readZips, $reduce($Reduce.reduceNOP, ListMap())))

      crystallize(given) must beTree(Crystallized(expected))
    }

    "avoid dangling map with known shape" in {
      crystallize(chain(
        $read(Collection("db", "zips")),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"), JsCore.Obj(ListMap(
            "first" -> JsCore.Select(Ident("x").fix, "pop").fix,
            "second" -> JsCore.Select(Ident("x").fix, "city").fix)).fix))),
          ListMap()))) must
      beTree(Crystallized(chain(
        $read(Collection("db", "zips")),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"), JsCore.Obj(ListMap(
            "first" -> JsCore.Select(Ident("x").fix, "pop").fix,
            "second" -> JsCore.Select(Ident("x").fix, "city").fix)).fix))),
          ListMap()),
        $project(Reshape(ListMap(
          BsonField.Name("first") -> -\/($include()),
          BsonField.Name("second") -> -\/($include()))),
          IgnoreId))))
    }

    "avoid dangling flatMap with known shape" in {
      crystallize(chain(
        $read(Collection("db", "zips")),
        $simpleMap(
          NonEmptyList(
            MapExpr(JsFn(Ident("x"), JsCore.Obj(ListMap(
              "first"  -> JsCore.Select(Ident("x").fix, "loc").fix,
              "second" -> Ident("x").fix)).fix)),
            FlatExpr(JsFn(Ident("x"), JsCore.Select(Ident("x").fix, "city").fix))),
          ListMap()))) must
      beTree(Crystallized(chain(
        $read(Collection("db", "zips")),
        $simpleMap(
          NonEmptyList(
            MapExpr(JsFn(Ident("x"), JsCore.Obj(ListMap(
              "first"  -> JsCore.Select(Ident("x").fix, "loc").fix,
              "second" -> Ident("x").fix)).fix)),
            FlatExpr(JsFn(Ident("x"), JsCore.Select(Ident("x").fix, "city").fix))),
          ListMap()),
        $project(Reshape(ListMap(
          BsonField.Name("first") -> -\/($include()),
          BsonField.Name("second") -> -\/($include()))),
          IgnoreId))))
    }

    "fold unwind into SimpleMap" in {
      import JsCore._

      crystallize(chain(
        $read(Collection("db", "zips")),
        $unwind(DocField(BsonField.Name("loc"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"), Obj(ListMap("0" -> Select(Ident("x").fix, "loc").fix)).fix))),
          ListMap()))) must
      beTree(Crystallized(chain(
        $read(Collection("db", "zips")),
        $simpleMap(
          NonEmptyList(
            FlatExpr(JsFn(Ident("x"), Select(Ident("x").fix, "loc").fix)),
            MapExpr(JsFn(Ident("x"), Obj(ListMap("0" -> Select(Ident("x").fix, "loc").fix)).fix))),
          ListMap()),
        $project(
          Reshape(ListMap(
            BsonField.Name("0") -> -\/($include()))),
          IgnoreId))))
    }

    "not fold unwind into SimpleMap with preceding pipeline op" in {
      import JsCore._

      crystallize(chain(
        $read(Collection("db", "zips")),
        $project(Reshape(ListMap(
            BsonField.Name("loc") -> -\/($var(DocField(BsonField.Name("loc")))))),
          IgnoreId),
        $unwind(DocField(BsonField.Name("loc"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"), Obj(ListMap("0" -> Select(Ident("x").fix, "loc").fix)).fix))),
          ListMap()))) must
        beTree(Crystallized(chain(
          $read(Collection("db", "zips")),
          $project(Reshape(ListMap(
              BsonField.Name("loc") -> -\/($var(DocField(BsonField.Name("loc")))))),
            IgnoreId),
          $unwind(DocField(BsonField.Name("loc"))),
          $simpleMap(
            NonEmptyList(
              MapExpr(JsFn(Ident("x"), Obj(ListMap("0" -> Select(Ident("x").fix, "loc").fix)).fix))),
            ListMap()),
          $project(
            Reshape(ListMap(
              BsonField.Name("0") -> -\/($include()))),
            IgnoreId))))
    }

    "fold multiple unwinds into SimpleMap" in {
      import JsCore._

      crystallize(chain(
        $read(Collection("db", "foo")),
        $unwind(DocField(BsonField.Name("bar"))),
        $unwind(DocField(BsonField.Name("baz"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"),
            Obj(ListMap(
              "0" -> Select(Ident("x").fix, "bar").fix,
              "1" -> Select(Ident("x").fix, "baz").fix)).fix))),
          ListMap()))) must
      beTree(Crystallized(chain(
        $read(Collection("db", "foo")),
        $simpleMap(
          NonEmptyList(
            FlatExpr(JsFn(Ident("x"), Select(Ident("x").fix, "bar").fix)),
            FlatExpr(JsFn(Ident("x"), Select(Ident("x").fix, "baz").fix)),
            MapExpr(JsFn(Ident("x"), Obj(ListMap(
              "0" -> Select(Ident("x").fix, "bar").fix,
              "1" -> Select(Ident("x").fix, "baz").fix)).fix))),
          ListMap()),
        $project(
          Reshape(ListMap(
            BsonField.Name("0") -> -\/($include()),
            BsonField.Name("1") -> -\/($include()))),
          IgnoreId))))
    }

    "not fold multiple unwinds into SimpleMap with preceding pipeline op" in {
      import JsCore._

      crystallize(chain(
        $read(Collection("db", "foo")),
        $project(Reshape(ListMap(
            BsonField.Name("loc") -> -\/($var(DocField(BsonField.Name("loc")))))),
          IgnoreId),
        $unwind(DocField(BsonField.Name("bar"))),
        $unwind(DocField(BsonField.Name("baz"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"),
            Obj(ListMap(
              "0" -> Select(Ident("x").fix, "bar").fix,
              "1" -> Select(Ident("x").fix, "baz").fix)).fix))),
          ListMap()))) must
      beTree(Crystallized(chain(
        $read(Collection("db", "foo")),
        $project(Reshape(ListMap(
            BsonField.Name("loc") -> -\/($var(DocField(BsonField.Name("loc")))))),
          IgnoreId),
        $unwind(DocField(BsonField.Name("bar"))),
        $unwind(DocField(BsonField.Name("baz"))),
        $simpleMap(
          NonEmptyList(
            MapExpr(JsFn(Ident("x"), Obj(ListMap(
              "0" -> Select(Ident("x").fix, "bar").fix,
              "1" -> Select(Ident("x").fix, "baz").fix)).fix))),
          ListMap()),
        $project(
          Reshape(ListMap(
            BsonField.Name("0") -> -\/($include()),
            BsonField.Name("1") -> -\/($include()))),
          IgnoreId))))
    }
  }

  "task" should {
    import WorkflowTask._

    "convert $match with $where into map/reduce" in {
      task(crystallize(chain(
        $read(Collection("db", "zips")),
        $match(Selector.Where(Js.BinOp("<",
          Js.Select(Js.Select(Js.Ident("this"), "city"), "length"),
          Js.Num(4, false))))))) must
      beTree[WorkflowTask](
        MapReduceTask(ReadTask(Collection("db", "zips")),
          MapReduce($Map.mapFn($Map.mapNOP), $Reduce.reduceNOP,
            selection = Some(Selector.Where(Js.BinOp("<",
              Js.Select(Js.Select(Js.Ident("this"), "city"), "length"),
              Js.Num(4, false)))))))
    }

    "always pipeline unconverted aggregation ops" in {
      // Tricky: don't want to actually finalize here, just testing `task` behavior
      task(Crystallized(chain(
        $read(Collection("db", "zips")),
        $group(
          Grouped(ListMap(
            BsonField.Name("__sd_tmp_1") -> $push($field("lEft")))),
          -\/($literal(Bson.Int32(1)))),
        $project(Reshape(ListMap(
          BsonField.Name("a")      -> -\/($include()),
          BsonField.Name("b")      -> -\/($include()),
          BsonField.Name("equal?") -> -\/($eq($field("a"), $field("b"))))),
          IncludeId),
        $match(Selector.Doc(
          BsonField.Name("equal?") -> Selector.Eq(Bson.Bool(true)))),
        $sort(NonEmptyList(BsonField.Name("a") -> Descending)),
        $limit(100),
        $skip(5),
        $project(Reshape(ListMap(
          BsonField.Name("a") -> -\/($include()),
          BsonField.Name("b") -> -\/($include()))),
          IncludeId)))) must
      beTree[WorkflowTask](
        PipelineTask(ReadTask(Collection("db", "zips")),
          List(
            $Group((),
              Grouped(ListMap(
                BsonField.Name("__sd_tmp_1") -> $push($field("lEft")))),
              -\/($literal(Bson.Null))),
            $Project((),
              Reshape(ListMap(
                BsonField.Name("a")      -> -\/($field("a")),
                BsonField.Name("b")      -> -\/($field("b")),
                BsonField.Name("equal?") -> -\/($eq($field("a"), $field("b"))))),
              IncludeId),
            $Match((),
              Selector.Doc(
                BsonField.Name("equal?") -> Selector.Eq(Bson.Bool(true)))),
            $Sort((), NonEmptyList(BsonField.Name("a") -> Descending)),
            $Limit((), 100),
            $Skip((), 5),
            $Project((),
              Reshape(ListMap(
                BsonField.Name("a") -> -\/($field("a")),
                BsonField.Name("b") -> -\/($field("b")))),
              IncludeId))))
    }

    "create maximal map/reduce" in {
      task(crystallize(chain(
        $read(Collection("db", "zips")),
        $match(Selector.Doc(
          BsonField.Name("loc") \ BsonField.Index(0) ->
            Selector.Lt(Bson.Int64(-73)))),
        $sort(NonEmptyList(BsonField.Name("city") -> Descending)),
        $limit(100),
        $map($Map.mapMap("value",
          Js.Access(Js.Ident("value"), Js.Num(0, false))),
          ListMap()),
        $reduce($Reduce.reduceFoldLeft, ListMap()),
        $map($Map.mapMap("value", Js.Ident("value")), ListMap())))) must
      beTree[WorkflowTask](
        MapReduceTask(ReadTask(Collection("db", "zips")),
          MapReduce(
            $Map.mapFn($Map.mapMap("value",
              Js.Access(Js.Ident("value"), Js.Num(0, false)))),
            $Reduce.reduceFoldLeft,
            selection = Some(Selector.Doc(
              BsonField.Name("loc") \ BsonField.Index(0) ->
                Selector.Lt(Bson.Int64(-73)))),
            inputSort =
              Some(NonEmptyList(BsonField.Name("city") -> Descending)),
            limit = Some(100),
            finalizer = Some($Map.finalizerFn($Map.mapMap("value",
              Js.Ident("value")))))))
    }

    "create maximal map/reduce with flatMap" in {
      task(crystallize(chain(
        $read(Collection("db", "zips")),
        $match(Selector.Doc(
          BsonField.Name("loc") \ BsonField.Index(0) ->
            Selector.Lt(Bson.Int64(-73)))),
        $sort(NonEmptyList(BsonField.Name("city") -> Descending)),
        $limit(100),
        $flatMap(Js.AnonFunDecl(List("key", "value"), List(
          Js.AnonElem(List(
            Js.AnonElem(List(Js.Ident("key"), Js.Ident("value"))))))),
          ListMap()),
        $reduce($Reduce.reduceFoldLeft, ListMap()),
        $map($Map.mapMap("value", Js.Ident("value")), ListMap())))) must
      beTree[WorkflowTask](
        MapReduceTask(ReadTask(Collection("db", "zips")),
          MapReduce(
            $FlatMap.mapFn(Js.AnonFunDecl(List("key", "value"), List(
              Js.AnonElem(List(
                Js.AnonElem(List(Js.Ident("key"), Js.Ident("value")))))))),
            $Reduce.reduceFoldLeft,
            selection = Some(Selector.Doc(
              BsonField.Name("loc") \ BsonField.Index(0) ->
                Selector.Lt(Bson.Int64(-73)))),
            inputSort =
              Some(NonEmptyList(BsonField.Name("city") -> Descending)),
            limit = Some(100),
            finalizer = Some($Map.finalizerFn($Map.mapMap("value",
              Js.Ident("value")))))))
    }

    "create map/reduce without map" in {
      task(crystallize(chain(
        $read(Collection("db", "zips")),
        $match(Selector.Doc(
          BsonField.Name("loc") \ BsonField.Index(0) ->
            Selector.Lt(Bson.Int64(-73)))),
        $sort(NonEmptyList(BsonField.Name("city") -> Descending)),
        $limit(100),
        $reduce($Reduce.reduceFoldLeft, ListMap()),
        $map($Map.mapMap("value", Js.Ident("value")), ListMap())))) must
      beTree[WorkflowTask](
        MapReduceTask(ReadTask(Collection("db", "zips")),
          MapReduce(
            $Map.mapFn($Map.mapNOP),
            $Reduce.reduceFoldLeft,
            selection = Some(Selector.Doc(
              BsonField.Name("loc") \ BsonField.Index(0) ->
                Selector.Lt(Bson.Int64(-73)))),
            inputSort =
              Some(NonEmptyList(BsonField.Name("city") -> Descending)),
            limit = Some(100),
            finalizer = Some($Map.finalizerFn($Map.mapMap("value",
              Js.Ident("value")))))))
    }

    "fold unwind into SimpleMap (when finalize is used)" in {
      import JsCore._

      task(crystallize(chain(
        $read(Collection("db", "zips")),
        $unwind(DocField(BsonField.Name("loc"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"), Obj(ListMap("0" -> Select(Ident("x").fix, "loc").fix)).fix))),
          ListMap())))) must
      beTree[WorkflowTask](
        PipelineTask(
          MapReduceTask(
            ReadTask(Collection("db", "zips")),
            MapReduce(
              Js.AnonFunDecl(Nil, List(
                Js.Call(
                  Js.Select(
                    Js.Call(
                      Js.AnonFunDecl(List("key", "value"), List(
                        Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
                        Js.ForIn(Js.Ident("elem"), JsCore.Select(Ident("value").fix, "loc").fix.toJs,
                          Js.Block(List(
                            Js.VarDef(List("each0" -> Js.Call(Js.Ident("clone"), List(Js.Ident("value"))))),
                            JsCore.unsafeAssign(Select(Ident("each0").fix, "loc").fix, Access(Select(Ident("value").fix, "loc").fix, Ident("elem").fix).fix),
                            Js.Block(List(
                              Js.VarDef(List("each1" ->
                                Obj(ListMap("0" -> Select(Ident("each0").fix, "loc").fix)).fix.toJs)),
                              Js.Call(Js.Select(Js.Ident("rez"), "push"), List(
                                Js.AnonElem(List(
                                  Js.Call(Js.Ident("ObjectId"), Nil),
                                  Js.Ident("each1")))))))))),
                        Js.Return(Js.Ident("rez")))),
                      List(Js.Select(Js.This, IdLabel), Js.This)),
                    "map"),
                  List(
                    Js.AnonFunDecl(List("__rez"), List(
                      Js.Call(Js.Select(Js.Ident("emit"), "apply"), List(Js.Null, Js.Ident("__rez"))))))))),
              $Reduce.reduceNOP,
              scope = $SimpleMap.implicitScope(Set("clone")))),
          List(
            $Project((),
              Reshape(ListMap(
                BsonField.Name("0") -> -\/($field("value", "0")))),
              IgnoreId))))
    }

    "fold multiple unwinds into SimpleMap (when finalize is used)" in {
      import JsCore._

      task(crystallize(chain(
        $read(Collection("db", "foo")),
        $unwind(DocField(BsonField.Name("bar"))),
        $unwind(DocField(BsonField.Name("baz"))),
        $simpleMap(
          NonEmptyList(MapExpr(JsFn(Ident("x"),
            Obj(ListMap(
              "0" -> Select(Ident("x").fix, "bar").fix,
              "1" -> Select(Ident("x").fix, "baz").fix)).fix))),
          ListMap())))) must
      beTree[WorkflowTask](
        PipelineTask(
          MapReduceTask(
            ReadTask(Collection("db", "foo")),
            MapReduce(
              Js.AnonFunDecl(Nil, List(
                Js.Call(
                  Js.Select(
                    Js.Call(
                      Js.AnonFunDecl(List("key", "value"), List(
                        Js.VarDef(List("rez" -> Js.AnonElem(Nil))),
                        Js.ForIn(Js.Ident("elem"), JsCore.Select(Ident("value").fix, "bar").fix.toJs,
                          Js.Block(List(
                            Js.VarDef(List("each0" -> Js.Call(Js.Ident("clone"), List(Js.Ident("value"))))),
                            JsCore.unsafeAssign(Select(Ident("each0").fix, "bar").fix, Access(Select(Ident("value").fix, "bar").fix, Ident("elem").fix).fix),
                            Js.ForIn(Js.Ident("elem"), JsCore.Select(Ident("each0").fix, "baz").fix.toJs,
                              Js.Block(List(
                                Js.VarDef(List("each1" -> Js.Call(Js.Ident("clone"), List(Js.Ident("each0"))))),
                                JsCore.unsafeAssign(Select(Ident("each1").fix, "baz").fix, Access(Select(Ident("each0").fix, "baz").fix, Ident("elem").fix).fix),
                                Js.Block(List(
                                  Js.VarDef(List("each2" ->
                                    Obj(ListMap(
                                      "0" -> Select(Ident("each1").fix, "bar").fix,
                                      "1" -> Select(Ident("each1").fix, "baz").fix)).fix.toJs)),
                                  Js.Call(Js.Select(Js.Ident("rez"), "push"), List(
                                    Js.AnonElem(List(
                                      Js.Call(Js.Ident("ObjectId"), Nil),
                                      Js.Ident("each2"))))))))))))),
                        Js.Return(Js.Ident("rez")))),
                      List(Js.Select(Js.This, IdLabel), Js.This)),
                    "map"),
                  List(
                    Js.AnonFunDecl(List("__rez"), List(
                      Js.Call(Js.Select(Js.Ident("emit"), "apply"), List(Js.Null, Js.Ident("__rez"))))))))),
              $Reduce.reduceNOP,
              scope = $SimpleMap.implicitScope(Set("clone")))),
          List(
            $Project((),
              Reshape(ListMap(
                BsonField.Name("0") -> -\/($field("value", "0")),
                BsonField.Name("1") -> -\/($field("value", "1")))),
              IgnoreId))))
    }
  }

  "SimpleMap" should {
    import JsCore._

    "raw" should {
      "extract one" in {
        val op = $SimpleMap((),
          NonEmptyList(MapExpr(JsFn(Ident("x"), Select(Ident("x").fix, "foo").fix))),
          ListMap())
        (op.raw match {
          case $Map(_, fn, _) =>
            fn.pprint(0) must_== "function (key, value) { return [key, (value != null) ? value.foo : undefined] }"
          case _ => failure
        }): org.specs2.execute.Result
      }

      "flatten one" in {
        val op = $SimpleMap((),
          NonEmptyList(FlatExpr(JsFn(Ident("x"), Select(Ident("x").fix, "foo").fix))),
          ListMap())
        (op.raw match {
          case $FlatMap(_, fn, _) =>
            fn.pprint(0) must_==
              """function (key, value) {
                |  var rez = [];
                |  for (var elem in ((value != null) ? value.foo : undefined)) {
                |    var each0 = clone(value);
                |    each0.foo = ((value != null) && (value.foo != null)) ? value.foo[elem] : undefined;
                |    rez.push([ObjectId(), each0])
                |  };
                |  return rez
                |}""".stripMargin
          case _ => failure
        }): org.specs2.execute.Result
      }
    }
  }

  "$redact" should {

    "render result variables" in {
      $Redact.DESCEND.bson.repr must_== "$$DESCEND"
      $Redact.PRUNE.bson.repr   must_== "$$PRUNE"
      $Redact.KEEP.bson.repr    must_== "$$KEEP"
    }
  }

  "RenderTree[Workflow]" should {
    def render(op: Workflow)(implicit RO: RenderTree[Workflow]): String = RO.render(op).draw.mkString("\n")

    "render read" in {
      render(readFoo) must_== "$Read(db; foo)"
    }

    "render simple project" in {
      val op = chain(readFoo,
        $project(
          Reshape(ListMap(
            BsonField.Name("bar") -> -\/($field("baz")))),
          IncludeId))

      render(op) must_==
        """Chain
          |├─ $Read(db; foo)
          |╰─ $Project
          |   ├─ Name(bar -> $baz)
          |   ╰─ IncludeId""".stripMargin
    }

    "render nested project" in {
      val op = chain(readFoo,
        $project(
          Reshape(ListMap(
            BsonField.Name("bar") -> \/-(Reshape(ListMap(
              BsonField.Name("0") -> -\/($field("baz"))))))),
          IncludeId))

      render(op) must_==
        """Chain
          |├─ $Read(db; foo)
          |╰─ $Project
          |   ├─ Name(bar)
          |   │  ╰─ Name(0 -> $baz)
          |   ╰─ IncludeId""".stripMargin
    }

    "render map/reduce ops" in {
      val op = chain(readFoo,
        $map(Js.AnonFunDecl(List("key"), Nil), ListMap()),
        $project(Reshape(ListMap(
          BsonField.Name("bar") -> -\/($field("baz")))),
          IncludeId),
        $flatMap(Js.AnonFunDecl(List("key"), Nil), ListMap()),
        $reduce(
          Js.AnonFunDecl(List("key", "values"),
            List(Js.Return(Js.Access(Js.Ident("values"), Js.Num(1, false))))),
          ListMap()))

      render(op) must_==
        """Chain
          |├─ $Read(db; foo)
          |├─ $Map
          |│  ├─ JavaScript(function (key) {})
          |│  ╰─ Scope(Map())
          |├─ $Project
          |│  ├─ Name(bar -> $baz)
          |│  ╰─ IncludeId
          |├─ $FlatMap
          |│  ├─ JavaScript(function (key) {})
          |│  ╰─ Scope(Map())
          |╰─ $Reduce
          |   ├─ JavaScript(function (key, values) { return values[1] })
          |   ╰─ Scope(Map())""".stripMargin
    }

    "render unchained" in {
      val op =
        $foldLeft(
          chain(readFoo,
            $project(Reshape(ListMap(
              BsonField.Name("bar") -> -\/($field("baz")))),
              IncludeId)),
          chain(readFoo,
            $map(Js.AnonFunDecl(List("key"), Nil), ListMap()),
            $reduce($Reduce.reduceNOP, ListMap())))

      render(op) must_==
      """$FoldLeft
        |├─ Chain
        |│  ├─ $Read(db; foo)
        |│  ╰─ $Project
        |│     ├─ Name(bar -> $baz)
        |│     ╰─ IncludeId
        |╰─ Chain
        |   ├─ $Read(db; foo)
        |   ├─ $Map
        |   │  ├─ JavaScript(function (key) {})
        |   │  ╰─ Scope(Map())
        |   ╰─ $Reduce
        |      ├─ JavaScript(function (key, values) { return (values != null) ? values[0] : undefined })
        |      ╰─ Scope(Map())""".stripMargin
    }
  }
}
