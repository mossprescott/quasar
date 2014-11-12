package slamdata.engine.physical.mongodb

import slamdata.engine._
import slamdata.engine.fp._
import slamdata.engine.fs.Path
import slamdata.engine.analysis.fixplate._
import slamdata.engine.analysis._
import slamdata.engine.sql.{SQLParser, Query}
import slamdata.engine.std._
import slamdata.engine.javascript._

import scalaz._
import Scalaz._

import collection.immutable.ListMap

import org.specs2.mutable._
import org.specs2.matcher.{Matcher, Expectable}
import slamdata.specs2._

class PlannerSpec extends Specification with CompilerHelpers with PendingWithAccurateCoverage {
  import StdLib._
  import structural._
  import math._
  import LogicalPlan._
  import SemanticAnalysis._
  import Reshape._
  import Workflow._
  import ExprOp._
  import IdHandling._

  case class equalToWorkflow(expected: Workflow)
      extends Matcher[Workflow] {
    def apply[S <: Workflow](s: Expectable[S]) = {
      def diff(l: S, r: Workflow): String = {
        val lt = RenderTree[Workflow].render(l)
        val rt = RenderTree[Workflow].render(r)
        RenderTree.show(lt diff rt)(new RenderTree[RenderedTree] {
          override def render(v: RenderedTree) = v
        }).toString
      }
      result(expected == s.value,
             "\ntrees are equal:\n" + diff(s.value, expected),
             "\ntrees are not equal:\n" + diff(s.value, expected),
             s)
    }
  }

  val queryPlanner = MongoDbPlanner.queryPlanner(_ => Cord.empty)

  def plan(query: String): Either[Error, Workflow] = {
    queryPlanner(QueryRequest(Query(query), None))._2.toEither
  }

  def plan(logical: Term[LogicalPlan]): Either[Error, Workflow] =
    (for {
      simplified <- \/-(Optimizer.simplify(logical))
      phys <- MongoDbPlanner.plan(simplified)
    } yield phys).toEither

  def beWorkflow(wf: Workflow) = beRight(equalToWorkflow(wf))

  "plan from query string" should {
    "plan simple constant example 1" in {
      plan("select 1") must
        beWorkflow($pure(Bson.Doc(ListMap("0" -> Bson.Int64(1)))))
    }.pendingUntilFixed

    "plan simple select *" in {
      plan("select * from foo") must beWorkflow($read(Collection("foo")))
    }

    "plan count(*)" in {
      plan("select count(*) from foo") must beWorkflow( 
        chain(
          $read(Collection("foo")),
          $group(
            Grouped(ListMap(BsonField.Name("0") -> Count)),
            -\/(Literal(Bson.Null)))))
    }

    "plan simple field projection on single set" in {
      plan("select foo.bar from foo") must
        beWorkflow(chain(
          $read(Collection("foo")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("bar") -> -\/(DocField(BsonField.Name("bar"))))),
            IgnoreId)))
    }

    "plan simple field projection on single set when table name is inferred" in {
      plan("select bar from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("bar") -> -\/(DocField(BsonField.Name("bar"))))),
           IgnoreId)))
    }
    
    "plan multiple field projection on single set when table name is inferred" in {
      plan("select bar, baz from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("bar") -> -\/(DocField(BsonField.Name("bar"))),
           BsonField.Name("baz") -> -\/(DocField(BsonField.Name("baz"))))),
           IgnoreId)))
    }

    "plan simple addition on two fields" in {
      plan("select foo + bar from baz") must
       beWorkflow(chain(
         $read(Collection("baz")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") -> -\/ (ExprOp.Add(
             DocField(BsonField.Name("foo")),
             DocField(BsonField.Name("bar")))))),
           IgnoreId)))
    }
    
    "plan concat" in {
      plan("select concat(bar, baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") -> -\/ (ExprOp.Concat(
             DocField(BsonField.Name("bar")),
             DocField(BsonField.Name("baz")),
             Nil)))),
           IgnoreId)))
    }

    "plan lower" in {
      plan("select lower(bar) from foo") must
      beWorkflow(chain(
        $read(Collection("foo")),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("0") ->
            -\/(ExprOp.ToLower(DocField(BsonField.Name("bar")))))),
          IgnoreId)))
    }

    "plan coalesce" in {
      plan("select coalesce(bar, baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(ExprOp.IfNull(
               DocField(BsonField.Name("bar")),
               DocField(BsonField.Name("baz")))))),
           IgnoreId)))
    }

    "plan date field extraction" in {
      plan("select date_part('day', baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(ExprOp.DayOfMonth(DocField(BsonField.Name("baz")))))),
           IgnoreId)))
    }

    "plan complex date field extraction" in {
      plan("select date_part('quarter', baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(
               ExprOp.Add(
                 ExprOp.Divide(
                   ExprOp.DayOfYear(DocField(BsonField.Name("baz"))),
                   ExprOp.Literal(Bson.Int32(92))),
                 ExprOp.Literal(Bson.Int32(1)))))),
           IgnoreId)))
    }

    "plan date field extraction: 'dow'" in {
      plan("select date_part('dow', baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/ (ExprOp.Add(
               ExprOp.DayOfWeek(ExprOp.DocField(BsonField.Name("baz"))),
               ExprOp.Literal(Bson.Int64(-1)))))),
           IgnoreId)))
    }

    "plan date field extraction: 'isodow'" in {
      plan("select date_part('isodow', baz) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/ (ExprOp.Cond(
               ExprOp.Eq(
                 ExprOp.DayOfWeek(ExprOp.DocField(BsonField.Name("baz"))),
                 ExprOp.Literal(Bson.Int64(1))),
               ExprOp.Literal(Bson.Int64(7)),
               ExprOp.Add(
                 ExprOp.DayOfWeek(ExprOp.DocField(BsonField.Name("baz"))),
                 ExprOp.Literal(Bson.Int64(-1))))))),
           IgnoreId)))
    }

    "plan filter array element" in {
      plan("select loc from zips where loc[0] < -73") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $match(Selector.Doc(
          BsonField.Name("loc") \ BsonField.Index(0) -> Selector.Lt(Bson.Int64(-73)))),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("loc") -> -\/(ExprOp.DocField(BsonField.Name("loc"))))),
          IgnoreId)))
    }

    "plan select array element" in {
      import Js._
      plan("select loc[0] from zips") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("value") ->
            -\/(ExprOp.DocField(BsonField.Name("loc"))))),
          IgnoreId),
        $map($Map.mapMap("value",
          Access(Access(Ident("value"), Str("value")), Num(0.0, false)))),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("0") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
          IgnoreId)))
    }

    "plan array length" in {
      plan("select array_length(bar, 1) from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(ExprOp.Size(DocField(BsonField.Name("bar")))))),
           IgnoreId)))
    }

    "plan sum in expression" in {
      plan("select sum(pop) * 100 from zips") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $group(
          Grouped(ListMap(BsonField.Name("value") ->
            Sum(ExprOp.DocField(BsonField.Name("pop"))))),
          -\/(Literal(Bson.Null))),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("0") ->
            -\/(ExprOp.Multiply(
              DocField(BsonField.Name("value")),
              ExprOp.Literal(Bson.Int64(100)))))),
          IncludeId)))
    }

    "plan conditional" in {
      plan("select case when pop < 10000 then city else loc end from zips") must
       beWorkflow(chain(
         $read(Collection("zips")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(Cond(
               Lt(
                 DocField(BsonField.Name("pop")),
                 ExprOp.Literal(Bson.Int64(10000))),
               DocField(BsonField.Name("city")),
               DocField(BsonField.Name("loc")))))),
           IncludeId)))
    }

    "plan negate" in {
      plan("select -bar from foo") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") ->
             -\/(ExprOp.Multiply(
               ExprOp.Literal(Bson.Int32(-1)),
               DocField(BsonField.Name("bar")))))),
           IgnoreId)))
    }

    "plan simple filter" in {
      plan("select * from foo where bar > 10") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.Doc(BsonField.Name("bar") -> Selector.Gt(Bson.Int64(10))))))
    }
    
    "plan simple reversed filter" in {
      plan("select * from foo where 10 < bar") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.Doc(BsonField.Name("bar") -> Selector.Gt(Bson.Int64(10))))))
    }
    
    "plan simple filter with expression in projection" in {
      plan("select a + b from foo where bar > 10") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.Doc(BsonField.Name("bar") -> Selector.Gt(Bson.Int64(10)))),
         $project(Reshape.Doc(ListMap(
           BsonField.Name("0") -> -\/ (ExprOp.Add(
             DocField(BsonField.Name("a")),
             DocField(BsonField.Name("b")))))),
           IgnoreId)))
    }

    "plan simple js filter" in {
      import Js._
      plan("select * from zips where length(city) < 4") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $match(
          Selector.Where(BinOp("<",
            Select(Select(Ident("this"), "city"), "length"),
            Num(4, false))))))
    }

    "plan filter with js and non-js" in {
      import Js._
      plan("select * from zips where length(city) < 4 and pop < 20000") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $match(Selector.And(
          Selector.Where(BinOp("<",
            Select(Select(Ident("this"), "city"), "length"),
            Num(4, false))),
          Selector.Doc(BsonField.Name("pop") ->
            Selector.Lt(Bson.Int64(20000)))))))
    }

    "plan filter with between" in {
      plan("select * from foo where bar between 10 and 100") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.And(
             Selector.Doc(BsonField.Name("bar") ->
               Selector.Gte(Bson.Int64(10))),
             Selector.Doc(BsonField.Name("bar") ->
               Selector.Lte(Bson.Int64(100)))))))
    }
    
    "plan filter with like" in {
      plan("select * from foo where bar like 'A%'") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(Selector.Doc(
           BsonField.Name("bar") ->
             Selector.Regex("^A.*$", false, false, false, false)))))
    }
    
    "plan filter with LIKE and OR" in {
      plan("select * from foo where bar like 'A%' or bar like 'Z%'") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.Or(
             Selector.Doc(BsonField.Name("bar") ->
               Selector.Regex("^A.*$", false, false, false, false)),
             Selector.Doc(BsonField.Name("bar") ->
               Selector.Regex("^Z.*$", false, false, false, false))))))
    }
    
    "plan filter with negate(s)" in {
      plan("select * from foo where bar != -10 and baz > -1.0") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.And(
             Selector.Doc(BsonField.Name("bar") ->
               Selector.Neq(Bson.Int64(-10))),
             Selector.Doc(BsonField.Name("baz") ->
               Selector.Gt(Bson.Dec(-1.0)))))))
    }
    
    "plan complex filter" in {
      plan("select * from foo where bar > 10 and (baz = 'quux' or foop = 'zebra')") must
       beWorkflow(chain(
         $read(Collection("foo")),
         $match(
           Selector.And(
             Selector.Doc(BsonField.Name("bar") -> Selector.Gt(Bson.Int64(10))),
             Selector.Or(
               Selector.Doc(BsonField.Name("baz") ->
                 Selector.Eq(Bson.Text("quux"))),
               Selector.Doc(BsonField.Name("foop") ->
                 Selector.Eq(Bson.Text("zebra"))))))))
    }

    "plan simple sort with field in projection" in {
      plan("select bar from foo order by bar") must
        beWorkflow(chain(
          $read(Collection("foo")),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("__tmp0") -> \/- (Reshape.Doc(ListMap(
                BsonField.Name("bar") ->
                  -\/ (ExprOp.DocField(BsonField.Name("bar")))))),
              BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("bar")))))))),
            IgnoreId),
          $sort(NonEmptyList(BsonField.Name("__tmp1") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("bar") ->
              -\/ (ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("bar"))))),
            ExcludeId)))
    }
    
    
    "plan simple sort with wildcard" in {
      plan("select * from zips order by pop") must
        beWorkflow(chain(
          $read(Collection("zips")),
          // TODO: Simplify to this once we identify sort keys in projection
          // $sort(NonEmptyList(BsonField.Name("pop") -> Ascending))
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("__tmp0") -> \/-(Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("pop")))))),
              BsonField.Name("__tmp1") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
            ExcludeId),
          $sort(NonEmptyList(BsonField.Name("__tmp0") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap (
            BsonField.Name("value") ->
              -\/(ExprOp.DocField(BsonField.Name ("__tmp1"))))),
            ExcludeId)))
    }

    "plan sort with expression in key" in {
      plan("select baz from foo order by bar/10") must
        beWorkflow(chain(
          $read(Collection("foo")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp3") -> \/-(Reshape.Doc(ListMap(
              BsonField.Name("baz") -> -\/(ExprOp.DocField(BsonField.Name("baz")))))),
            BsonField.Name("__tmp4") -> \/-(Reshape.Arr(ListMap(
              BsonField.Index(0) -> -\/ (ExprOp.Divide(
                    ExprOp.DocField(BsonField.Name("bar")),
                    ExprOp.Literal(Bson.Int64(10))))))))),
            ExcludeId),
          $sort(NonEmptyList(BsonField.Name("__tmp4") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("baz") ->
              -\/(ExprOp.DocField(BsonField.Name("__tmp3") \ BsonField.Name("baz"))))),
            IgnoreId)))
    }

    "plan select with wildcard and field" in {
      import Js._

      plan("select *, pop from zips") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp0") -> \/-(Reshape.Doc(ListMap(
              BsonField.Name("pop") ->
                -\/(ExprOp.DocField(BsonField.Name("pop")))))),
            BsonField.Name("__tmp1") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
            IncludeId),
          $map($Map.mapMap("leftUnknown",
            Call(AnonFunDecl(List("rez"),
              List(
                ForIn(
                  Ident("attr"),
                  Select(Ident("leftUnknown"), "__tmp1"),
                  If(
                    Call(
                      Select(Select(Ident("leftUnknown"), "__tmp1"),
                        "hasOwnProperty"),
                      List(Ident("attr"))),
                    BinOp("=",
                      Access(Ident("rez"), Ident("attr")),
                      Access(Select(Ident("leftUnknown"), "__tmp1"),
                        Ident("attr"))),
                    None)),
                BinOp("=",
                  Access(Ident("rez"), Str("pop")),
                  Select(
                    Select(Ident("leftUnknown"), "__tmp0"),
                    "pop")),
                Return(Ident("rez")))),
              List(AnonObjDecl(Nil)))))))
    }

    "plan sort with wildcard and expression in key" in {
      import Js._

      plan("select * from zips order by pop/10 desc") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp3") -> \/-(Reshape.Doc(ListMap(
              BsonField.Name("__sd__0") -> -\/(ExprOp.Divide(
                ExprOp.DocField(BsonField.Name("pop")),
                ExprOp.Literal(Bson.Int64(10))))))),
            BsonField.Name("__tmp4") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
            IncludeId),
          $map($Map.mapMap("leftUnknown",
            Call(AnonFunDecl(List("rez"),
              List(
                ForIn(Ident("attr"),Select(Ident("leftUnknown"), "__tmp4"),If(Call(Select(Select(Ident("leftUnknown"), "__tmp4"), "hasOwnProperty"),List(Ident("attr"))),BinOp("=",Access(Ident("rez"),Ident("attr")),Access(Select(Ident("leftUnknown"), "__tmp4"),Ident("attr"))),None)),
                BinOp("=",Access(Ident("rez"),Str("__sd__0")),Select(Select(Ident("leftUnknown"),"__tmp3"),"__sd__0")), Return(Ident("rez")))),
              List(AnonObjDecl(Nil))))),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("__tmp5") -> \/-(Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("__sd__0")))))),
              BsonField.Name("__tmp6") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
            ExcludeId),
          $sort(NonEmptyList(BsonField.Name("__tmp5") \ BsonField.Index(0) -> Descending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name("__tmp6"))))),
            ExcludeId)))
    }
    
    "plan simple sort with field not in projections" in {
      plan("select name from person order by height") must
        beWorkflow(chain(
          $read(Collection("person")),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("__tmp0") -> \/- (Reshape.Doc(ListMap(
                BsonField.Name("name") -> -\/ (ExprOp.DocField(BsonField.Name("name")))))),
              BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("height")))))))),
            IgnoreId),
          $sort(NonEmptyList(BsonField.Name("__tmp1") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("name") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("name"))))),
            IgnoreId)))
    }
    
    "plan sort with expression and alias" in {
      plan("select pop/1000 as popInK from zips order by popInK") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp3") -> \/- (Reshape.Doc(ListMap(
              BsonField.Name("popInK") -> -\/ (ExprOp.Divide(ExprOp.DocField(BsonField.Name("pop")), ExprOp.Literal(Bson.Int64(1000))))))),
            BsonField.Name("__tmp4") -> \/- (Reshape.Arr(ListMap(
              BsonField.Index(0) -> -\/ (ExprOp.Divide(ExprOp.DocField(
                  BsonField.Name("pop")), ExprOp.Literal(Bson.Int64(1000))))))))),
            ExcludeId),
          $sort(NonEmptyList(BsonField.Name("__tmp4") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("popInK") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp3") \ BsonField.Name("popInK"))))),
            ExcludeId)))
    }
    
    "plan sort with filter" in {
      plan("select city, pop from zips where pop <= 1000 order by pop desc, city") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $match(Selector.Doc(BsonField.Name("pop") -> Selector.Lte(Bson.Int64(1000)))),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp9") -> \/-  (Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("city"))),
              BsonField.Name("pop") -> -\/(ExprOp.DocField(BsonField.Name("pop")))))),
            BsonField.Name("__tmp10") -> \/- (Reshape.Arr(ListMap(
              BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("pop"))),
              BsonField.Index(1) -> -\/ (ExprOp.DocField(BsonField.Name("city")))))))),
            IgnoreId),
          $sort(NonEmptyList(
            BsonField.Name("__tmp10") \ BsonField.Index(0) -> Descending,
            BsonField.Name("__tmp10") \ BsonField.Index(1) -> Ascending)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp9") \ BsonField.Name("city"))),
            BsonField.Name("pop") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp9") \ BsonField.Name("pop"))))),
            ExcludeId)))
    }
    
    "plan sort with expression, alias, and filter" in {
      plan("select pop/1000 as popInK from zips where pop >= 1000 order by popInK") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $match(Selector.Doc(BsonField.Name("pop") -> Selector.Gte(Bson.Int64(1000)))),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp6") -> \/-(Reshape.Doc(ListMap(BsonField.Name("popInK") -> -\/(ExprOp.Divide(ExprOp.DocField(BsonField.Name("pop")), ExprOp.Literal(Bson.Int64(1000))))))),
            BsonField.Name("__tmp7") -> \/-(Reshape.Arr(ListMap(BsonField.Index(0) -> -\/(ExprOp.Divide(ExprOp.DocField(BsonField.Name("pop")), ExprOp.Literal(Bson.Int64(1000))))))))),
            ExcludeId),
          $sort(NonEmptyList(BsonField.Name("__tmp7") \ BsonField.Index(0) -> Ascending)),
          $project(Reshape.Doc(ListMap(BsonField.Name("popInK") -> -\/(ExprOp.DocField(BsonField.Name("__tmp6") \ BsonField.Name("popInK"))))),
            ExcludeId))
        )
    }

    "plan multiple column sort with wildcard" in {
      plan("select * from zips order by pop, city desc") must
       beWorkflow(chain(
         $read(Collection("zips")),
         // TODO: Simplify to this once we identify sort keys in projection
         // $sort(NonEmptyList(
         //   BsonField.Name("pop") -> Ascending,
         //   BsonField.Name("city") -> Descending))
         $project(Reshape.Doc(ListMap(
           BsonField.Name("__tmp6") -> \/-(Reshape.Arr(ListMap(
             BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("pop"))),
             BsonField.Index(1) -> -\/(ExprOp.DocField(BsonField.Name("city")))))),
           BsonField.Name("__tmp7") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
           ExcludeId),
         $sort(NonEmptyList(
           BsonField.Name("__tmp6") \ BsonField.Index(0) -> Ascending,
           BsonField.Name("__tmp6") \ BsonField.Index(1) -> Descending)),
         $project(Reshape.Doc(ListMap (
           BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name ("__tmp7"))))),
           ExcludeId)))
    }
    
    "plan many sort columns" in {
      plan("select * from zips order by pop, state, city, a4, a5, a6") must
       beWorkflow(chain(
         $read(Collection("zips")),
         // TODO: Simplify to this once we identify sort keys in projection
         // Sort(NonEmptyList(
         //   BsonField.Name("pop") -> Ascending,
         //   BsonField.Name("state") -> Ascending,
         //   BsonField.Name("city") -> Ascending,
         //   BsonField.Name("a4") -> Ascending,
         //   BsonField.Name("a5") -> Ascending,
         //   BsonField.Name("a6") -> Ascending))
         $project(Reshape.Doc(ListMap(
           BsonField.Name("__tmp18") -> \/-(Reshape.Arr(ListMap(
             BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("pop"))),
             BsonField.Index(1) -> -\/(ExprOp.DocField(BsonField.Name("state"))),
             BsonField.Index(2) -> -\/(ExprOp.DocField(BsonField.Name("city"))),
             BsonField.Index(3) -> -\/(ExprOp.DocField(BsonField.Name("a4"))),
             BsonField.Index(4) -> -\/(ExprOp.DocField(BsonField.Name("a5"))),
             BsonField.Index(5) -> -\/(ExprOp.DocField(BsonField.Name("a6")))))),
           BsonField.Name("__tmp19") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
           ExcludeId),
         $sort(NonEmptyList(
           BsonField.Name("__tmp18") \ BsonField.Index(0) -> Ascending,
           BsonField.Name("__tmp18") \ BsonField.Index(1) -> Ascending,
           BsonField.Name("__tmp18") \ BsonField.Index(2) -> Ascending,
           BsonField.Name("__tmp18") \ BsonField.Index(3) -> Ascending,
           BsonField.Name("__tmp18") \ BsonField.Index(4) -> Ascending,
           BsonField.Name("__tmp18") \ BsonField.Index(5) -> Ascending)),
         $project(Reshape.Doc(ListMap (
           BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name("__tmp19"))))),
           ExcludeId)))
    }

    "plan efficient count and field ref" in {
      plan("SELECT city, COUNT(*) AS cnt FROM zips ORDER BY cnt DESC") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $group(
              Grouped(ListMap(
                BsonField.Name("city") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("city"))),
                BsonField.Name("cnt") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))))),
              -\/ (ExprOp.Literal(Bson.Null))),
            $unwind(ExprOp.DocField(BsonField.Name("city"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp5") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("cnt")))))),
              BsonField.Name("__tmp6") -> -\/ (ExprOp.DocVar.ROOT()))),
              ExcludeId),
            $sort(NonEmptyList(BsonField.Name("__tmp5") \ BsonField.Index(0) -> Descending)),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp6") \ BsonField.Name("city"))),
              BsonField.Name("cnt") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp6") \ BsonField.Name("cnt"))))),
              ExcludeId))
        }
    }

    "plan trivial group by" in {
      plan("select city from zips group by city") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
            BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("city")))))),
          BsonField.Name("__tmp2") -> -\/ (ExprOp.DocVar.ROOT()))),
          ExcludeId),
        $group(
          Grouped(ListMap(
            BsonField.Name("city") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("city"))))),
          -\/ (ExprOp.DocField(BsonField.Name("__tmp1")))),
        $unwind(ExprOp.DocField(BsonField.Name("city")))))
    }

    "plan trivial group by with wildcard" in {
      plan("select * from zips group by city") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
            BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("city")))))),
          BsonField.Name("__tmp2") -> -\/ (ExprOp.DocVar.ROOT()))),
          ExcludeId),
        $group(
          Grouped(ListMap(
            BsonField.Name("__tmp0") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("__tmp2"))))),
          -\/ (ExprOp.DocField(BsonField.Name("__tmp1")))),
        $unwind(ExprOp.DocField(BsonField.Name("__tmp0"))),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("value") -> -\/ (ExprOp.DocField(BsonField.Name("__tmp0"))))),
          ExcludeId)))
    }

    "plan count grouped by single field" in {
      plan("select count(*) from bar group by baz") must
        beWorkflow {
          chain(
            $read(Collection("bar")),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("baz")))))))),
              ExcludeId),
            $group(Grouped(ListMap(
              BsonField.Name("0") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))))),
              -\/(ExprOp.DocField(BsonField.Name("__tmp1")))))
        }
    }

    "plan count and sum grouped by single field" in {
      plan("select count(*) as cnt, sum(biz) as sm from bar group by baz") must
        beWorkflow {
          chain(
            $read(Collection("bar")),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("baz")))))),
              BsonField.Name("__tmp2") -> -\/ (ExprOp.DocVar.ROOT()))),
              ExcludeId),
            $group(
              Grouped(ListMap(
                BsonField.Name("cnt") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))),
                BsonField.Name("sm") -> ExprOp.Sum(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("biz"))))),
              -\/ (ExprOp.DocField(BsonField.Name("__tmp1")))))
        }
    }

    "plan sum grouped by single field with filter" in {
      plan("select sum(pop) as sm from zips where state='CO' group by city") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $match(Selector.Doc(
              BsonField.Name("state") -> Selector.Eq(Bson.Text("CO")))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp4") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("city")))))),
              BsonField.Name("__tmp5") -> -\/ (ExprOp.DocVar.ROOT()))),
              ExcludeId),
            $group(
              Grouped(ListMap(
                BsonField.Name("sm") -> ExprOp.Sum(ExprOp.DocField(BsonField.Name("__tmp5") \ BsonField.Name("pop"))))),
              -\/ (ExprOp.DocField(BsonField.Name("__tmp4")))))
        }
    }

    "plan count and field when grouped" in {
      plan("select count(*) as cnt, city from zips group by city") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("city")))))),
              BsonField.Name("__tmp2") -> -\/(ExprOp.DocVar.ROOT()))),
              ExcludeId),
            $group(
              Grouped(ListMap(
                BsonField.Name("cnt") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))),
                BsonField.Name("city") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("city"))))),
              -\/(ExprOp.DocField(BsonField.Name("__tmp1")))),
            $unwind(ExprOp.DocField(BsonField.Name("city"))))
            // $project(Reshape.Doc(ListMap(
            //   BsonField.Name("cnt") -> -\/(ExprOp.DocField(BsonField.Name("cnt"))),
            //   BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("city"))))),
            //   ExcludeId))  // Note: added in conversion to WorkflowTask
        }
    }

    "plan object flatten" in {
      import Js._

      plan("select geo{*} from usa_factbook") must
        beWorkflow {
          chain(
            $read(Collection("usa_factbook")),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("value") ->
                -\/(ExprOp.DocField(BsonField.Name("geo"))))),
              IgnoreId),
            $flatMap(
              AnonFunDecl(List("key", "value"), List(
                VarDef(List("rez" -> AnonElem(Nil))),
                ForIn(
                  Ident("attr"),
                  Access(Ident("value"), Str("value")),
                  Call(
                    Select(Ident("rez"), "push"),
                    List(
                      AnonElem(List(
                        Call(Ident("ObjectId"), Nil),
                        Access(
                          Access(Ident("value"), Str("value")),
                          Ident("attr"))))))),
                Return(Ident("rez"))))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("geo") -> -\/(ExprOp.DocVar(DocVar.ROOT, None)))),
              IgnoreId))
        }
    }

    "plan array project with concat" in {
      import Js._
      plan("select city, loc[0] from zips") must
        beWorkflow {
          chain(
            $foldLeft(
              chain(
                $read(Collection("zips")),
                $project(Reshape.Doc(ListMap(
                  BsonField.Name("__tmp0") -> \/-(Reshape.Doc(ListMap(
                    BsonField.Name("__tmp3") -> -\/(ExprOp.DocVar.ROOT())))))),
                  IncludeId)),
              chain(
                $read(Collection("zips")),
                $project(Reshape.Doc(ListMap(
                  BsonField.Name("value") ->
                    -\/(ExprOp.DocField(BsonField.Name("loc"))))),
                  IncludeId),
                $map($Map.mapMap("value",
                  Access(Access(Ident("value"), Str("value")), Num(0, false)))),
                $project(Reshape.Doc(ListMap(
                  BsonField.Name("__tmp1") -> -\/(ExprOp.DocVar.ROOT()))),
                  IncludeId))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("city") ->
                -\/(ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("__tmp3") \ BsonField.Name("city"))),
              BsonField.Name("1") ->
                -\/(ExprOp.DocField(BsonField.Name("__tmp1"))))),
              IgnoreId))
        }
    }

    "plan array flatten" in {
      plan("select loc[*] from zips") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $project(Reshape.Doc(ListMap(BsonField.Name("value") -> -\/ (ExprOp.DocField(BsonField.Name("loc"))))),
              IgnoreId),
            $unwind(ExprOp.DocField(BsonField.Name("value"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("loc") -> -\/(ExprOp.DocField(BsonField.Name("value"))))),
              IgnoreId))  // Note: becomes ExcludeId in conversion to WorkflowTask
        }
    }

    "plan limit with offset" in {
      plan("SELECT * FROM zips LIMIT 5 OFFSET 100") must
        beWorkflow(chain($read(Collection("zips")), $limit(105), $skip(100)))
    }

    "plan filter and limit" in {
      plan("SELECT city, pop FROM zips ORDER BY pop DESC LIMIT 5") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("__tmp0") -> \/-(Reshape.Doc(ListMap(
                BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("city"))),
                BsonField.Name("pop") -> -\/(ExprOp.DocField(BsonField.Name("pop")))))),
              BsonField.Name("__tmp1") -> \/-(Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("pop")))))))),
              IgnoreId),
            $sort(NonEmptyList(BsonField.Name("__tmp1") \ BsonField.Index(0) -> Descending)),
            $limit(5),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("city"))),
              BsonField.Name("pop") -> -\/(ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("pop"))))),
              ExcludeId))
        }
    }

    "plan simple single field selection and limit" in {
      plan("SELECT city FROM zips LIMIT 5") must
        beWorkflow {
          chain(
            $read(Collection("zips")),
            $project(Reshape.Doc(ListMap(BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("city"))))),
              IgnoreId),
            $limit(5))
        }
    }

    "plan complex group by with sorting and limiting" in {
      plan("SELECT city, SUM(pop) AS pop FROM zips GROUP BY city ORDER BY pop") must
        beWorkflow {
          $read(Collection("zips"))
        }
    }.pendingUntilFixed

    "plan simple distinct" in {
      plan("select distinct city, state from zips") must 
      beWorkflow(
        chain(
          $read(Collection("zips")),
          $project(Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("city"))),
              BsonField.Name("state") -> -\/ (ExprOp.DocField(BsonField.Name("state"))))),
            IgnoreId),
          $group(
            Grouped(ListMap(
              BsonField.Name("value") -> ExprOp.First(ExprOp.DocVar.ROOT()))),
              \/- (Reshape.Arr(ListMap(
                BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("city"))),
                BsonField.Index(1) -> -\/ (ExprOp.DocField(BsonField.Name("state"))))))),
          $project(Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("city"))),
              BsonField.Name("state") -> -\/ (ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("state"))))),
            ExcludeId)))
    }

    "plan distinct as expression" in {
      plan("select count(distinct(city)) from zips") must 
        beWorkflow(chain(
          $read(Collection("zips")),
          $group(
            Grouped(ListMap(
              BsonField.Name("value") -> ExprOp.First(ExprOp.DocField(BsonField.Name("city"))))),
              -\/ (ExprOp.DocField(BsonField.Name("city")))),
          $group(
            Grouped(ListMap(
              BsonField.Name("0") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))))),
              -\/ (ExprOp.Literal(Bson.Null)))))
    }

    "plan distinct of expression as expression" in {
      plan("select count(distinct substring(city, 0, 1)) from zips") must 
        beWorkflow(chain(
          $read(Collection("zips")),
          $group(
            Grouped(ListMap(
              BsonField.Name("value") -> ExprOp.First(
                ExprOp.Substr(
                  ExprOp.DocField(BsonField.Name("city")),
                  ExprOp.Literal(Bson.Int64(0)),
                  ExprOp.Literal(Bson.Int64(1)))))),
              -\/ (ExprOp.Substr(
                ExprOp.DocField(BsonField.Name("city")),
                ExprOp.Literal(Bson.Int64(0)),
                ExprOp.Literal(Bson.Int64(1))))),
          $group(
            Grouped(ListMap(
              BsonField.Name("0") -> ExprOp.Sum(ExprOp.Literal(Bson.Int32(1))))),
              -\/ (ExprOp.Literal(Bson.Null)))))
    }

    "plan distinct of wildcard" in {
      plan("select distinct * from zips") must 
        beWorkflow(
          $read(Collection("zips")))
    }.pendingUntilFixed("#283")

    "plan distinct of wildcard as expression" in {
      plan("select count(distinct *) from zips") must 
        beWorkflow(
          $read(Collection("zips")))
    }.pendingUntilFixed("#283")

    "plan distinct with simple order by" in {
      plan("select distinct city from zips order by city") must
        beWorkflow(
          chain(
              $read(Collection("zips")),
              $project(Reshape.Doc(ListMap(
                BsonField.Name("__tmp0") -> \/-(Reshape.Doc(ListMap(
                  BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("city")))))),
                BsonField.Name("__tmp1") -> \/-(Reshape.Arr(ListMap(
                  BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("city")))))))),
                IgnoreId),
              $sort(NonEmptyList(
                BsonField.Name("__tmp1") \ BsonField.Index(0) -> Ascending)),
              $group(
                Grouped(ListMap(
                  BsonField.Name("value") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp0"))),
                  BsonField.Name("__sd_key_0") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp1") \ BsonField.Index(0))))),
                -\/(ExprOp.DocField(BsonField.Name("__tmp0")))),
              $sort(NonEmptyList(BsonField.Name("__sd_key_0") -> Ascending)),
              $project(Reshape.Doc(ListMap(
                BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("city"))))),
                ExcludeId)))
    }

    "plan distinct with unrelated order by" in {
      plan("select distinct city from zips order by pop desc") must
        beWorkflow(
          chain(
              $read(Collection("zips")),
              $project(
                Reshape.Doc(ListMap(
                  BsonField.Name("__tmp0") -> \/- (Reshape.Doc(ListMap(
                    BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("city"))),
                    BsonField.Name("__sd__0") -> -\/ (ExprOp.DocField(BsonField.Name("pop")))))),
                  BsonField.Name("__tmp1") -> \/- (Reshape.Arr(ListMap(
                    BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("pop")))))))),
                IgnoreId),
              $sort(NonEmptyList(
                BsonField.Name("__tmp1") \ BsonField.Index(0) -> Descending)),
              $project(
                Reshape.Doc(ListMap(
                  BsonField.Name("__tmp2") -> \/- (Reshape.Arr(ListMap(
                    BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("__tmp0") \ BsonField.Name("city")))))),
                  BsonField.Name("__tmp3") -> -\/ (ExprOp.DocVar.ROOT()))),
                ExcludeId),
              $group(
                Grouped(ListMap(
                  BsonField.Name("value") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp3") \ BsonField.Name("__tmp0"))),
                  BsonField.Name("__sd_key_0") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp3") \ BsonField.Name("__tmp1") \ BsonField.Index(0))))),
                -\/(ExprOp.DocField(BsonField.Name("__tmp2")))),
              $sort(NonEmptyList(
                BsonField.Name("__sd_key_0") -> Descending)),
              $project(
                Reshape.Doc(ListMap(
                  BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("city"))))),
                IgnoreId)))
    }

    "plan distinct as function with group" in {
      plan("select state, count(distinct(city)) from zips group by state") must 
        beWorkflow(
          $read(Collection("zips")))
    }.pendingUntilFixed
    
    "plan distinct with sum and group" in {
      plan("SELECT DISTINCT SUM(pop) AS totalPop, city FROM zips GROUP BY city") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("__tmp1") -> \/-(Reshape.Arr(ListMap(
              BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("city")))))),
            BsonField.Name("__tmp2") -> -\/(ExprOp.DocVar.ROOT()))),
            ExcludeId),
          $group(
            Grouped(ListMap(
              BsonField.Name("totalPop") -> ExprOp.Sum(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("pop"))),
              BsonField.Name("city") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("city"))))),
            -\/ (ExprOp.DocField(BsonField.Name("__tmp1")))),
          $unwind(ExprOp.DocField(BsonField.Name("city"))),
          $group(
            Grouped(ListMap(
              BsonField.Name("value") -> ExprOp.First(ExprOp.DocVar.ROOT()))),
            \/-(Reshape.Arr(ListMap(
              BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("totalPop"))),
              BsonField.Index(1) -> -\/ (ExprOp.DocField(BsonField.Name("city"))))))),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("totalPop") -> -\/ (ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("totalPop"))),
              BsonField.Name("city") -> -\/ (ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("city"))))),
              ExcludeId)))
    }
    
    "plan distinct with sum, group, and orderBy" in {
      plan("SELECT DISTINCT SUM(pop) AS totalPop, city FROM zips GROUP BY city ORDER BY totalPop DESC") must
        beWorkflow(
          chain(
              $read(Collection("zips")),
              $project(
                Reshape.Doc(ListMap(
                  BsonField.Name("__tmp1") -> \/-(Reshape.Arr(ListMap(
                    BsonField.Index(0) -> -\/(ExprOp.DocField(BsonField.Name("city")))))),
                  BsonField.Name("__tmp2") -> -\/(ExprOp.DocVar.ROOT()))),
                ExcludeId),
              $group(
                Grouped(ListMap(
                  BsonField.Name("__sd_tmp_1") -> ExprOp.Sum(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("pop"))),
                  BsonField.Name("__sd_tmp_2") -> ExprOp.Push(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("city"))),
                  BsonField.Name("__sd_tmp_3") -> ExprOp.Sum(ExprOp.DocField(BsonField.Name("__tmp2") \ BsonField.Name("pop"))))),
                -\/ (ExprOp.DocField(BsonField.Name("__tmp1")))),
              $project(Reshape.Doc(ListMap(
                BsonField.Name("__tmp7") -> \/-(Reshape.Doc(ListMap(
                  BsonField.Name("totalPop") -> -\/(ExprOp.DocField(BsonField.Name("__sd_tmp_1"))),
                  BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("__sd_tmp_2")))))),
                BsonField.Name("__tmp8") -> \/-(Reshape.Doc(ListMap(
                  BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name("__sd_tmp_3")))))))),
                IgnoreId),
              $unwind(ExprOp.DocField(BsonField.Name("__tmp7") \ BsonField.Name("city"))),
              $project(
                Reshape.Doc(ListMap(
                  BsonField.Name("__tmp5") -> \/- (Reshape.Arr(ListMap(
                    BsonField.Index(0) -> -\/ (ExprOp.DocField(BsonField.Name("__tmp8") \ BsonField.Name("value")))))),
                  BsonField.Name("__tmp6") -> -\/ (ExprOp.DocVar.ROOT()))),
                ExcludeId),
              $sort(NonEmptyList(
                BsonField.Name("__tmp5") \ BsonField.Index(0) -> Descending)),
              $group(
                Grouped(ListMap(
                  BsonField.Name("value") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp6") \ BsonField.Name("__tmp7"))),
                  BsonField.Name("__sd_key_0") -> ExprOp.First(ExprOp.DocField(BsonField.Name("__tmp5") \ BsonField.Index(0))))),
                -\/(ExprOp.DocField(BsonField.Name("__tmp6") \ BsonField.Name("__tmp7")))),
              $sort(NonEmptyList(BsonField.Name("__sd_key_0") -> Descending)),
              $project(Reshape.Doc(ListMap(
                BsonField.Name("totalPop") -> -\/(ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("totalPop"))),
                BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("value") \ BsonField.Name("city"))))),
                ExcludeId)))

    }
    
    "plan select length()" in {
      plan("select length(city) from zips") must
        beWorkflow(chain(
          $read(Collection("zips")),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name("city"))))),
            IgnoreId),
          $simpleMap(x => JsCore.Select(JsCore.Select(x, "value").fix, "length").fix),
          $project(
            Reshape.Doc(ListMap(
              BsonField.Name("0") -> -\/(ExprOp.DocVar.ROOT()))),
            IgnoreId)))
    }
    
    "plan select length() and simple field" in {
      plan("select city, length(city) from zips") must
      beWorkflow(chain(
        $read(Collection("zips")),
        $project(
          Reshape.Doc(ListMap(
            BsonField.Name("__tmp2") -> \/-(Reshape.Doc(ListMap(
              BsonField.Name("value") -> -\/(ExprOp.DocField(BsonField.Name("city")))))),
            BsonField.Name("__tmp3") -> -\/(ExprOp.DocVar.ROOT()))),
          IncludeId),
        $simpleMap(value => JsCore.Obj(ListMap(
          "__tmp0" -> JsCore.Select(JsCore.Select(JsCore.Select(value, "__tmp2").fix, "value").fix, "length").fix,
          "__tmp1" -> JsCore.Select(value, "__tmp3").fix)).fix),
        $project(
          Reshape.Doc(ListMap(
            BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("__tmp1") \ BsonField.Name("city"))),
            BsonField.Name("1") -> -\/(ExprOp.DocField(BsonField.Name("__tmp0"))))),
          IgnoreId)))
    }
    
    "plan combination of two distinct sets" in {
      plan("SELECT (DISTINCT foo.bar) + (DISTINCT foo.baz) FROM foo") must
        beWorkflow(
          $read(Collection("zips")))
    }.pendingUntilFixed
    
    import Js._

    def joinStructure(
      left: Workflow, right: Workflow,
      leftKey: ExprOp, rightKey: Js.Expr,
      fin: WorkflowOp) = {
      def initialPipeOps(src: Workflow): Workflow =
        chain(
          src,
          $group(
            Grouped(ListMap(BsonField.Name("left") ->
              ExprOp.AddToSet(ExprOp.DocVar.ROOT()))),
            -\/(leftKey)),
          $project(Reshape.Doc(ListMap(
            BsonField.Name("left") ->
              -\/(ExprOp.DocField(BsonField.Name("left"))),
            BsonField.Name("right") -> -\/(ExprOp.Literal(Bson.Arr(List()))))),
            IncludeId))
      fin(
        $foldLeft(
          initialPipeOps(left),
          chain(
            right,
            $map($Map.mapKeyVal(("key", "value"),
              rightKey,
              AnonObjDecl(List(
                ("left", AnonElem(List())),
                ("right", AnonElem(List(Ident("value")))))))),
            $reduce(
              AnonFunDecl(List("key", "values"),
                List(
                  VarDef(List(
                    ("result", AnonObjDecl(List(
                      ("left", AnonElem(List())),
                      ("right", AnonElem(List()))))))),
                  Call(Select(Ident("values"), "forEach"),
                    List(AnonFunDecl(List("value"),
                      List(
                        BinOp("=",
                          Select(Ident("result"), "left"),
                          Call(
                            Select(Select(Ident("result"), "left"), "concat"),
                            List(Select(Ident("value"), "left")))),
                        BinOp("=",
                          Select(Ident("result"), "right"),
                          Call(
                            Select(Select(Ident("result"), "right"), "concat"),
                            List(Select(Ident("value"), "right")))))))),
                  Return(Ident("result"))))))))
    }
            
    "plan simple join" in {
      plan("select zips2.city from zips join zips2 on zips._id = zips2._id") must
        beWorkflow(
          joinStructure(
            $read(Collection("zips")),
            $read(Collection("zips2")),
            ExprOp.DocField(BsonField.Name("_id")),
            Select(Ident("value"), "_id"),
            chain(_,
              $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
                BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
                BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
              $unwind(ExprOp.DocField(BsonField.Name("left"))),
              $unwind(ExprOp.DocField(BsonField.Name("right"))),
              $project(Reshape.Doc(ListMap(
                BsonField.Name("city") ->
                  -\/(ExprOp.DocField(BsonField.Name("right") \ BsonField.Name("city"))))),
                IgnoreId))))  // Note: becomes ExcludeId in conversion to WorkflowTask
    }

    "plan non-equi join" in {
      plan("select zips2.city from zips join zips2 on zips._id < zips2._id") must beLeft
    }

    "plan simple inner equi-join" in {
      plan(
        "select foo.name, bar.address from foo join bar on foo.id = bar.foo_id") must
      beWorkflow(
        joinStructure(
          $read(Collection("foo")),
          $read(Collection("bar")),
          ExprOp.DocField(BsonField.Name("id")),
          Select(Ident("value"), "foo_id"),
          chain(_,
            $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
              BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
              BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
            $unwind(ExprOp.DocField(BsonField.Name("left"))),
            $unwind(ExprOp.DocField(BsonField.Name("right"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("name") ->
                -\/(ExprOp.DocField(BsonField.Name("left") \ BsonField.Name("name"))),
              BsonField.Name("address") ->
                -\/(ExprOp.DocField(BsonField.Name("right") \ BsonField.Name("address"))))),
              IgnoreId))))  // Note: becomes ExcludeId in conversion to WorkflowTask
    }

    "plan simple inner equi-join with wildcard" in {
      plan("select * from foo join bar on foo.id = bar.foo_id") must
      beWorkflow(
        joinStructure(
          $read(Collection("foo")),
          $read(Collection("bar")),
          ExprOp.DocField(BsonField.Name("id")),
          Select(Ident("value"), "foo_id"),
          chain(_,
            $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
              BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
              BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
            $unwind(ExprOp.DocField(BsonField.Name("left"))),
            $unwind(ExprOp.DocField(BsonField.Name("right"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("left") ->
                -\/(ExprOp.DocField(BsonField.Name("left"))),
              BsonField.Name("right") ->
                -\/(ExprOp.DocField(BsonField.Name("right"))))),
              IgnoreId),
            $map(
              AnonFunDecl(List("key", "bothProjects"), List(
                Return(AnonElem(List(Ident("key"), Call(
                  AnonFunDecl(List("rez"), List(
                    ForIn(Ident("attr"),Select(Ident("bothProjects"), "left"),If(Call(Select(Select(Ident("bothProjects"), "left"), "hasOwnProperty"), List(Ident("attr"))), BinOp("=",Access(Ident("rez"), Ident("attr")), Access(Select(Ident("bothProjects"), "left"), Ident("attr"))), None)),
                    ForIn(Ident("attr"), Select(Ident("bothProjects"), "right"), If(Call(Select(Select(Ident("bothProjects"), "right"), "hasOwnProperty"), List(Ident("attr"))), BinOp("=", Access(Ident("rez"), Ident("attr")), Access(Select(Ident("bothProjects"), "right"), Ident("attr"))), None)),
                    Return(Ident("rez")))),
                  List(AnonObjDecl(List()))))))))))))
    }

    "plan simple left equi-join" in {
      plan(
        "select foo.name, bar.address " +
          "from foo left join bar on foo.id = bar.foo_id") must
      beWorkflow(
        joinStructure(
          $read(Collection("foo")),
          $read(Collection("bar")),
          ExprOp.DocField(BsonField.Name("id")),
          Select(Ident("value"), "foo_id"),
          chain(_,
            $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
              BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0))))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("left") -> -\/(ExprOp.DocField(BsonField.Name("left"))),
              BsonField.Name("right") -> -\/(ExprOp.Cond(
                ExprOp.Eq(
                  ExprOp.Size(ExprOp.DocField(BsonField.Name("right"))),
                  ExprOp.Literal(Bson.Int32(0))),
                ExprOp.Literal(Bson.Arr(List(Bson.Doc(ListMap())))),
                ExprOp.DocField(BsonField.Name("right")))))),
              IgnoreId),
            $unwind(ExprOp.DocField(BsonField.Name("left"))),
            $unwind(ExprOp.DocField(BsonField.Name("right"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("name") -> -\/(ExprOp.DocField(BsonField.Name("left") \ BsonField.Name("name"))),
              BsonField.Name("address") -> -\/(ExprOp.DocField(BsonField.Name("right") \ BsonField.Name("address"))))),
              IgnoreId))))  // Note: becomes ExcludeId in conversion to WorkflowTask
    }
 
    "plan 3-way equi-join" in {
      plan(
        "select foo.name, bar.address " +
          "from foo join bar on foo.id = bar.foo_id " +
          "join baz on bar.id = baz.bar_id") must
      beWorkflow(
        joinStructure(
          joinStructure(
            $read(Collection("foo")),
            $read(Collection("bar")),
            ExprOp.DocField(BsonField.Name("id")),
            Select(Ident("value"), "foo_id"),
            chain(_,
              $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
                BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
                BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
              $unwind(ExprOp.DocField(BsonField.Name("left"))),
              $unwind(ExprOp.DocField(BsonField.Name("right"))))),
          $read(Collection("baz")),
          ExprOp.DocField(BsonField.Name("right") \ BsonField.Name("id")),
          Select(Ident("value"), "bar_id"),
          chain(_,
            $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
              BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
              BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
            $unwind(ExprOp.DocField(BsonField.Name("left"))),
            $unwind(ExprOp.DocField(BsonField.Name("right"))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("name") -> -\/(ExprOp.DocField(BsonField.Name("left") \ BsonField.Name("left") \ BsonField.Name("name"))),
              BsonField.Name("address") -> -\/(ExprOp.DocField(BsonField.Name("left") \ BsonField.Name("right") \ BsonField.Name("address"))))),
              IgnoreId))))  // Note: becomes ExcludeId in conversion to WorkflowTask
    }

    "plan simple cross" in {
      import Js._
      plan("select zips2.city from zips, zips2 where zips._id = zips2._id") must
      beWorkflow(
        joinStructure(
          $read(Collection("zips")),
          $read(Collection("zips2")),
          ExprOp.Literal(Bson.Null),
          Null,
          chain(_,
            $match(Selector.Doc(ListMap[BsonField, Selector.SelectorExpr](
              BsonField.Name("left") -> Selector.NotExpr(Selector.Size(0)),
              BsonField.Name("right") -> Selector.NotExpr(Selector.Size(0))))),
            $unwind(ExprOp.DocField(BsonField.Name("left"))),
            $unwind(ExprOp.DocField(BsonField.Name("right"))),
            $match(Selector.Where(BinOp("==",
              Select(Select(Ident("this"), "left"), "_id"),
              Select(Select(Ident("this"), "right"), "_id")))),
            $project(Reshape.Doc(ListMap(
              BsonField.Name("city") -> -\/(ExprOp.DocField(BsonField.Name("right") \ BsonField.Name("city"))))),
              IgnoreId))))  // Note: becomes ExcludeId in conversion to WorkflowTask
    }
  }

/*
  "plan from LogicalPlan" should {
    "plan simple OrderBy" in {
      val lp = LogicalPlan.Let(
                  'tmp0, read("foo"),
                  LogicalPlan.Let(
                    'tmp1, makeObj("bar" -> ObjectProject(Free('tmp0), Constant(Data.Str("bar")))),
                    LogicalPlan.Let('tmp2, 
                      set.OrderBy(
                        Free('tmp1),
                        MakeArrayN(
                          makeObj(
                            "key" -> ObjectProject(Free('tmp1), Constant(Data.Str("bar"))),
                            "order" -> Constant(Data.Str("ASC"))
                          )
                        )
                      ),
                      Free('tmp2)
                    )
                  )
                )

      val exp = chain(
        $read(Collection("foo")),
        $project(Reshape.Doc(ListMap(
          BsonField.Name("lEft") -> \/- (Reshape.Doc(ListMap(
            BsonField.Name("bar") -> -\/ (ExprOp.DocField(BsonField.Name("bar")))))), 
          BsonField.Name("rIght") -> \/- (Reshape.Arr(ListMap(
            BsonField.Index(0) -> \/- (Reshape.Doc(ListMap(
              BsonField.Name("key") -> -\/ (ExprOp.DocField(
                BsonField.Name("bar"))), 
              BsonField.Name("order") -> -\/ (ExprOp.Literal(Bson.Text("ASC")))))))))))), 
        $sort(NonEmptyList(BsonField.Name("rIght") \ BsonField.Index(0) \ BsonField.Name("key") -> Ascending)), 
        $project(Reshape.Doc(ListMap(BsonField.Name("bar") -> -\/(ExprOp.DocField(BsonField.Name("lEft") \ BsonField.Name("bar")))))))

      plan(lp) must beWorkflow(exp)
    }

    "plan OrderBy with expression" in {
      val lp = LogicalPlan.Let('tmp0, 
                  read("foo"),
                  set.OrderBy(
                    Free('tmp0),
                    MakeArrayN(
                      makeObj(
                        "key" -> math.Divide(
                                  ObjectProject(Free('tmp0), Constant(Data.Str("bar"))),
                                  Constant(Data.Dec(10.0))),
                        "order" -> Constant(Data.Str("ASC"))
                      )
                    )
                  )
                )

      val exp = chain(
                  $read(Collection("foo")),
                  $project(Reshape.Doc(ListMap(
                    BsonField.Name("__sd_tmp_1") ->  \/- (Reshape.Arr(ListMap(
                      BsonField.Index(0) -> -\/ (ExprOp.Divide(
                                                          DocField(BsonField.Name("bar")), 
                                                          Literal(Bson.Dec(10.0)))))))))),
                  $sort(NonEmptyList(BsonField.Name("__sd_tmp_1") \ BsonField.Index(0) -> Ascending))
                  // We'll want another Project here to remove the temporary field
                  )

      plan(lp) must beWorkflow(exp)
    }.pendingUntilFixed

    "plan OrderBy with expression and earlier pipeline op" in {
      val lp = LogicalPlan.Let('tmp0,
                  read("foo"),
                  LogicalPlan.Let('tmp1,
                    set.Filter(
                      Free('tmp0),
                      relations.Eq(
                        ObjectProject(Free('tmp0), Constant(Data.Str("baz"))),
                        Constant(Data.Int(0))
                      )
                    ),
                    set.OrderBy(
                      Free('tmp1),
                      MakeArrayN(
                        makeObj(
                          "key" -> ObjectProject(Free('tmp1), Constant(Data.Str("bar"))),
                          "order" -> Constant(Data.Str("ASC"))
                        )
                      )
                    )
                  )
                )

      val exp = chain(
                  $read(Collection("foo")),
                  $match(Selector.Doc(
                    BsonField.Name("baz") -> Selector.Eq(Bson.Int64(0)))),
                  $sort(NonEmptyList(BsonField.Name("bar") -> Ascending)))

      plan(lp) must beWorkflow(exp)
    }.pendingUntilFixed

    "plan OrderBy with expression (and extra project)" in {
      val lp = LogicalPlan.Let('tmp0, 
                  read("foo"),
                  LogicalPlan.Let('tmp9,
                    makeObj(
                      "bar" -> ObjectProject(Free('tmp0), Constant(Data.Str("bar")))
                    ),
                    set.OrderBy(
                      Free('tmp9),
                      MakeArrayN(
                        makeObj(
                          "key" -> math.Divide(
                                    ObjectProject(Free('tmp9), Constant(Data.Str("bar"))),
                                    Constant(Data.Dec(10.0))),
                          "order" -> Constant(Data.Str("ASC"))
                        )
                      )
                    )
                  )
                )

      val exp = chain(
        $read(Collection("foo")),
        $project(Reshape.Doc(ListMap(BsonField.Name("lEft") -> \/-(Reshape.Doc(ListMap(BsonField.Name("bar") -> -\/(ExprOp.DocField(BsonField.Name("bar")))))), BsonField.Name("rIght") -> \/-(Reshape.Arr(ListMap(BsonField.Index(0) -> \/-(Reshape.Doc(ListMap(BsonField.Name("key") -> -\/(ExprOp.Divide(ExprOp.DocField(BsonField.Name("bar")), ExprOp.Literal(Bson.Dec(10.0)))), BsonField.Name("order") -> -\/(ExprOp.Literal(Bson.Text("ASC")))))))))))), 
        $sort(NonEmptyList(BsonField.Name("rIght") \ BsonField.Index(0) \ BsonField.Name("key") -> Ascending)), 
        $project(Reshape.Doc(ListMap(BsonField.Name("bar") -> -\/(ExprOp.DocField(BsonField.Name("lEft") \ BsonField.Name("bar")))))))

      plan(lp) must beWorkflow(exp)
    }
  }
  */
}
