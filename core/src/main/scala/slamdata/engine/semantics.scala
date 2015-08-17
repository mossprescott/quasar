/*
 * Copyright 2014 - 2015 SlamData Inc.
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

package slamdata.engine

import slamdata.Predef._
import slamdata.{NonTerminal, RenderTree, RenderedTree, Terminal}
import slamdata.fp._
import slamdata.engine.analysis._
import slamdata.engine.std.Library

import scala.AnyRef

import scalaz.{Tree => _, Node => _, _}, Scalaz._

sealed trait SemanticError {
  def message: String
}

object SemanticError {
  import slamdata.engine.sql._

  implicit val SemanticErrorShow = new Show[SemanticError] {
    override def show(value: SemanticError) = Cord(value.message)
  }

  final case class GenericError(message: String) extends SemanticError

  final case class DomainError(data: Data, hint: Option[String]) extends SemanticError {
    def message = "The data '" + data + "' did not fall within its expected domain" + hint.map(": " + _)
  }

  final case class FunctionNotFound(name: String) extends SemanticError {
    def message = "The function '" + name + "' could not be found in the standard library"
  }
  final case class FunctionNotBound(node: Node) extends SemanticError {
    def message = "A function was not bound to the node " + node
  }
  final case class TypeError(expected: Type, actual: Type, hint: Option[String]) extends SemanticError {
    def message = "Expected type " + expected + " but found " + actual + hint.map(": " + _).getOrElse("")
  }
  final case class VariableParseError(vari: VarName, value: VarValue, cause: slamdata.engine.sql.ParsingError) extends SemanticError {
    def message = "The variable " + vari + " should contain a SQL expression but was `" + value.value + "` (" + cause.message + ")"
  }
  final case class DuplicateRelationName(defined: String, duplicated: SqlRelation) extends SemanticError {
    private def nameOf(r: SqlRelation) = r match {
      case TableRelationAST(name, aliasOpt) => aliasOpt.getOrElse(name)
      case ExprRelationAST(_, alias)        => alias
      case JoinRelation(_, _, _, _)         => "unknown"
      case CrossRelation(_, _)              => "unknown"
    }

    def message = "Found relation with duplicate name '" + defined + "': " + defined
  }
  final case class NoTableDefined(node: Node) extends SemanticError {
    def message = "No table was defined in the scope of \'" + node.sql + "\'"
  }
  final case class MissingField(name: String) extends SemanticError {
    def message = "No field named '" + name + "' exists"
  }
  final case class MissingIndex(index: Int) extends SemanticError {
    def message = "No element exists at array index '" + index
  }
  final case class WrongArgumentCount(func: Func, expected: Int, actual: Int) extends SemanticError {
    def message = "Wrong number of arguments for function '" + func.name + "': expected " + expected + " but found " + actual
  }
  final case class NonCompilableNode(node: Node) extends SemanticError {
    def message = "The node " + node + " cannot be compiled"
  }
  final case class ExpectedLiteral(node: Node) extends SemanticError {
    def message = "Expected literal but found '" + node.sql + "'"
  }
  final case class AmbiguousReference(node: Node, relations: List[SqlRelation]) extends SemanticError {
    def message = "The expression '" + node.sql + "' is ambiguous and might refer to any of the tables " + relations.mkString(", ")
  }
  final case object CompiledTableMissing extends SemanticError {
    def message = "Expected the root table to be compiled but found nothing"
  }
  final case class CompiledSubtableMissing(name: String) extends SemanticError {
    def message = "Expected to find a compiled subtable with name \"" + name + "\""
  }
  final case class DateFormatError(func: Func, str: String, hint: Option[String]) extends SemanticError {
    def message = "Date/time string could not be parsed as " + func.name + ": " + str + hint.map(" (" + _ + ")").getOrElse("")
  }
}

trait SemanticAnalysis {
  import slamdata.engine.sql._
  import SemanticError._

  type Failure = NonEmptyList[SemanticError]

  private def fail[A](e: SemanticError) = Validation.failure[NonEmptyList[SemanticError], A](NonEmptyList(e))
  private def succeed[A](s: A) = Validation.success[NonEmptyList[SemanticError], A](s)

  def tree(root: Node): AnnotatedTree[Node, Unit] = AnnotatedTree.unit(root, n => n.children)

  /**
   * This analyzer looks for function invocations (including operators),
   * and binds them to their associated function definitions in the
   * provided library. If a function definition cannot be found,
   * produces an error with details on the failure.
   */
  def FunctionBind[A](library: Library) = {
    def findFunction(name: String) = {
      val lcase = name.toLowerCase

      library.functions.find(f => f.name.toLowerCase == lcase).map(f => Validation.success(Some(f))).getOrElse(
        fail(FunctionNotFound(name))
      )
    }

    Analysis.annotate[Node, A, Option[Func], Failure] {
      case (InvokeFunction(name, args)) => findFunction(name)

      case (Unop(expr, op)) => findFunction(op.name)

      case (Binop(left, right, op)) => findFunction(op.name)

      case _ => Validation.success(None)
    }
  }

  sealed trait Synthetic
  object Synthetic {
    final case object SortKey extends Synthetic
  }

  implicit val SyntheticRenderTree = RenderTree.fromToString[Synthetic]("Synthetic")

  /**
   * Inserts synthetic fields into the projections of each `select` stmt to hold
   * the values that will be used in sorting, and annotates each new projection
   * with Synthetic.SortKey. The compiler will generate a step to remove these
   * fields after the sort operation.
   */
  def TransformSelect[A]: Analysis[Node, A, Option[Synthetic], Failure] = {
    val prefix = "__sd__"

    def transform(node: Node): Node =
      node match {
        case sel @ Select(_, projections, _, _, _, Some(sql.OrderBy(keys)), _, _) => {
          def matches(key: Expr): PartialFunction[Proj, Expr] = key match {
            case Ident(keyName) => {
              case Proj(_, Some(alias))        if keyName == alias    => key
              case Proj(Ident(projName), None) if keyName == projName => key
              case Proj(Splice(_), _)                                 => key
            }
            case _ => {
              case Proj(expr2, Some(alias)) if key == expr2 => Ident(alias)
            }
          }

          // Note: order of the keys has to be preserved, so this complex fold seems
          // to be the best way.
          type Target = (List[Proj], List[(Expr, OrderType)], Int)

          val (projs2, keys2, _) = keys.foldRight[Target]((Nil, Nil, 0)) {
            case ((expr, orderType), (projs, keys, index)) =>
              projections.collectFirst(matches(expr)).fold {
                val name  = prefix + index.toString()
                val proj2 = Proj(expr, Some(name))
                val key2  = Ident(name) -> orderType
                (proj2 :: projs, key2 :: keys, index + 1)
              } (
                kExpr => (projs, (kExpr, orderType) :: keys, index))
          }

          sel.copy(projections = projections ++ projs2,
                   orderBy     = Some(sql.OrderBy(keys2)))
        }

        case _ => node
      }

    val ann = Analysis.annotate[Node, Unit, Option[Synthetic], Failure] { node =>
      node match {
        case Proj(_, Some(name)) if name.startsWith(prefix) =>
          Some(Synthetic.SortKey).success
        case _ => None.success
      }
    }

    tree1 => ann(tree(transform(tree1.root)))
  }

  case class TableScope(scope: Map[String, SqlRelation])

  implicit val ShowTableScope = new Show[TableScope] {
    override def show(v: TableScope) = Show[Map[String, Node]].show(v.scope)
  }

  /**
   * This analysis identifies all the named tables within scope at each node in
   * the tree. If two tables are given the same name within the same scope, then
   * because this leads to an ambiguity, an error is produced containing details
   * on the duplicate name.
   */
  def ScopeTables[A] = Analysis.readTree[Node, A, TableScope, Failure] { tree =>
    import Validation.{success, failure}

    Analysis.fork[Node, A, TableScope, Failure]((scopeOf, node) => {
      def parentScope(node: Node) = tree.parent(node).map(scopeOf).getOrElse(TableScope(Map()))

      node match {
        case Select(_, projections, relations, filter, groupBy, orderBy, limit, offset) =>
          val parentMap = parentScope(node).scope

          (relations.foldLeft[Validation[Failure, Map[String, SqlRelation]]](success(Map.empty[String, SqlRelation])) {
            case (v, relation) =>
              implicit val sg = Semigroup.firstSemigroup[SqlRelation]

              v +++ tree.subtree(relation).foldDown[Validation[Failure, Map[String, SqlRelation]]](success(Map.empty[String, SqlRelation])) {
                case (v, relation : SqlRelation) =>
                  v.fold(
                    failure,
                    acc => {
                      val name = relation match {
                        case TableRelationAST(name, aliasOpt) => Some(aliasOpt.getOrElse(name))
                        case ExprRelationAST(_, alias)        => Some(alias)
                        case JoinRelation(_, _, _, _)         => None
                        case CrossRelation(_, _)              => None
                      }

                      (name.map { name =>
                        (acc.get(name).map{ relation2 =>
                          fail(DuplicateRelationName(name, relation2))
                        }).getOrElse(success(acc + (name -> relation)))
                      }).getOrElse(success(acc))
                    }
                  )

                case (v, _) => v // We're only interested in relations
              }
          }).map(map => TableScope(parentMap ++ map))

        case _ => success(parentScope(node))
      }
    })
  }

  sealed trait Provenance {
    import Provenance._

    def & (that: Provenance): Provenance = Both(this, that)

    def | (that: Provenance): Provenance = Either(this, that)

    def simplify: Provenance = this match {
      case x : Either => anyOf(x.flatten.map(_.simplify).filterNot(_ == Empty))
      case x : Both => allOf(x.flatten.map(_.simplify).filterNot(_ == Empty))
      case _ => this
    }

    def namedRelations: Map[String, List[NamedRelation]] = Foldable[List].foldMap(relations)(_.namedRelations)

    def relations: List[SqlRelation] = this match {
      case Empty => Nil
      case Value => Nil
      case Relation(value) => value :: Nil
      case Either(v1, v2) => v1.relations ++ v2.relations
      case Both(v1, v2) => v1.relations ++ v2.relations
    }

    def flatten: Set[Provenance] = Set(this)

    override def equals(that: scala.Any): Boolean = (this, that) match {
      case (x, y) if (x.eq(y.asInstanceOf[AnyRef])) => true
      case (Relation(v1), Relation(v2)) => v1 == v2
      case (Either(_, _), that @ Either(_, _)) => this.simplify.flatten == that.simplify.flatten
      case (Both(_, _), that @ Both(_, _)) => this.simplify.flatten == that.simplify.flatten
      case (_, _) => false
    }

    override def hashCode = this match {
      case Either(_, _) => this.simplify.flatten.hashCode
      case Both(_, _) => this.simplify.flatten.hashCode
      case _ => super.hashCode
    }
  }
  trait ProvenanceInstances {
    implicit val ProvenanceRenderTree = new RenderTree[Provenance] { self =>
      import Provenance._

      def render(v: Provenance) = {
        val ProvenanceNodeType = List("Provenance")

        def nest(l: RenderedTree, r: RenderedTree, sep: String) = (l, r) match {
          case (RenderedTree(_, ll, Nil), RenderedTree(_, rl, Nil)) =>
                    Terminal(ProvenanceNodeType, Some("(" + ll + " " + sep + " " + rl + ")"))
          case _ => NonTerminal(ProvenanceNodeType, Some(sep), l :: r :: Nil)
        }

        v match {
          case Empty               => Terminal(ProvenanceNodeType, Some("Empty"))
          case Value               => Terminal(ProvenanceNodeType, Some("Value"))
          case Relation(value)     => RenderTree[Node].render(value).copy(nodeType = ProvenanceNodeType)
          case Either(left, right) => nest(self.render(left), self.render(right), "|")
          case Both(left, right)   => nest(self.render(left), self.render(right), "&")
        }
      }
    }

    implicit val ProvenanceOrMonoid = new Monoid[Provenance] {
      import Provenance._

      def zero = Empty

      def append(v1: Provenance, v2: => Provenance) = (v1, v2) match {
        case (Empty, that) => that
        case (this0, Empty) => this0
        case _ => v1 | v2
      }
    }

    implicit val ProvenanceAndMonoid = new Monoid[Provenance] {
      import Provenance._

      def zero = Empty

      def append(v1: Provenance, v2: => Provenance) = (v1, v2) match {
        case (Empty, that) => that
        case (this0, Empty) => this0
        case _ => v1 & v2
      }
    }
  }
  object Provenance extends ProvenanceInstances {
    case object Empty extends Provenance
    case object Value extends Provenance
    case class Relation(value: SqlRelation) extends Provenance
    case class Either(left: Provenance, right: Provenance) extends Provenance {
      override def flatten: Set[Provenance] = {
        def flatten0(x: Provenance): Set[Provenance] = x match {
          case Either(left, right) => flatten0(left) ++ flatten0(right)
          case _ => Set(x)
        }
        flatten0(this)
      }
    }
    case class Both(left: Provenance, right: Provenance) extends Provenance {
      override def flatten: Set[Provenance] = {
        def flatten0(x: Provenance): Set[Provenance] = x match {
          case Both(left, right) => flatten0(left) ++ flatten0(right)
          case _ => Set(x)
        }
        flatten0(this)
      }
    }

    def allOf(xs: Iterable[Provenance]): Provenance = {
      import scalaz.std.iterable._

      xs.concatenate(ProvenanceAndMonoid)
    }

    def anyOf(xs: Iterable[Provenance]): Provenance = {
      import scalaz.std.iterable._

      xs.concatenate(ProvenanceOrMonoid)
    }
  }

  /**
   * This phase infers the provenance of every expression, issuing errors
   * if identifiers are used with unknown provenance. The phase requires
   * TableScope annotations on the tree.
   */
  def ProvenanceInfer = Analysis.readTree[Node, TableScope, Provenance, Failure] { tree =>
    Analysis.join[Node, TableScope, Provenance, Failure]((provOf, node) => {
      import Validation.{success, failure}

      def propagate(child: Node) = success(provOf(child))

      def NA: Validation[Nothing, Provenance] = success(Provenance.Empty)

      (node match {
        case Select(_, projections, relations, filter, groupBy, orderBy, limit, offset) =>
          success(Provenance.allOf(projections.map(provOf)))

        case Proj(expr, _)      => propagate(expr)
        case SetLiteral(exprs)  => success(Provenance.Value)
        case ArrayLiteral(exprs) => success(Provenance.Value)
          // FIXME: NA case
        case Splice(expr)       => expr.fold(NA)(x => success(provOf(x)))
        case v @ Vari(_)        => success(Provenance.Value)
        case Binop(left, right, op) =>
          success(provOf(left) & provOf(right))
        case Unop(expr, op) => success(provOf(expr))
        case ident @ Ident(name) =>
          val tableScope = tree.attr(node).scope

          (tableScope.get(name).map((Provenance.Relation.apply _) andThen success)).getOrElse {
            Provenance.anyOf(tableScope.values.map(Provenance.Relation.apply)) match {
              case Provenance.Empty => fail(NoTableDefined(ident))

              case x => success(x)
            }
          }

        case InvokeFunction(name, args) =>
          success(Provenance.allOf(args.map(provOf)))
        case Case(cond, expr) => propagate(expr)
        case Match(expr, cases, default) =>
          success(cases.map(provOf).concatenate(Provenance.ProvenanceAndMonoid))
        case Switch(cases, default) => success(cases.map(provOf).concatenate(Provenance.ProvenanceAndMonoid))

        case IntLiteral(value) => success(Provenance.Value)

        case FloatLiteral(value) => success(Provenance.Value)

        case StringLiteral(value) => success(Provenance.Value)

        case BoolLiteral(value) => success(Provenance.Value)

        case NullLiteral => success(Provenance.Value)

        case r @ TableRelationAST(_, _) => success(Provenance.Relation(r))

        case r @ ExprRelationAST(_, _) => success(Provenance.Relation(r))

        case r @ JoinRelation(_, _, _, _) => success(Provenance.Relation(r))

        case r @ CrossRelation(_, _) => success(Provenance.Relation(r))

        case GroupBy(keys, having) => success(Provenance.allOf(keys.map(provOf)))

        case OrderBy(keys) => success(Provenance.allOf(keys.map(_._1).toList.map(provOf)))

        case _ : BinaryOperator => NA

        case _ : UnaryOperator => NA
      }).map(_.simplify)
    })
  }

  sealed trait InferredType
  object InferredType {
    case class Specific(value: Type) extends InferredType
    case object Unknown extends InferredType

    implicit val ShowInferredType = new Show[InferredType] {
      override def show(v: InferredType) = v match {
        case Unknown => Cord("?")
        case Specific(v) => Show[Type].show(v)
      }
    }
  }

  /**
   * This phase works top-down to push out known types to terms with unknowable
   * types (such as columns and wildcards). The annotation is the type of the node,
   * which defaults to Type.Top in cases where it is not known.
   */
  def TypeInfer = {
    Analysis.readTree[Node, Option[Func], Map[Node, Type], Failure] { tree =>
      import Validation.{success}

      Analysis.fork[Node, Option[Func], Map[Node, Type], Failure]((mapOf, node) => {
        /**
         * Retrieves the inferred type of the current node being annotated.
         */
        def inferredType = for {
          parent   <- tree.parent(node)
          selfType <- mapOf(parent).get(node)
        } yield selfType

        /**
         * Propagates the inferred type of this node to its sole child node.
         */
        def propagate(child: Node) = propagateAll(child :: Nil)

        /**
         * Propagates the inferred type of this node to its identically-typed
         * children nodes.
         */
        def propagateAll(children: Seq[Node]) = success(inferredType.map(t => Map(children.map(_ -> t): _*)).getOrElse(Map()))

        def annotateFunction(args: List[Node]) =
          (tree.attr(node).map { func =>
            val typesV = inferredType.map(func.untype).getOrElse(success(func.domain))
            typesV map (types => (args zip types).toMap)
          }).getOrElse(fail(FunctionNotBound(node)))

        /**
         * Indicates no information content for the children of this node.
         */
        def NA = success(Map.empty[Node, Type])

        node match {
          case Select(_, projections, relations, filter, groupBy, orderBy, limit, offset) =>
            inferredType match {
              // TODO: If there's enough type information in the inferred type to do so, push it
              //       down to the projections.

              case _ => NA
            }

          case Proj(expr, _)     => propagate(expr)
          case SetLiteral(exprs) =>
            inferredType match {
              // Push the set type down to the children:
              case Some(Type.Set(tpe)) => success(exprs.map(_ -> tpe).toMap)

              case _ => NA
            }
          case ArrayLiteral(exprs) =>
            inferredType match {
              // Push the array type down to the children:
              case Some(Type.FlexArr(_, _, tpe)) => success(exprs.map(_ -> tpe).toMap)

              case _ => NA
            }
          case Splice(expr) => expr.fold(NA)(propagate(_))
          case v @ Vari(_) => NA
          case Binop(left, right, _) => annotateFunction(left :: right :: Nil)
          case Unop(expr, _) => annotateFunction(expr :: Nil)
          case Ident(_) => NA
          case InvokeFunction(_, args) => annotateFunction(args)
          case Case(_, expr) => propagate(expr)
          case Match(_, cases, default) => propagateAll(cases ++ default)
          case Switch(cases, default) => propagateAll(cases ++ default)
          case IntLiteral(_) => NA
          case FloatLiteral(_) => NA
          case StringLiteral(_) => NA
          case BoolLiteral(_) => NA
          case NullLiteral => NA
          case TableRelationAST(_, _) => NA
          case ExprRelationAST(expr, _) => propagate(expr)
          case JoinRelation(_, _, _, _) => NA
          case CrossRelation(_, _) => NA
          case GroupBy(_, _) => NA
          case OrderBy(_) => NA
          case _: BinaryOperator => NA
          case _: UnaryOperator  => NA
        }
      })
    } >>> Analysis.readTree[Node, Map[Node, Type], InferredType, Failure] { tree =>
      Analysis.fork[Node, Map[Node, Type], InferredType, Failure]((typeOf, node) => {
        // Read the inferred type of this node from the parent node's attribute:
        succeed((for {
          parent   <- tree.parent(node)
          selfType <- tree.attr(parent).get(node)
        } yield selfType).map(InferredType.Specific.apply).getOrElse(InferredType.Unknown))
      })
    }
  }

  /**
   * This phase works bottom-up to check the type of all expressions.
   * In the event of a type error, an error will be produced containing
   * details on the expected versus actual type.
   */
  def TypeCheck = {
    Analysis.readTree[Node, (Option[Func], InferredType), Type, Failure] { tree =>
      Analysis.join[Node, (Option[Func], InferredType), Type, Failure]((typeOf, node) => {
        def func(node: Node): ValidationNel[SemanticError, Func] = {
          tree.attr(node)._1.map(Validation.success).getOrElse(fail(FunctionNotBound(node)))
        }

        def inferType(default: Type): ValidationNel[SemanticError, Type] = succeed(tree.attr(node)._2 match {
          case InferredType.Unknown => default
          case InferredType.Specific(v) => v
        })

        def typecheckArgs(func: Func, actual: List[Type]): ValidationNel[SemanticError, Unit] = {
          val expected = func.domain

          if (expected.length != actual.length) {
            fail[Unit](WrongArgumentCount(func, expected.length, actual.length))
          } else {
            (expected.zip(actual).map {
              case (expected, actual) => Type.typecheck(expected, actual)
            }).sequenceU.map(κ(()))
          }
        }

        def typecheckFunc(args: List[Expr]) = {
          func(node).fold(
            Validation.failure,
            func => {
              val argTypes = args.map(typeOf)

              typecheckArgs(func, argTypes).fold(
                Validation.failure,
                κ(func.apply(argTypes)))
            })
        }

        def NA = succeed(Type.Bottom)

        def propagate(n: Node) = succeed(typeOf(n))

        node match {
          case s @ Select(_, projections, relations, filter, groupBy, orderBy, limit, offset) =>
            succeed(Type.Obj(s.namedProjections(None).map(t => (t._1, typeOf(t._2))).toMap, None))

          case Proj(expr, _)     => propagate(expr)
          case SetLiteral(exprs) => succeed(Type.Arr(exprs.map(typeOf)))  // FIXME: should be Type.Set(...)
          case ArrayLiteral(exprs) => succeed(Type.Arr(exprs.map(typeOf)))
          case Splice(_) => inferType(Type.Top)
          case v @ Vari(_) => inferType(Type.Top)
          case Binop(left, right, op) => typecheckFunc(left :: right :: Nil)
          case Unop(expr, op) => typecheckFunc(expr :: Nil)
          case Ident(name) => inferType(Type.Top)
          case InvokeFunction(name, args) => typecheckFunc(args.toList)
          case Case(cond, expr) => succeed(typeOf(expr))
          case Match(expr, cases, default) =>
            succeed((cases ++ default).map(typeOf).foldLeft[Type](Type.Top)(_ | _).lub)
          case Switch(cases, default) =>
            succeed((cases ++ default).map(typeOf).foldLeft[Type](Type.Top)(_ | _).lub)
          case IntLiteral(value) => succeed(Type.Const(Data.Int(value)))
          case FloatLiteral(value) => succeed(Type.Const(Data.Dec(value)))
          case StringLiteral(value) => succeed(Type.Const(Data.Str(value)))
          case BoolLiteral(value) => succeed(Type.Const(Data.Bool(value)))
          case NullLiteral => succeed(Type.Const(Data.Null))
          case TableRelationAST(_, _) => NA
          case ExprRelationAST(expr, _) => propagate(expr)
          case JoinRelation(_, _, _, _) => succeed(Type.Bool)
          case CrossRelation(left, right) => succeed(typeOf(left) & typeOf(right))
          case GroupBy(keys, having) =>
            // Not necessary but might be useful:
            succeed(Type.Arr(keys.map(typeOf)))
          case OrderBy(keys) => NA
          case _: BinaryOperator => NA
          case _: UnaryOperator  => NA
        }
      })
    }
  }

  type Annotations = (((Option[Synthetic], Provenance), Option[Func]), Type)

  val AllPhases: Analysis[Node, Unit, Annotations, Failure] =
    (TransformSelect[Unit].push(()) >>>
      ScopeTables.second >>>
      ProvenanceInfer.second).push(()) >>>
    FunctionBind[Unit](std.StdLib).second.dup2 >>>
    TypeInfer.second >>>
    TypeCheck.pop2
}
object SemanticAnalysis extends SemanticAnalysis
