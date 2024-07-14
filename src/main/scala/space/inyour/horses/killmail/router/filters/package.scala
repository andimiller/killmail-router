package space.inyour.horses.killmail.router

import cats.{Eq, Eval}
import io.circe.{ACursor, Json, JsonNumber}
import spire.algebra.Bool

import scala.annotation.tailrec

package object filters:
  enum PathOperation:
    case DownField(name: String)
    case DownIndex(idx: Int)

  enum Expr:
    // pure
    case Pure(result: Boolean)
    // comparisons
    case Equals(path: List[PathOperation], value: Json)
    case GreaterThan(path: List[PathOperation], value: JsonNumber)
    case LessThan(path: List[PathOperation], value: JsonNumber)
    case ContainsSubstring(path: List[PathOperation], value: String)
    // boolean algebra
    case Not(expr: Expr)
    case And(left: Expr, right: Expr)
    case Or(left: Expr, right: Expr)

  given Bool[Expr] with
    def complement(a: Expr): Expr   = Expr.Not(a)
    def and(a: Expr, b: Expr): Expr = Expr.And(a, b)
    def or(a: Expr, b: Expr): Expr  = Expr.Or(a, b)
    def zero: Expr                  = Expr.Pure(false)
    def one: Expr                   = Expr.Pure(true)

  // base equals just uses universal equals
  given Eq[Expr] = Eq.fromUniversalEquals

  inline def evaluatePath(path: List[PathOperation])(input: Json): Option[Json] =
    path
      .foldLeft(input.hcursor.asInstanceOf[ACursor]) { (cursor, op) =>
        op match
          case PathOperation.DownField(name) => cursor.downField(name)
          case PathOperation.DownIndex(idx)  => cursor.downN(idx)
      }
      .focus

  // runner
  def run(expr: Expr)(input: Json): Eval[Boolean] = expr match
    case Expr.Pure(result)                   => Eval.now(result)
    case Expr.Equals(path, value)            =>
      Eval.now {
        evaluatePath(path)(input).fold(false)(_ == value)
      }
    case Expr.GreaterThan(path, value)       =>
      Eval.now {
        evaluatePath(path)(input).flatMap(_.asNumber).fold(false)(_.toDouble > value.toDouble)
      }
    case Expr.LessThan(path, value)          =>
      Eval.now {
        evaluatePath(path)(input).flatMap(_.asNumber).fold(false)(_.toDouble < value.toDouble)
      }
    case Expr.ContainsSubstring(path, value) =>
      Eval.now {
        evaluatePath(path)(input).flatMap(_.asString).fold(false)(_.contains(value))
      }
    case Expr.Not(expr)                      =>
      run(expr)(input).map(!_)
    case Expr.And(left, right)               =>
      for
        l <- run(left)(input)
        r <- run(right)(input)
      yield l && r
    case Expr.Or(left, right)                =>
      for
        l <- run(left)(input)
        r <- run(right)(input)
      yield l || r
