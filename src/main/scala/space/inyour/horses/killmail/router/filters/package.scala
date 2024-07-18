package space.inyour.horses.killmail.router

import skunk.Codec as SkunkCodec
import cats.Show
import cats.effect.kernel.Sync
import cats.implicits.*
import cats.parse.*
import cats.parse.strings.Json.delimited as jsonStringCodec
import io.circe.jawn.decode as jawnDecode
import io.circe.{Decoder, Encoder}
import cats.{Eq, Eval}
import io.circe.{ACursor, Json, JsonNumber}
import spire.algebra.Bool
import net.andimiller.cats.parse.interpolator.*
import space.inyour.horses.killmail.router.filters.PathOperation.MapArrayPath
import scala.collection.immutable.ListMap

package object filters:

  opaque type PrettyExpr = Expr
  extension (p: PrettyExpr) def expr: Expr = p

  object PrettyExpr:
    private def indentString(spaces: Int)(s: String) = s.linesIterator.map(line => (" " * spaces) + line).mkString("\n")

    trait IndentedShow[T] extends Show[T] {
      def show(indent: Int, t: T): String

      def show(t: T): String = show(0, t)
    }

    import Expr.given_Show_JsonNumber
    import Expr.showListPathOperation

    given IndentedShow[PrettyExpr] with

      def showBindings(bs: ListMap[String, Expr]): String = {
        val bindings = bs.toList
          .map {
            case (name, e) if e.isSimple =>
              show"($name $e)"
            case (name, e)               =>
              show"""($name
                |${show(2, e)}
                |)
                |""".stripMargin
          }
          .map(indentString(4))
          .mkString("\n")

        show"""  [
              |$bindings
              |  ]""".stripMargin
      }

      def show(indent: Int, t: Expr): String = indentString(indent)(t match
        case Expr.Pure(result)                         => result.toString
        case Expr.Reference(result)                    => result
        case a @ Expr.Apply(path, expr) if a.isSimple  => show"(apply $path $expr)"
        case Expr.Apply(path, expr)                    =>
          show"""(apply
                |  $path
                |${show(2, expr)}
                |)
                |""".stripMargin
        case e @ Expr.Exists(path, expr) if e.isSimple => show"(exists $path $expr)"
        case Expr.Exists(path, expr)                   => show"""(exists
                                                               |  $path
                                                               |${show(2, expr)}
                                                               |)
                                                               |""".stripMargin
        case n @ Expr.Not(expr) if n.isSimple          => show"(not $expr)"
        case Expr.Not(expr)                            => show"""(not
                                                                |${show(2, expr)}
                                                                |)
                                                                |""".stripMargin
        case Expr.Equals(path, value)                  => show"(== $path $value)"
        case Expr.GreaterThan(path, value)             => show"(> $path $value)"
        case Expr.LessThan(path, value)                => show"(< $path $value)"
        case Expr.Contains(path, value)                => show"(contains $path ${value.noSpaces})"
        case Expr.And(left, right)                     => show"""(and
                                            |${show(2, left)}
                                            |${show(2, right)}
                                            |)
                                            |""".stripMargin
        case Expr.Or(left, right)                      => show"""(or
                                           |${show(2, left)}
                                           |${show(2, right)}
                                           |)
                                           |""".stripMargin

        case Expr.Let(bindings, expr) => show"""(let
                                                    |${showBindings(bindings)}
                                                    |${show(2, expr)}
                                                    |)
                                                    |""".stripMargin
      )

    given Encoder[PrettyExpr] = Encoder[String].contramap(_.show)
    given Decoder[PrettyExpr] = Decoder[String].emap(Expr.codec.parser.parseAll(_).leftMap(_.show))

  enum PathOperation:
    case DownField(name: String)
    case DownIndex(idx: Int)
    case MapArrayPath(subpath: List[PathOperation])

  enum Expr:
    def pretty: PrettyExpr = this
    def isSimple: Boolean  = this match // checks if this contains things complex enough to split lines
      case Apply(_, e)             => e.isSimple
      case Not(e)                  => e.isSimple
      case Exists(_, e)            => e.isSimple
      case _: Let | _: And | _: Or => false // these are always multiline
      case _                       => true

    // pure
    case Pure(result: Boolean)
    // comparisons
    case Apply(path: List[PathOperation], expr: Expr)
    case Equals(path: List[PathOperation], value: Json)
    case GreaterThan(path: List[PathOperation], value: JsonNumber)
    case LessThan(path: List[PathOperation], value: JsonNumber)
    case Contains(path: List[PathOperation], value: Json)
    case Exists(
        path: List[PathOperation],
        expr: Expr
    ) // expects the path to point at an array, checks if any items in that array have this expression evaluate to true
    // boolean algebra
    case Not(expr: Expr)
    case And(left: Expr, right: Expr)
    case Or(left: Expr, right: Expr)
    // language constructs
    case Let(bindings: ListMap[String, Expr], main: Expr)
    case Reference(name: String)

  object Expr:
    given Bool[Expr] with
      def complement(a: Expr): Expr   = Expr.Not(a)
      def and(a: Expr, b: Expr): Expr = Expr.And(a, b)
      def or(a: Expr, b: Expr): Expr  = Expr.Or(a, b)
      def zero: Expr                  = Expr.Pure(false)
      def one: Expr                   = Expr.Pure(true)

    def parse[F[_]: Sync](s: String): F[Expr] = Sync[F].fromEither(
      codec.parser.parseAll(s).leftMap(e => new RuntimeException(e.show))
    )

    // base parser and show
    given Show[Json]                                                   = Show.show(_.noSpacesSortKeys)
    given Show[JsonNumber]                                             = Show.show(n => Json.fromJsonNumber(n).noSpacesSortKeys)
    given jsonParser: Parser[Json]                                     = Parser.charsWhile(_ != ')').mapFilter(s => jawnDecode[Json](s).toOption)
    lazy implicit val showPathOperation: Show[PathOperation]           = Show.show {
      case PathOperation.DownField(name)       => show".$name"
      case PathOperation.DownIndex(idx)        => show"[$idx]"
      case PathOperation.MapArrayPath(subpath) => show"|{$subpath}"
    }
    lazy implicit val showListPathOperation: Show[List[PathOperation]] = Show.show(_.mkString_("root", "", ""))
    lazy implicit val pathParser: Parser[List[PathOperation]]          = Parser.defer {
      Parser.recursive { recurse =>
        val field   = (p"." *> Parser.charsWhile0(c => !" .[|{}\r\n\t".toSet.contains(c))).map(PathOperation.DownField.apply)
        val idx     = p"[${Numbers.digits}]".map(_.toInt).map(PathOperation.DownIndex.apply)
        val mapPath = p"|{$recurse}".map(PathOperation.MapArrayPath.apply)
        val list    = field.orElse(idx).orElse(mapPath).rep0
        p"root" *> list
      }
    }

    given Show[ListMap[String, Expr]] = Show.show { lm =>
      lm.toList
        .map { case (name, e) =>
          show"($name $e)"
        }
        .mkString("[", " ", "]")
    }

    given Show[Expr] with
      def show(t: Expr): String = t match
        case Expr.Pure(result)             => result.toString
        case Expr.Reference(name)          => name
        case Expr.Apply(path, expr)        => show"(apply $path $expr)"
        case Expr.Equals(path, value)      => show"(== $path $value)"
        case Expr.GreaterThan(path, value) => show"(> $path $value)"
        case Expr.LessThan(path, value)    => show"(< $path $value)"
        case Expr.Contains(path, value)    => show"(contains $path ${value.noSpaces})"
        case Expr.Not(expr)                => show"(not $expr)"
        case Expr.And(left, right)         => show"(and $left $right)"
        case Expr.Or(left, right)          => show"(or $left $right)"
        case Expr.Exists(path, expr)       => show"(exists $path $expr)"
        case Expr.Let(bindings, expr)      => show"(let $bindings $expr)"

    private inline def whitespace: Parser[Unit]   = Parser.charsWhile(_.isWhitespace).void
    private inline def whitespace0: Parser0[Unit] = Parser.charsWhile0(_.isWhitespace).void

    private inline def nameParser: Parser[String] = Parser.charsWhile(c => c.isLetterOrDigit || "-".contains(c))

    lazy implicit val given_Parser_Expr: Parser[Expr] = Parser.defer {
      Parser.recursive { recurse =>
        val int                             = Numbers.digits.map(JsonNumber.fromDecimalStringUnsafe)
        val binding: Parser[(String, Expr)] = pm"($nameParser$whitespace$recurse$whitespace0)".map { (n, _, e, _) => (n, e) }

        Parser.oneOf(
          List(
            p"true".as(Expr.Pure(true)).orElse(p"false".as(Expr.Pure(false))),
            nameParser.map(Expr.Reference.apply),
            p"(apply $pathParser $recurse)".map(Expr.Apply.apply),
            p"(== $pathParser $jsonParser)".map(Expr.Equals.apply),
            p"(> $pathParser $int)".map(Expr.GreaterThan.apply),
            p"(< $pathParser $int)".map(Expr.LessThan.apply),
            p"(contains $pathParser $jsonParser)".map(Expr.Contains.apply),
            pm"(not$whitespace$recurse$whitespace0)".map { (_, e, _) => Expr.Not(e) },
            for
              _ <- p"(and"
              _ <- whitespace
              l <- recurse
              _ <- whitespace
              r <- recurse
              _ <- whitespace0
              _ <- p")"
            yield Expr.And(l, r),
            for
              _ <- p"(or"
              _ <- whitespace
              l <- recurse
              _ <- whitespace
              r <- recurse
              _ <- whitespace0
              _ <- p")"
            yield Expr.Or(l, r),
            for
              _    <- p"(exists"
              _    <- whitespace
              path <- pathParser
              _    <- whitespace
              expr <- recurse
              _    <- whitespace0
              _    <- p")"
            yield Expr.Exists(path, expr),
            for
              _        <- p"(let"
              _        <- whitespace
              _        <- p"["
              _        <- whitespace0
              bindings <- binding.repSep(whitespace).map(bs => ListMap.from(bs.iterator))
              _        <- whitespace0
              _        <- p"]"
              _        <- whitespace
              expr     <- recurse
              _        <- whitespace0
              _        <- p")"
            yield Expr.Let(bindings, expr)
          )
        )
      }
    }
    given codec: StringCodec[Parser, Expr] with
      def parser: Parser[Expr]    = given_Parser_Expr
      def encode(e: Expr): String = e.show
    /// circe
    given Encoder[Expr]                               = Encoder[String].contramap(_.show)
    given Decoder[Expr]                               = Decoder[String].emap(codec.parser.parseAll(_).leftMap(_.show))

    /// skunk
    given SkunkCodec[Expr] = SkunkCodec.simple(
      codec.encode,
      codec.parser.parseAll(_).leftMap(_.show),
      skunk.data.Type.text
    )

    // base equals just uses universal equals
    given Eq[Expr] = Eq.fromUniversalEquals

    def evaluatePath(path: List[PathOperation])(input: Json): Option[Json] =
      path
        .foldLeft(input.hcursor.asInstanceOf[ACursor]) { (cursor, op) =>
          op match
            case PathOperation.DownField(name)       => cursor.downField(name)
            case PathOperation.DownIndex(idx)        => cursor.downN(idx)
            case PathOperation.MapArrayPath(subpath) =>
              cursor.withFocus { j =>
                j.mapArray { arr =>
                  arr.map { item =>
                    evaluatePath(subpath)(item).getOrElse(Json.Null)
                  }
                }
              }
        }
        .focus

    // runner
    def run(expr: Expr, bindings: ListMap[String, Expr] = ListMap.empty)(input: Json): Eval[Boolean] = expr match
      case Expr.Pure(result)             => Eval.now(result)
      case Expr.Apply(path, e)           =>
        Eval.later {
          evaluatePath(path)(input).fold(false) { focus =>
            run(e, bindings)(focus).value
          }
        }
      case Expr.Equals(path, value)      =>
        Eval.now {
          evaluatePath(path)(input).getOrElse(Json.Null) == value
        }
      case Expr.GreaterThan(path, value) =>
        Eval.now {
          evaluatePath(path)(input).flatMap(_.asNumber).fold(false)(_.toDouble > value.toDouble)
        }
      case Expr.LessThan(path, value)    =>
        Eval.now {
          evaluatePath(path)(input).flatMap(_.asNumber).fold(false)(_.toDouble < value.toDouble)
        }
      case Expr.Contains(path, value)    =>
        Eval.now {
          evaluatePath(path)(input)
            .flatMap { j =>
              val stringContains = (j.asString, value.asString).mapN { case (l, r) =>
                l.contains(r)
              }
              val arrayContains  = j.asArray.map { arr =>
                arr.contains(value)
              }
              stringContains.orElse(arrayContains)
            }
            .getOrElse(false)
        }
      case Expr.Not(expr)                => {
        run(expr, bindings)(input).map(!_)
      }
      case Expr.And(left, right)         =>
        for
          l <- run(left, bindings)(input)
          r <- run(right, bindings)(input)
        yield l && r
      case Expr.Or(left, right)          =>
        for
          l <- run(left, bindings)(input)
          r <- run(right, bindings)(input)
        yield l || r
      case Expr.Exists(path, expr)       =>
        Eval.later {
          evaluatePath(path)(input).flatMap(_.asArray) match
            case Some(values) =>
              values.exists { value =>
                run(expr, bindings)(value).value
              }
            case None         =>
              false
        }
      case Expr.Reference(name)          =>
        bindings.get(name) match
          case Some(e) => run(e, bindings)(input)
          case None    => throw new RuntimeException(s"Unable to find binding for $name")
      case Expr.Let(extraBindings, expr) =>
        run(expr, bindings ++ extraBindings)(input)
