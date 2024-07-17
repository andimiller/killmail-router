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
      def show(indent: Int, t: Expr): String = indentString(indent)(t match
        case Expr.Pure(result)             => result.toString
        case Expr.Equals(path, value)      => show"(== $path $value)".stripMargin
        case Expr.GreaterThan(path, value) => show"(> $path $value)"
        case Expr.LessThan(path, value)    => show"(< $path $value)"
        case Expr.Contains(path, value)    => show"(contains $path ${value.noSpaces})"
        case Expr.Not(expr)                => show"(not $expr)"
        case Expr.And(left, right)         => show"""(and
                                            |${show(2, left)}
                                            |${show(2, right)}
                                            |)
                                            |""".stripMargin
        case Expr.Or(left, right)          => show"""(or
                                           |${show(2, left)}
                                           |${show(2, right)}
                                           |)
                                           |""".stripMargin
        case Expr.Exists(path, expr)       => show"""(exists
                                                    |  $path
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

    // pure
    case Pure(result: Boolean)
    // comparisons
    case Equals(path: List[PathOperation], value: Json)
    case GreaterThan(path: List[PathOperation], value: JsonNumber)
    case LessThan(path: List[PathOperation], value: JsonNumber)
    case Contains(path: List[PathOperation], value: Json)
    // boolean algebra
    case Not(expr: Expr)
    case And(left: Expr, right: Expr)
    case Or(left: Expr, right: Expr)
    // combinators
    case Exists(
        path: List[PathOperation],
        expr: Expr
    ) // expects the path to point at an array, checks if any items in that array have this expression evaluate to true

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

    given Show[Expr] with
      def show(t: Expr): String = t match
        case Expr.Pure(result)             => result.toString
        case Expr.Equals(path, value)      => show"(== $path $value)"
        case Expr.GreaterThan(path, value) => show"(> $path $value)"
        case Expr.LessThan(path, value)    => show"(< $path $value)"
        case Expr.Contains(path, value)    => show"(contains $path ${value.noSpaces})"
        case Expr.Not(expr)                => show"(not $expr)"
        case Expr.And(left, right)         => show"(and $left $right)"
        case Expr.Or(left, right)          => show"(or $left $right)"
        case Expr.Exists(path, expr)       => show"(exists $path $expr)"

    private inline def whitespace: Parser[Unit]   = Parser.charsWhile(_.isWhitespace).void
    private inline def whitespace0: Parser0[Unit] = Parser.charsWhile0(_.isWhitespace).void

    lazy implicit val given_Parser_Expr: Parser[Expr] = Parser.defer {
      Parser.recursive { recurse =>
        val int = Numbers.digits.map(JsonNumber.fromDecimalStringUnsafe)

        Parser.oneOf(
          List(
            p"true".as(Expr.Pure(true)).orElse(p"false".as(Expr.Pure(false))),
            p"(== $pathParser $jsonParser)".map(Expr.Equals.apply),
            p"(> $pathParser $int)".map(Expr.GreaterThan.apply),
            p"(< $pathParser $int)".map(Expr.LessThan.apply),
            p"(contains $pathParser $jsonParser)".map(Expr.Contains.apply),
            p"(not $recurse)".map(Expr.Not.apply),
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
            yield Expr.Exists(path, expr)
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
    def run(expr: Expr)(input: Json): Eval[Boolean] = expr match
      case Expr.Pure(result)             => Eval.now(result)
      case Expr.Equals(path, value)      =>
        Eval.now {
          evaluatePath(path)(input).fold(false)(_ == value)
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
      case Expr.Not(expr)                =>
        run(expr)(input).map(!_)
      case Expr.And(left, right)         =>
        for
          l <- run(left)(input)
          r <- run(right)(input)
        yield l && r
      case Expr.Or(left, right)          =>
        for
          l <- run(left)(input)
          r <- run(right)(input)
        yield l || r
      case Expr.Exists(path, expr)       =>
        Eval.later {
          evaluatePath(path)(input).flatMap(_.asArray) match
            case Some(values) =>
              values.exists { value =>
                run(expr)(value).value
              }
            case None         =>
              false
        }
