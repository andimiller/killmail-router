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
import space.inyour.horses.killmail.router.filters.PathOperation.MapArray

package object filters:

  enum PathOperation:
    case DownField(name: String)
    case DownIndex(idx: Int)
    case MapArray(subpath: List[PathOperation])

  enum Expr:
    def spaces2: String = Expr.prettyShow.show(this)

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
      case PathOperation.DownField(name)   => show".$name"
      case PathOperation.DownIndex(idx)    => show"[$idx]"
      case PathOperation.MapArray(subpath) => show"|{$subpath}"
    }
    lazy implicit val showListPathOperation: Show[List[PathOperation]] = Show.show(_.mkString_("root", "", ""))
    given pathParser: Parser[List[PathOperation]]                      = Parser.recursive { recurse =>
      val field         = (p"." *> Parser.charsWhile0(c => !" .[|{}".toSet.contains(c))).map(PathOperation.DownField.apply)
      val idx           = p"[${Numbers.digits}]".map(_.toInt).map(PathOperation.DownIndex.apply)
      val pathOperation = p"|{$recurse}".map(PathOperation.MapArray.apply)
      val list          = field.orElse(idx).orElse(pathOperation).rep0
      p"root" *> list
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

    private def indentString(spaces: Int)(s: String) = s.linesIterator.map(line => (" " * spaces) + line).mkString("\n")

    trait IndentedShow[T] {
      def show(indent: Int, t: T): String
      def show(t: T): String = show(0, t)
    }

    lazy val prettyShow: IndentedShow[Expr] = new IndentedShow[Expr]:
      def show(indent: Int, t: Expr): String = indentString(indent)(t match
        case Expr.Pure(result)             => result.toString
        case Expr.Equals(path, value)      => show"(== $path $value)".stripMargin
        case Expr.GreaterThan(path, value) => show"(> $path $value)"
        case Expr.LessThan(path, value)    => show"(< $path $value)"
        case Expr.Contains(path, value)    => show"(contains $path ${value.noSpaces})"
        case Expr.Not(expr)                => show"(not $expr)"
        case Expr.And(left, right)         => show"""(and
                                                 |${show(indent + 2, left)}
                                                 |${show(indent + 2, right)}
                                                 |)
                                                 |""".stripMargin
        case Expr.Or(left, right)          => show"""(or
                                            |${show(indent + 2, left)}
                                            |${show(indent + 2, right)}
                                            |)
                                            |""".stripMargin
      )

    given Parser[Expr]  = Parser.recursive { recurse =>
      val int = Numbers.digits.map(JsonNumber.fromDecimalStringUnsafe)

      Parser.oneOf(
        List(
          p"true".as(Expr.Pure(true)).orElse(p"false".as(Expr.Pure(false))),
          p"(== $pathParser $jsonParser)".map(Expr.Equals.apply),
          p"(> $pathParser $int)".map(Expr.GreaterThan.apply),
          p"(< $pathParser $int)".map(Expr.LessThan.apply),
          p"(contains $pathParser $jsonParser)".map(Expr.Contains.apply),
          p"(not $recurse)".map(Expr.Not.apply),
          p"(and $recurse $recurse)".map(Expr.And.apply),
          p"(or $recurse $recurse)".map(Expr.Or.apply)
        )
      )
    }
    given codec: StringCodec[Parser, Expr] with
      def parser: Parser[Expr]    = summon
      def encode(e: Expr): String = e.show
    /// circe
    given Encoder[Expr] = Encoder[String].contramap(_.show)
    given Decoder[Expr] = Decoder[String].emap(codec.parser.parseAll(_).leftMap(_.show))

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
            case PathOperation.DownField(name)   => cursor.downField(name)
            case PathOperation.DownIndex(idx)    => cursor.downN(idx)
            case PathOperation.MapArray(subpath) =>
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
