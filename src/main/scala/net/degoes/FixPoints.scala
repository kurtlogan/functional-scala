package net.degoes

import net.degoes.FixPoints.Json
import scalaz._
import Scalaz._

object FixPoints {
  type ??? = Nothing


  object fixpoint {

    // We have factored out the recursion by replacing
    // the recursion with a type parameter
    // this means we lose the ability to recurse
    // but can regain this ability with a Fix point data type

    sealed trait Json[+A]

    object Json {
      case object Null extends Json[Nothing]
      case class Bool[A](value: Boolean) extends Json[A]
      case class Num[A](value: BigDecimal) extends Json[A]
      case class Arr[A](array: List[A]) extends Json[A]
      case class Obj[A](obj: Map[String, A]) extends Json[A]

      // this should only travers the A's
      implicit val travvereJson: Traverse[Json] = ???
    }


    case class JDate(time: ???)
    type JsonWithDate[A] = Fix[Coproduct[Json, JDate, A]]

    case class Fix[F[_]](unfix: F[Fix[F]])
    def fix[F[_]](f: F[Fix[F]]): Fix[F] = Fix(f)

    type JsonR = Fix[Json] // Json[Json[Json[Json[.....]]]]

    import Json._

    val example: JsonR = fix(Json.Obj[Fix[Json]](Map(
      "none" -> fix(Null),
      "age" -> fix(Num(24)),
      "bool" -> fix(Bool(true))
    )))

    // Fixpoint fold by only requiring traverse from your data type
    def cata[F[_]: Traverse, Z](fix: Fix[F])(algebra: F[Z] => Z): Z = {
      val a = fix.unfix.map(cata(_)(algebra))
    }
  }

  sealed trait Json { self =>
    import Json._

    def fold[Z](z: Z)(f: (Json, Z) => Z): Z =
      self match {
        case j => f(j, z)
      }
  }

  object Json {
    case object Null extends Json
    case class Bool(value: Boolean) extends Json
    case class Num(value: BigDecimal) extends Json
    case class Arr(array: List[Json]) extends Json
    case class Obj(obj: Map[String, Json]) extends Json
  }



  def collectedFieldName(json: Json): List[String] =
    json.fold[List[String]](Nil) {
      case (_, l) => l
    }
}