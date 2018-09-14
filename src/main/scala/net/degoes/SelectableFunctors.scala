package net.degoes

import java.io.IOException

import scalaz._
import Scalaz._
import scalaz.zio.KleisliIO

object SelectableFunctors {

  // Kleisli F
  // A => F[B]
  // Kleisli IO
  // A => IO[E, B]
  // A => B

  val println: KleisliIO[IOException, String, Unit] = { KleisliIO.impure(???); ??? }
  val readln: KleisliIO[IOException, Unit, String] = { KleisliIO.impure(???); ??? }
  readln >>> println // Super fast due to not boxing

  sealed trait Parser[+E, +A] { self =>
    def map[B](f: A => B): Parser[E, B] = Map(self, f)

    def || [E0 >: E, B](that: Parser[E0, B]): Parser[E0, Either[A, B]] =
      Alternative(self, that)

    def * : Parser[E, List[A]] = Repeat(self)

    def ~ [E0 >: E, B](that: Parser[E0, B]): Parser[E0, (A, B)] = Zip(self, that)

    def <~ [E0 >: E, B](that: Parser[E0, B]): Parser[E0, A] = (self ~ that).map(_._1)

    def ~> [E0 >: E, B](that: Parser[E0, B]): Parser[E0, B] = (self ~ that).map(_._2)
  }

  object Parser {
    def fail[E](e: E): Parser[E, Nothing] = Fail(e)

    def char[E](e: E): Parser[E, Char] = Character(e)

    // SelectableFunctor stuff
    def select[E, A](cond: Parser[E, Boolean])(ifTrue: Parser[E, A], ifFalse: Parser[E, A]): Parser[E, A] =
      Select(cond, ifTrue, ifFalse)

    // Can write a useful switch statement purely in terms of select ^ above
  }

  case class Fail[E](error: E) extends Parser[E, Nothing]
  case class Succeed[A](value: A) extends Parser[Nothing, A]
  case class Character[E](error: E) extends Parser[E, Char]
  case class Repeat[E, A](value: Parser[E, A]) extends Parser[E, List[A]]
  case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
  case class Zip[E,A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
  case class Map[E, A, B](value: Parser[E, A], f: A => B) extends Parser[E, B]

  // SelectableFunctor :)
  // A monad can look an A and for every possible A it can produce a different parsers
  // this type of context sensitivity can only produce a finite number of different parsers
  // this allows optimisations as there is a finite number of case so we can introspect all of the cases
  case class Select[E, A](condition: Parser[E, Boolean], ifTrue: Parser[E, A], ifFalse: Parser[E, A])
    extends Parser[E, A]

  implicit def applicativeParser[E]: Applicative[Parser[E, ?]] = new Applicative[Parser[E, ?]] {
    override def point[A](a: => A): Parser[E, A] = Succeed(a)

    override def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] =
      Map[E, (A => B, A), B](Zip[E, A => B, A](f, fa), t => t._1(t._2))
  }

  import Parser._

  // this allows very low level hand optimized while loops
  // this comes from only using an applicative and it doesn't need to be sequential
  // this could be almost as fast as a hand optimised parser
  // this is unable to make any decisions based on runtime values
  // SelectableFunctors allow you to make runtime decisions using almost no more power than
  // an applicative
  def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] =
    (s: String) => {
      var index: Int = 0
      var error: E = null.asInstanceOf[E]
      var value: A = null.asInstanceOf[A]
      type Repr = () => Unit

      def compile0(parser: Parser[E, A]): Repr = ???

      compile0(parser)

      if(error != null) Left(error) else Right(value)
    }

  sealed trait Json
  sealed trait JsonError

  val ParserJSON: Parser[JsonError, Json] = ???

  val parseJson: String => Either[JsonError, Json] = compiler(ParserJSON)


}