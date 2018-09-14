// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._
import net.degoes.arts.A.{Effect, Geocode, Response, Value}
import scalaz.zio.{App, IO}

import scala.concurrent.{ExecutionContext, Future}

object exercises {

}


object SimpleExample extends App {
  import scalaz.zio.interop.scalaz72._

  //  type Task[A] =IO[Throwable, A]
  //
  //  class MyIO[A](val io: Task[A]) extends AnyVal
  //
  //  def createMonadState[S]: Task[MonadState[Task, S]] = ???
  //
  //  case class MyStateType()
  //
  //  def myLocalState[F[_]: MonadState[?, MyStateType], A](s: F[A]): F[Boolean] = ???
  //
  //  for {
  //    monadState <- createMonadState[MyStateType]
  //    result <- myLocalState[MyIO](monadState)
  //  } yield result

  override def run(args: List[String]): IO[Nothing, SimpleExample.ExitStatus] = ???

  trait Logging[F[_]] {
    def logLine(s: => String): F[Unit]
  }

  object Logging {
    def apply[F[_]](implicit F: Logging[F]): Logging[F] = F
  }

  trait Console[F[_]] {
    def putStrLn(s: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F

    //    implicit val ConsoleIO: Console[IO[Throwable, ?]] =
    //      new Console[IO[Throwable, ?]] {
    //        override def putStrLn(s: String): IO[Throwable, Unit] = console.putStrLn(s)
    //        override def getStrLn: IO[Throwable, String] = console.getStrLn
    //      }

    def putStrLn[F[_]: Console](s: String): F[Unit] = Console[F].putStrLn(s)
    def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn
  }

  // Writer or tell but this is an example of a monad transformer
  case class LoggingT[F[_], A](run: List[String] => F[(List[String], A)]) {
    def map[B](f: A => B)(implicit F: Functor[F]): LoggingT[F, B] =
      LoggingT(log => run(log).map { case (lines, a) => (lines, f(a)) })

    def flatMap[B](f: A => LoggingT[F, B])(implicit F: Monad[F]): LoggingT[F, B] =
      LoggingT(log => run(log).flatMap { case (lines, a) => f(a).run(lines) })

    def eval(implicit F: Functor[F]): F[(List[String], A)] =
      run(Nil).map(t => (t._1.reverse, t._2))
  }

  object LoggingT {
    def point[F[_]: Applicative, A](a: A): LoggingT[F, A] =
      LoggingT(log => (log, a).point[F])

    def lift[F[_]: Functor, A](fa: F[A]): LoggingT[F, A] =
      LoggingT(log => fa.map(a => (log, a)))

    implicit def LoggingTMonad[F[_]: Monad]: Monad[LoggingT[F, ?]] = new Monad[LoggingT[F, ?]] {
      override def bind[A, B](fa: LoggingT[F, A])(f: A => LoggingT[F, B]): LoggingT[F, B] =
        fa.flatMap(f)

      override def point[A](a: => A): LoggingT[F, A] = LoggingT.point(a)
    }

    def log[F[_]: Applicative](line: String): LoggingT[F, Unit] =
      LoggingT(log => (line ::log, ()).point[F])
  }

  import Console._

  def MyApp[F[_]: Console: Monad]: F[String] =
    (for {
      _ <- LoggingT.lift[F, Unit](putStrLn("Hello, what is your name"))
      _ <- LoggingT.log[F]("My log line")
      n <- LoggingT.lift[F, String](getStrLn)
      _ <- LoggingT.lift[F, Unit](putStrLn(s"Good to meet you $n"))
    } yield n).eval.flatMap {
      case (log, name) =>
        putStrLn(log.mkString("\n")).flatMap(_ => name.point[F])
    }
}

object A {
  import scala.concurrent.Future

  case class Response()
  case class Value()
  case class Geocode()

  // Either[E, Option[A]]
  // E + Option[A]
  // E + (Unit + A)
  // (E + Unit) + A
  // Either[Option[E], A] // equal to previous and eliminates overhead on happy path


  type Effect[E, A] = Future[Either[E, Option[A]]]

  // Much slower due to the extra method calls - with original type
//  case class Effect[E, A](run: Future[Either[E, Option[A]]])(implicit ec: ExecutionContext) {
//    def map[B](f: A => B): Effect[E, B] = Effect(run.map(_.map(_.map(f))))
//
//    def flatMap[B](f: A => Effect[E, B]): Effect[E, B] = Effect(run.flatMap {
//      case Left(e) => Future.successful(Left(e))
//      case Right(None) => Future.successful(Right(None))
//      case Right(Some(a)) => f(a).run
//    })
//  }

  // with type after simplification using substitution like above
//  case class Effect[E, A](run: Future[Either[Option[E], A]])(implicit ec: ExecutionContext) {
//    def map[B](f: A => B): Effect[E, B] = Effect(run.map(_.map(f)))
//
//    def flatMap[B](f: A => Effect[E, B]): Effect[E, B] = Effect(run.flatMap {
//      case Left(Some(e)) => Future.successful(Left(Some(e)))
//      case Left(None) => Future.successful(Left(None))
//      case Right(Some(a)) => f(a).run
//    })
//  }

  // type EffectT[E, A] = OptionT[EitherT[Future, E, A], A]

  def geoAPI[E](url: String): Effect[E, Geocode] = ???

  def cacheAPI[E](key: Array[Byte]): Effect[E, Value] = ???

  def queryDatabase[E](query: String): Effect[E, Response] = ???
}

object AboveButTagless {
  trait Effect[F[_, _]] {
    // def monadError[E]: MonadError[F[E, _], E]
    def monad[E]: Monad[F[E, ?]]

    def fail[E, A](e: E): F[E, A]

    def attempt[E, A](fea: F[E, A]): F[Nothing, Either[E, A]]

    def none[E, A]: F[E, A]

    def some[E, A](a: A): F[E, A]

    def fromFuture[E, A](f: Future[A]): F[E, A]

    def toFuture[E, A](fea: F[E, A]): Future[A]
  }

  object Effect {
    def apply[F[_, _]](implicit F: Effect[F]): Effect[F] = F

    type MyIO[E, A] = IO[Option[E], A]

    implicit val EffectIO: Effect[MyIO] = new Effect[MyIO] {
      override def monad[E]: Monad[MyIO[E, ?]] = new Monad[MyIO[E, ?]] {
        override def bind[A, B](fa: MyIO[E, A])(f: A => MyIO[E, B]): MyIO[E, B] =
          fa.flatMap(f)

        override def point[A](a: => A): MyIO[E, A] = IO.point(a)
      }

      override def fail[E, A](e: E): MyIO[E, A] = IO.fail(Some(e))

      override def attempt[E, A](fea: MyIO[E, A]): MyIO[Nothing, Either[E, A]] =
        fea.attempt.flatMap {
          case Left(None) => IO.fail(None)
          case Left(Some(e)) => IO.now(Left(e))
          case Right(a) => IO.now(Right(a))
        }

      override def none[E, A]: MyIO[E, A] = IO.fail(None)

      override def some[E, A](a: A): MyIO[E, A] = IO.now(a)

      override def fromFuture[E, A](f: Future[A]): MyIO[E, A] = ??? // in interop package

      override def toFuture[E, A](fea: MyIO[E, A]): Future[A] = ??? // in interop package
    }

    type Task[A] = IO[Throwable, A]
    type MyTask[E, A] = EitherT[Task, E, A]

    implicit def EffectTask: Effect[MyTask] = ???
  }

  def geoAPI[F[_, _], E](url: String): F[E, Geocode] = ???

  def cacheAPI[F[_, _], E](key: Array[Byte]): F[E, Value] = ???

  def queryDatabase[F[_, _], E](query: String): F[E, Response] = ???
}

object FreeMonad {

  sealed trait ConsoleF[A]
  case class ReadLineOp[A](next: String => A) extends ConsoleF[A]
  case class WriteLineOp[A](line: String, next: A) extends ConsoleF[A]

  sealed trait Free[F[_], A] {
    def fold[G[_]](f: F ~> G): G[A] = ??? // for interpreting the program
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Join[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

  type ConsoleProgram[A] = Free[ConsoleF, A]

//  def myProgram: ConsoleProgram[String] =
//    for {
//      _ <- ???
//    } yield ()
//
//  val myProgramIO: IO[Throwable, String] =
//    myProgram.fold[IO[Throwable, ?]](???)

}