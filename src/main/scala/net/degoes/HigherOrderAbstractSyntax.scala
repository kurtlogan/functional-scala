package net.degoes

import scalaz._
import Scalaz._
import scalaz.zio._

object HigherOrderAbstractSyntax {

  type ??? = Nothing

  /**
    * let i = 0
    * in while (i < 10) { i = i + 1 }
    */

  trait Expr[F[_]] {
    def intLit(value: Int): F[Int]
    def add[A](l: F[Int], r: F[Int]): F[Int]
    def let[A, B](name: Symbol, value: F[A], body: F[A] => F[B]): F[B]
    def updateVariable[A](name: Symbol, value: F[A]): F[A]
    def lessThan(left: F[Int], right: F[Int]): F[Boolean]
    def while0[A](condition: F[Boolean], body: F[A]): F[Unit]
  }

//  this is with associated type - notice the Var[_] variable
//  trait Expr[F[_]] {
//    // Associated types?
//    type Var[_]
//
//    def intLit(value: Int): F[Int]
//    def add[A](l: F[Int], r: F[Int]): F[Int]
//    def let[A, B](name: Symbol, value: F[A], body: Var[A] => F[B]): F[B]
//    def updateVariable[A](name: Var[A], value: F[A]): F[A]
//    def value[A](variable: Var[A]): F[A]
//    def lessThan(left: F[Int], right: F[Int]): F[Boolean]
//    def while0[A](condition: F[Boolean], body: F[A]): F[Unit]
//  }

  object Expr {
    def apply[F[_]](implicit F: Expr[F]): Expr[F] = F
  }

  implicit class IntExprSyntax[F[_]](left: F[Int]) {
    def + (right: F[Int])(implicit F: Expr[F]): F[Int] = F.add(left, right)
    def < (right: F[Int])(implicit F: Expr[F]): F[Boolean] = F.lessThan(left, right)
  }

  def int[F[_]: Expr](i: Int): F[Int] = Expr[F].intLit(i)
  def let[F[_]: Expr, A, B](name: Symbol, value: F[A])(body: F[A] => F[B]): F[B] =
    Expr[F].let(name, value, body)
  def while0[F[_]: Expr, A](condition: F[Boolean])(body: F[A]): F[Unit] =
    Expr[F].while0(condition, body)
  def updateVariable[F[_]: Expr, A](name: Symbol, value: F[A]): F[A] =
    Expr[F].updateVariable[A](name, value)


  case class IState(map: Map[Symbol, Any]) {
    def addValue(name: Symbol, value: Any): IState = copy(map = map + (name -> value))

    def removeVariable(name: Symbol): IState = copy(map = map - name)
  }

  type MyIO[A] = IO[String, A]

//  def interpret(ref: Ref[IState]): Expr[MyIO] = new Expr[MyIO] {
//    override def intLit(value: Int): MyIO[Int] =
//      IO.now(value)
//
//    override def add[A](l: MyIO[Int], r: MyIO[Int]): MyIO[Int] =
//      l.seqWith(r)(_ + _)
//
//    override def let[A, B](name: Symbol, value: MyIO[A], body: MyIO[A] => MyIO[B]): MyIO[B] =
//      for {
//        v <- value
//        _ <- ref.update(_.addValue(name, v))
//        b <- body
//        _ <- ref.update(_.removeVariable(name))
//      } yield b
//
//    override def updateVariable[A](name: Symbol, value: MyIO[A]): MyIO[A] =
//      for {
//        v <- value
//        _ <- ref.update(_.addValue(name, v))
//      } yield v
//
//    override def lessThan(left: MyIO[Int], right: MyIO[Int]): MyIO[Boolean] =
//      left.seqWith(right)(_ < _)
//
//    override def while0[A](condition: MyIO[Boolean], body: MyIO[A]): MyIO[Unit] =
//      (for {
//        b <- condition
//        _ <- if (b) body else IO.unit
//      } yield b).repeat(Schedule.doWhile[Boolean](identity)).void
//  }


  def program[F[_]: Expr]: F[Unit] =
    let('i, int(0))(i => // Can create variables that can clash
      while0(i < int(10))(
        updateVariable('i, i + int(1)) // 'i still not fully type safe
      )
    )
}