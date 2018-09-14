package net.degoes

import scalaz.Lens

object ReaderEx {

  // This technique can be used for multiple monad including reader and state

  trait MonadReader[R, F[_]] {
    def read: F[R]
  }

  case class LowDslEnv1()
  case class LowDslEnv2()
  case class GlobalEnv(env1: LowDslEnv1, env2: LowDslEnv2)

  object GlobalEnv {
    implicit val GlobalHasEnv1: HasEnv1[GlobalEnv] = ???
    implicit val GlobalHasEnv2: HasEnv2[GlobalEnv] = ???
  }

  def program[F[_]: MonadReader[GlobalEnv, ?]]: F[Unit] = ???

  trait HasEnv1[R] {
    def env1: Lens[R, LowDslEnv1]
  }

  trait HasEnv2[R] {
    def env1: Lens[R, LowDslEnv1]
  }

  // Unified as both dsl's use the same type R
  // polymorhic over the type of environment
  // use lenes to get access to part of it you need
  def myLowDsl1[R: HasEnv1, F[_]: MonadReader[R, ?]]: F[Unit] = ???
  def myLowDsl2[R: HasEnv2, F[_]: MonadReader[R, ?]]: F[Unit] = ???
}