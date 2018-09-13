// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec
import scala.language.higherKinds

object algebra {

  /**
    * Associativity
    * append(x, append(y, z)) == append(append(x, y), z)
    * x |+| (y |+| z) == (x |+| y) |+| z
    * @tparam A
    */
//  trait Semigroup[A] {
//    def append(l: => A, r: => A): A
//  }
//
//  implicit class SemigroupOps[A](val a: A) extends AnyVal {
//    def |+|(other: A)(implicit S: Semigroup[A]): A = S.append(a, other)
//  }
//
//  case class Sum(value: Int)
//  implicit val sumSemigroup: Semigroup[Sum] = new Semigroup[Sum] {
//    override def append(l: => Sum, r: => Sum): Sum = Sum(l.value + r.value)
//  }
//
//
//  case class Product(value: Int)
//  implicit val productSemigroup: Semigroup[Product] = new Semigroup[Product] {
//    override def append(l: => Product, r: => Product): Product = Product(l.value * r.value)
//  }

  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] = new Semigroup[NotEmpty[A]] {
    override def append(f1: NotEmpty[A], f2: => NotEmpty[A]): NotEmpty[A] =
      NotEmpty(f1.head, f1.tail.map(x => append(x, f2)).orElse(Some(f2)))
  }

  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  /**
    * Identity
    * append(mzero, a) === a
    * append(a, mzero) === a
    * @tparam A
    */
//  trait Monoid[A] extends Semigroup[A] {
//    def mzero: A
//  }

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //

  sealed trait AccessLevel
  case object Read extends AccessLevel
  case object Write extends AccessLevel

  case class Email(value: String)

  case class Resource(resources: String, accessLevel: AccessLevel)
  case class Permission(value: Map[Email, Map[Resource, Set[AccessLevel]]])
  implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    override def zero: Permission = Permission(Map())

    override def append(f1: Permission, f2: => Permission): Permission = Permission(f1.value |+| f2.value)
  }
  val example2 = mzero[Permission] |+| Permission(Map())

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
      override def append(f1: (A, B), f2: => (A, B)): (A, B) = (f1._1 |+| f2._1, f1._2 |+| f2._2)
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = new Monoid[NotEmpty[A]] {
    override def zero: NotEmpty[A] = ???

    override def append(f1: NotEmpty[A], f2: => NotEmpty[A]): NotEmpty[A] = ???
  }
}

object functor {


  /**
    * Identity:    fmap(identity) === identity
    * Composition: fmap(f.compose(g)) == fmap(f).compose(fmap(g))
    * @tparam F
    */
//  trait Functor[F[_]] {
//    def map[A, B](fa: F[A])(f: A => B): F[B]
//
//    def fmap[A, B](f: A => B): F[A] => F[B]
//  }
//
//  implicit def FunctorMap[K]: Functor[Map[K, ?]] = new Functor[Map[K, ?]] {
//    override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fmap(f)(fa)
//
//    override def fmap[A, B](f: A => B): Map[K, A] => Map[K, B] = fa => fa.map { case (k, v) => k -> f(v) }
//  }

  val g: Int => String = _.toString
  val f: String => Int = _.length
  val actual = List(1, 232, 9, 45, 62)
  val expect = List(1, 3, 1, 2, 2)

  assert(actual.map(g).map(f) == expect)
  assert(actual.map(f.compose(g)) == expect)
  assert(actual.map(g).map(f) == actual.map(f.compose(g)))
  assert(actual.map(identity) == actual)

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = {

      @tailrec
      def loop(next: List[A], acc: List[B]): List[B] = next match {
        case Nil => acc
        case a :: as => loop(as, f(a) :: acc)
      }

      loop(fa, List())
    }
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(v) => Some(f(v))
      }
  }

  implicit val vectorFunctor: Functor[Vector] = new Functor[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }

  implicit def mapFunctor[K]: Functor[Map[K, ?]] = new Functor[Map[K, ?]] {
    override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)
  }

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }
      }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = new Functor[Parser[E, ?]] {
    override def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
      Parser(input =>
        fa.run(input) match {
          case Left(e) => Left(e)
          case Right((s, a)) => Right((s, f(a)))
        })
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = new Functor[DataType] {
    override def map[A, B](fa: DataType[A])(f: A => B): DataType[B] = ??? // impossibru becase it is in invariant in A
  }

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F, G, ?]] {
    override def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
      FunctorProduct(fa.l.map(f), fa.r.map(f))
  }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F, G, ?]] {
    override def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =
      FunctorSum(fa.run.bimap(_.map(f), _.map(f)))
  }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F, G, ?]] {
    override def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] =
      FunctorNest[F, G, B](fa.run.map(_.map(f)))
  }


  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
    (l, r) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] =
    zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) =>
        zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, _) => List()
    }

  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => (a, b) :: zipList2(as, bs)
      case _ => List()
    }

//  trait Apply[F[_]] extends Functor[F] {
//    def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
//  }
//
//  implicit class ApplySyntax[F[_], A](l: F[A]) {
//    def *>[B](r: F[B])(implicit F: Apply[F]): F[B] =
//      F.zip(l, r).map(_._2) // Not equal to l
//
//    def <*[B](r: F[B])(implicit F: Apply[F]): F[A] =
//      F.zip(l, r).map(_._1) // Not equal to r
//  }
//
//  trait Applicative[F[_]] extends Apply[F] {
//    def point[A](a: => A): F[A]
//  }

  val l = List(1, 2, 3)
  val r = List(9, 2)
  val lr1 = List((1, 9), (1, 2), (2, 9), (2, 2), (3, 9), (3, 2))
  val lr2 = List((1, 9), (2, 2))

  val lr1_mapped1 = lr1.map(_._1)
  // (l <* r) List(1, 1, 2, 2, 3, 3)
  // (l <* r) !== l

  val lr1_mapped2 = lr1.map(_._2)
  // (l *> r) List(9, 2, 9, 2, 9, 2)
  // (l *> r) !== r

  val lr2_mapped1 = lr2.map(_._1)
  // (l <* r) List(1, 2)

  val lr2_mapped2 = lr2.map(_._2)
  // (l *> r) List(9, 2)

  implicit val ApplyOption: Apply[Option] = new Apply[Option] {
    override def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
      (fa, f) match {
        case (Some(a), Some(ab)) => Some(ab(a))
        case _ => None
      }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }


  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = Some(a)

      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
        (fa, f) match {
          case (Some(a), Some(ab)) => point(ab(a))
          case _ => None
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]

  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    (l |@| r)((_, _))

  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    zip(fa, fab).map { case (a, f) => f(a) }

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] =
        Parser(input => Right((input, a)))

      def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] =
        Parser(input => f.run(input) match {
          case Left(e) => Left(e)
          case Right((s0, ab)) => fa.run(s0) match {
            case Left(e) => Left(e)
            case Right((s1, a)) => point(ab(a)).run(s1)
          }
        })
    }

  trait Monad[F[_]] extends Applicative[F] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
//
//  implicit val optionMonad: Monad[Option] = new Monad[Option] {
//    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
//      fa match {
//        case None => None
//        case Some(a) => f(a)
//      }
//
//    override def point[A](a: => A): Option[A] = Some(a)
//
//    override def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
//      (fa, f) match {
//        case (Some(a), Some(ab)) => point(ab(a))
//        case _ => None
//      }
//  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa match {
        case Nil => Nil
        case x :: xs => f(x) ++ bind(xs)(f)
      }

    override def point[A](a: => A): List[A] = List(a)

    override def ap[A, B](fa: => List[A])(f: => List[A => B]): List[B] = ???
  }

  trait Future[+A] { self =>
    def flatMap[B](f: A => Future[B]): Future[B] = ???
  }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {
    override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
      fa match {
        case Leaf(a) => f(a)
        case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
      }

    override def point[A](a: => A): BTree[A] = Leaf(a)

    override def ap[A, B](fa: => BTree[A])(f: => BTree[A => B]): BTree[B] = ???
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    override def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] =
      Parser(input => fa.run(input) match {
        case Left(e) => Left(e)
        case Right((s0, a)) => f(a).run(s0)
      })

    override def point[A](a: => A): Parser[E, A] = Parser(input => Right((input, a)))

    override def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] = ???
  }
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  implicit val bTreeFoldable: Foldable[BTree] = new Foldable[BTree] {
    override def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Leaf(a) => f(a)
        case Fork(l, r) => F.append(foldMap(l)(f), foldMap(r)(f))
      }

    override def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(a) => f(a, z)
        case Fork(l, r) => foldRight(l, foldRight(r, z)(f))(f)
      }

  }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //

  // Not useful
//  implicit def FunctionFoldable[A]: Foldable[A => ?] = new Foldable[A => ?] {
//    override def foldMap[A, B](fa: A => A)(f: A => B)(implicit F: Monoid[B]): B = F.zero
//
//    override def foldRight[A, B](fa: A => A, z: => B)(f: (A, B) => B): B = z
//  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] = new Traverse[BTree] {
    override def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit ev: Applicative[G]): G[BTree[B]] =
      fa match {
        case Leaf(a) => f(a).map[BTree[B]](Leaf(_))
        case Fork(l, r) =>
          val lg = traverseImpl(l)(f)
          val rg =traverseImpl(r)(f)

          (lg |@| rg)(Fork(_, _))
      }
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ??? // impossibru
}

object optics {
  sealed trait Country
  case object UK extends Country
  case object USA extends Country
  case object France extends Country

  object Country {
    val usa: Prism[Country, Unit] =
      Prism[Country, Unit]({
          case USA => Some(())
          case _ => None
        },
        _ => USA)

    def change(c: Country): Prism[Country, Unit] =
      Prism[Country, Unit]({
          case `c` => Some(())
          case _ => None
        },
        _ => c)
  }

  case class Organisation(site: Site, name: String, address: Address)

  object Organisation {
    val siteLens: Lens[Organisation, Site] = Lens[Organisation, Site](_.site, a => _.copy(site = a))
  }

  case class Address(
    number: String,
    street: String,
    postcode: String,
    country: Country)

  case class Site(
    manager: Employee,
    address: Address,
    employees: Set[Employee])

  object Site {
    val managerLens: Lens[Site, Employee] = Lens[Site, Employee](_.manager, a => _.copy(manager = a))
  }

  case class Employee(
    name: String,
    dob: java.time.Instant,
    salary: BigDecimal,
    address: Address)

  object Employee {
    val salaryLens: Lens[Employee, BigDecimal] = Lens[Employee, BigDecimal](_.salary, a => _.copy(salary = a))
  }

  lazy val org: Organisation = ???

//  org.copy(site =
//    org.site.map(site =>
//      site.copy(manager =
//        site.manager.copy(salary =
//          site.manager.salary * 0.95))))

  // this works for monomorphic data structures
  type Optic[S, A]

  // S - Super structure
  // A - Sub structure
  // An optic S A allows you to focus in on a sub structure
  // A inside a super structure S, for the purpose of
  // accessing or modifying the substructure

  // Lenses let you focus in on a term in product type
  // Prisms let you focus in on a term in a sum type

  final case class Lens[S, A](
    get: S => A,
    set: A => S => S
  ) { self =>

    def >>>[B](that: Lens[A, B]): Lens[S, B] =
      Lens[S, B](
        (s: S) => that.get(self.get(s)),
        (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s)
      )

    final def update(f: A => A): S => S = (s: S) => set(f(get(s)))(s)
  }

  val org_lens: Lens[Organisation, BigDecimal] = Organisation.siteLens >>> Site.managerLens >>> Employee.salaryLens

  val newOrg: Organisation = org_lens.update(s => s * 0.95)(org)

  final case class Prism[S, A](
    get: S => Option[A],
    set: A => S
  ) {
    self =>

    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      Prism(
        (s: S) => self.get(s).flatMap(that.get),
        self.set.compose(that.set)
      )

    def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  def _Left[A, B]: Prism[Either[A, B], A] =
    Prism({
        case Left(a) => Some(a)
        case Right(_) => None
      },
      Left(_))

  def _Right[A, B]: Prism[Either[A, B], B] =
    Prism({
        case Left(_) => None
        case Right(b) => Some(b)
      },
      Right(_))

  val org_prism = ???

//  case class Component1[S](lens: Lens[S, Component1.State]) {
//    def run[S](state: S): (S, Boolean) = ???
//  }
//  object Component1 {
//    case class Config(server: String, port: Int)
//    case class State(config: Config)
//  }
//  case class Component2[S](lens: Lens[S, Component2.State]) {
//    def run[S](state: S): (S, Int) = ???
//  }
//  object Component2 {
//    case class Config(loggingDirectory: String)
//    case class State(config: Config)
//  }
//  case class MyAppState(
//                         c1: Component1.State,
//                         c2: Component2.State
//                       )
//  object MyAppState {
//    val c1: Lens[MyAppState, Component1.State] = ???
//    val c2: Lens[MyAppState, Component2.State] = ???
//  }
//  val c1 : Component1[MyAppState] = Component1(MyAppState.c1)
//  val c2 : Component2[MyAppState] = Component2(MyAppState.c2)
}
