// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

import java.util.Date

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.util.Try

object types {
  (i: Int) => i * i
  List((i: Int) => i * i)

  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = List(true, false)

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] =
    List(Left(()), Right(true), Right(false))

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] =
  List((true, true), (false, false), (true, false), (false, true))

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] =
    List(Right(()), Left(Right(())), Left(Left(())))

  // Can create an isomorphic representation between 2 sets when the cardinality of the two sets are equal

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person = Person2

  type Person1 = (Int, String)
  case class Person2(age: Int, name: String)

  //
  // EXERCISE 6
  //
  // Prove that `1 * A is equivalent to A` by implementing the following functions
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 7
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //
  type Identifier = Identifier2
  type Identifier1 = Either[Int, String]

  sealed trait Identifier2
  case class Human(value: String) extends Identifier2
  case class Robot(value: Int) extends Identifier2

//  enum Identifier {
//    case Human(value: String)
//    case Robot(value: Int)
//  }

  val v: Identifier2 = ???
  v match {
    case Human(v) => s"Human: $v"
    case Robot(v) => s"Robot: $v"
  }

  sealed trait ProgrammingLanguage
  case object Scala extends ProgrammingLanguage
  case object Haskell extends ProgrammingLanguage
  case object PureScript extends ProgrammingLanguage
  case object APL extends ProgrammingLanguage

  // Exercise 8
  //
  // Prove that `0 + A is equivalent to A` by implementing the following functions
  //
  def to2[A](e: Either[A, Nothing]): A = e match {
    case Left(a) => a
    case Right(n) => (n: A)
  }

  def from2[A](a: A): Either[A, Unit] = Left(a)

  //
  // EXERCISE 9
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //

  // type CreditCard = ???
  case class CreditCard(number: String, expDate: Date, securityCode: Int)

  //
  // EXERCISE 10
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  // type PaymentMethod = ???
  sealed trait PaymentMethod
  case object CreditCardPM extends PaymentMethod
  case object BankTransfer extends PaymentMethod
  case object CryptoCurrency extends PaymentMethod

  //
  // EXERCISE 11
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, employment date.
  //
  // type Employee = ???
  case class Employee(title: String, salary: Int, name: String, startDate: Date)

  // EXERCISE 12
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  // type ChessPiece = ???
  sealed trait ChessPiece
  case object Pawn   extends ChessPiece
  case object Rook   extends ChessPiece
  case object Bishop extends ChessPiece
  case object Knight extends ChessPiece
  case object Queen  extends ChessPiece
  case object King   extends ChessPiece

  //
  // EXERCISE 13
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  // type GameWorld =
  case class GameWorld(
                        map: GameMap
                      )
//
//  sealed trait GameMap
//  case class Dungeon() extends GameMap
//  case class Plains() extends GameMap

  case class GameMap(map: List[Realm], paths: List[RealmId => RealmId])

  case class Character(inv: List[Item], charType: CharType)

  sealed trait CharType
  case class Player(identifier: String) extends CharType
  case class NPC(name: String, npcType: NPCType) extends CharType

  sealed trait NPCType
  case object Wizard extends NPCType
  case object Troll extends NPCType
  case object Ogre extends NPCType

  class RealmId private(value: Int)
  object RealmId {
    // Smart constructor means it is impossible to create an invalid realm id
    def apply(i: Int): Option[RealmId] = Some(new RealmId(i))
  }

  case class Realm(id: RealmId, realmType: RealmType, description: String, inv: List[Item], chars: List[Character])

  sealed trait RealmType
  case object Plains extends RealmType
  case object Dungeon extends RealmType
  case object Caves extends RealmType
  case object Indoors extends RealmType

  sealed trait Item

}

object functions {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Convert the following partial function into a total function.
  //
  def parseInt1(s: String): Try[Int] = Try(s.toInt)
  def parseInt2(s: String): Option[Int] = Try(s.toInt).toOption

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.updated(i, f(arr(i)))
  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[mutable.ArraySeq[A]] =
    Try(arr.updated(i, f(arr(i)))).toOption

  // deffering decisions makes code easier to understand and

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] = Try(a / b).toOption

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }

  def freshId2(seed: Int): (Int, Int) = (seed + 1, seed)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Either[String, A] = as match {
    case List() => Left("Oh no, it's impossible!!!")
    case h :: _ => Right(h)
  }

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }

  final case class Charge(account: Account, amount: Double)
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val coffee = Coffee()
    (coffee, Charge(account, coffee.price))
  }

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  val readLine: String = "john"

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ()

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A =
    List(
      "To list commands, type `commands`.",
        "For help on a command, type `help <command>`",
          "To exit the help page, type `exit`.")
      .foldLeft(println("Welcome to the help page!"))((x, y) => combine(x, println(y)))

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }

//  johns solution
//  type Bitmap = List[List[Boolean]]
//  type Cursor = (Int, Int)
//  type Operation = (Cursor, Bitmap) => (Cursor, Bitmap)
//
//  val draw2: Operation = (c, b) => (c, ???)
//  val goLeft: Operation = (c, b) => ((c._1 - 1, c._2), b)
//  val goRight: Operation = (c, b) => ((c._1 + 1, c._2), b)
//  val goUp: Operation = (c, b) => ((c._1, c._2 - 1), b)
//  val goDown: Operation = (c, b) => ((c._1, c._2 + 1), b)
//
//  def draw2(size: Int, op: Operation): Bitmap = op((0, 0), List.fill(size, size)(false))._2

  sealed trait Operation
  case object Draw    extends Operation
  case object GoLeft  extends Operation
  case object GoRight extends Operation
  case object GoUp    extends Operation
  case object GoDown  extends Operation

  type Bitmap = List[List[Boolean]]
  type Cursor = (Int, Int)

  def wrap(size: Int, x: Int): Int =
    if (x < 0) (size - 1) + ((x + 1) % size) else x % size

  def update(bitmap: Bitmap, cursor: Cursor): Bitmap = {
    bitmap.zipWithIndex.map {
      case (a, i) => a.zipWithIndex.map {
        case (b, j) => if((i, j) == cursor) true else b
      }
    }
  }

  def draw2(size: Int, op: List[Operation]): Option[Bitmap] = {
    def loop(cursor: Cursor, ops: List[Operation], bitmap: Bitmap): (Cursor, List[Operation], Bitmap) =
      ops match {
        case List() => (cursor, List(), bitmap)
        case Draw :: xs => (cursor, xs, update(bitmap, cursor))
        case GoLeft :: xs => ((wrap(size, cursor._1 - 1), cursor._2), xs, bitmap)
        case GoRight :: xs => ((wrap(size, cursor._1 + 1), cursor._2), xs, bitmap)
        case GoUp :: xs => ((cursor._1, wrap(size, cursor._2 - 1)), xs, bitmap)
        case GoDown :: xs => ((cursor._1, wrap(size, cursor._2 + 1)), xs, bitmap)
      }

    size match {
      case 0 => None
      case i => Some(loop((0, 0), op, List.fill(i, i)(false))._3)
    }
  }
}

object higher_order {
  case class Parser[+E, +A](
    run: String => Either[E, (String, A)])

  def fail[E](e: E): Parser[E, Nothing] =
    Parser(_ => Left(e))

  def point[A](a: => A): Parser[Nothing, A] =
    Parser(input => Right((input, a)))

  def char[E](e: E): Parser[E, Char] =
    Parser { input =>
      if (input.isEmpty) Left(e)
      else Right((input.drop(1), input.charAt(0)))
    }

  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]): Parser[E2, Either[A, B]] =
    Parser(input => l.run(input) match {
      case Right((input, a)) => Right((input, Left(a)))
      case Left(_) => r.run(input) match {
        case Right((input, b)) => Right((input, Right(b)))
        case Left(e) => Left(e)
      }
    })

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) =
    a => (f(a), g(a))


  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) =
    (a, c) => { (f(a), g(c)) }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = {
    case Left(a) => f(a)
    case Right(c) => g(c)
  }


  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = {
    case Left(a) => Left(f(a))
    case Right(c) => Right(g(c))
  }

  //
  // EXERCISE 6
  //
  // Implement the following higer-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function called `snd` that returns the second
  // element out of any `(A, B)`.
  //
  object snd {
    def apply[A, B](t: (A, B)): B = t._2
  }
  // snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    @tailrec
    def apply[A](f: A => A)(a: A, i: Int): A = {
      if (i <= 0) a
      else {
        repeat(f)(f(a), i - 1)
      }
    }
  }

  repeat[String](x => x + " ")("hello", 4)

  // repeat[Int](100)(0, _ + 1) // 100
  // repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = Left(a)
  def countExample1b[A, B](a: A, b: B): Either[A, B] = Right(b)
  val countExample1Answer = ???

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = f(a)
  def countExample2b[A, B](f: A => B, g: A => B, a: A): B = g(a)
  val countExample2Answer = ???

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data =
    "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String =
    (data: String) => data.split(";")(1)
  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val Expected =
    Map("2018-09-20" ->
      "On date 2018-09-20, there were 1 power outages")
  def groupBy1(
    l: List[String],
    by: String => String)(
      reducer: (String, List[String]) => String):
      Map[String, String] = ???
  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 {
    def apply[A, B, C](xs: List[A], by: A => B)(reducer: (B, List[A]) => C): Map[B, C] = {
      xs.groupBy(by).map(kv => (kv._1, reducer.tupled(kv)))
    }
  }
}

object higher_kinded {
  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  case class Foo[F[_], A](fa: F[A])
  // [F[_], A] => *
  // [* => *, A] => *
  // [* => *, *] => *


  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Map]

  //
  // EXERCISE 3
  //
  // Create a new type that has kind `(* -> *) -> *`.
  //
  type NewType1[F[_]] = List[F[_]]
  type Answer3 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 4
  //
  // Create a trait with kind `*`.
  //
  trait Answer4

  //
  // EXERCISE 5
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer5[A, B, C]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[F[_], G[_[_]]]

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  
  val ListCollectionLike: CollectionLike[List] = new CollectionLike[List] {
    override def empty[A]: List[A] = List()

    override def cons[A](a: A, as: List[A]): List[A] = a :: as

    override def uncons[A](as: List[A]): Option[(A, List[A])] = as match {
      case Nil => None
      case x :: xs => Some((x, xs))
    }
  }

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }

  val ListSized: Sized[List] = new Sized[List] {
    override def size[A](fa: List[A]): Int = fa.length
  }

  val OptionSized: Sized[Option] = new Sized[Option] {
    override def size[A](fa: Option[A]): Int = fa match {
      case None => 0
      case Some(_) => 1
    }
  }

  import scalaz._
  import Scalaz._

  def FoldableSized[F[_]: scalaz.Foldable]: Sized[F] = new Sized[F] {
    override def size[A](fa: F[A]): Int = fa.foldLeft(0)((a, _) => a + 1)
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapSized1: Sized[Map[String, ?]] = new Sized[Map[String, ?]] {
    override def size[A](fa: Map[String, A]): Int =
      fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    override def size[A](fa: Map[K, A]): Int =
      fa.size
  }

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A, B]: Sized[(A, B, ?)] = new Sized[(A, B, ?)] {
    override def size[C](fa: (A, B, C)): Int = 1 // this returns 1 because size returns the number of `C` in the type
  }
}

object typeclasses {
  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */


  // If we a1: A, a2: A, a3: A
  // Then:
  // lt(a1, a2) && lt(a2, a3) == lt(a1, a3)
  // lt(a1, a1) == false

  // Wihtout typeclasses
  def sort[A](list: List[A])(f: (A, A) => Boolean): List[A] = list match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, moreThan) = xs.partition(f(_, x))

      sort(lessThan)(f) ++ List(x) ++ sort(moreThan)(f)
  }



  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }

  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _) => false
            case (_, Nil) => false
            case (l :: ls, r :: rs) =>
              (l === r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean =
      eq.equals(l, r)
  }

  def nub[A: Eq](xs: List[A]): List[A] = {
    def contains(a1: A, l: List[A]): Boolean =
      l.foldLeft(false)((b, a2) => b || a1 === a2)

    xs.foldLeft[List[A]](Nil) {
      case (acc, a) =>
        if(contains(a, acc)) acc
        else a :: acc
    }.reverse
  }

  implicit val intEq: Eq[Int] = new Eq[Int] {
    override def equals(l: Int, r: Int): Boolean = l == r
  }

  implicit def listEq[A: Eq]: Eq[List[A]] = new Eq[List[A]] {
    override def equals(l: List[A], r: List[A]): Boolean =
      (l, r) match {
        case (Nil, Nil) => true
        case (_, Nil) => false
        case (Nil, _) => false
        case (x :: xs, y :: ys) =>
          x === y && equals(xs, ys)
      }
  }

  nub(1 :: 3 :: 2 :: 5 :: 7 :: 2 :: 9 :: Nil)
  nub[List[Int]](List(1, 2) :: List(3, 2) :: List(9, 0) :: Nil)

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering

  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      override def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (LT, LT) => true
          case (EQUAL, EQUAL) => true
          case (GT, GT) => true
          case _ => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      A.compare(l, r) == LT
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, moreThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(moreThan)
  }

  def sort2[A: Ord](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, moreThan) = xs.partition(_ < x)

      sort2(lessThan) ++ List(x) ++ sort2(moreThan)
  }

  //
  // Scalaz 8 Encoding
  //
  sealed abstract class InstanceOfModule {
    type InstanceOf[T] <: T
    def instanceOf[T](t: T): InstanceOf[T]
  }

  object InstanceOfModule {
    val impl: InstanceOfModule = new InstanceOfModule {
      override type InstanceOf[T] = T
      override def instanceOf[T](t: T) = t
    }
  }
  import InstanceOfModule.impl._

  type ???[A] = Nothing


  /**
    * {{
    * // Identity
    * fa.map(identity) === fa
    *
    * // Compositional
    * fa.map(f).map(g) === fa.map(f(g(_)))
    * }}
    */
  trait FunctorClass[F[_]] {
    def map[A, B](fa: F[A], f: A => B): F[B]
  }

  type Functor[F[_]] = InstanceOf[FunctorClass[F]]

  object FunctorClass {
    def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
  }

  class AnyFunctorSyntax()

  /**
   * {{
   * // Associativity:
   * (a <> b) <> c === a <> (b <> c)
   * }}
   */
  trait SemigroupClass[A] {
    def append(l: => A, r: => A): A
  }
  type Semigroup[A] = InstanceOf[SemigroupClass[A]]
  object SemigroupClass {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

    implicit val SemigroupString: Semigroup[String] =
      instanceOf(
        new SemigroupClass[String] {
          def append(l: => String, r: => String): String = l + r
        })

    implicit def SemigroupList[A]: Semigroup[List[A]] =
      instanceOf(
        new SemigroupClass[List[A]] {
          def append(l: => List[A], r: => List[A]): List[A] = l ++ r
        })
  }

  implicit def AnyToSemigroupSyntax[A](a: => A): SemigroupSyntax[A] =
    new SemigroupSyntax(() => a)

  class SemigroupSyntax[A](l: () => A) {
    def <> (r: => A)(implicit A: Semigroup[A]): A = A.append(l(), r)
  }
  //
  // EXERCISE 2
  //
  // Create an instance of the `Semigroup` type class for `Duration`.
  //
  implicit val SemigroupInstant: Semigroup[Duration] =
    instanceOf(
      new SemigroupClass[Duration] {
        override def append(l: => Duration, r: => Duration): Duration = l + r
      }
    )

  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] =
    instanceOf(
      new SemigroupClass[Int] {
        override def append(l: => Int, r: => Int): Int = l + r
      }
    )

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] =
    instanceOf(
      new SemigroupClass[Set[A]] {
        override def append(l: => Set[A], r: => Set[A]): Set[A] = l.union(r)
      }
    )

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V](implicit ev: Semigroup[V]): Semigroup[Map[K, V]] =
    instanceOf(
      new SemigroupClass[Map[K, V]] {
        override def append(l: => Map[K, V], r: => Map[K, V]): Map[K, V] = {
          l ++ r.map { case (k, v1) =>
            k ->
              l.get(k)
               .map(v2 => ev.append(v1, v2))
               .getOrElse(v1)
          }
        }
      }
    )

  //
  // EXERCISE 6
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //
  /**
   * {{
   * append(zero, a) === a
   * append(a, zero) === a
   * }}
   */
  trait MonoidClass[A] extends SemigroupClass[A] {
    def zero: A
  }

  object MonoidClass {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = A
  }

  type Monoid[A] = InstanceOf[MonoidClass[A]]
  implicit def MonoidSemigroup[A](implicit M: Monoid[A]): Semigroup[A] =
    instanceOf(M)

  def empty[A: Monoid]: A = MonoidClass[A].zero

  //
  // EXERCISE 7
  //
  // Create an instance of the `Monoid` type class for `Duration`.
  //

  import scala.concurrent.duration._

  implicit val MonoidInstant: Monoid[Duration] =
    instanceOf(
      new MonoidClass[Duration] {
        override def zero: Duration = 0.seconds

        override def append(l: => Duration, r: => Duration): Duration = l + r
      }
    )

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] =
    instanceOf(
      new MonoidClass[String] {
        override def zero: String = ""

        override def append(l: => String, r: => String): String = l + r
      }
    )

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] =
    instanceOf(
      new MonoidClass[List[A]] {
        override def zero: List[A] = List()

        override def append(l: => List[A], r: => List[A]): List[A] = l ++ r
      }
    )

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  // implicit val MonoidInt: Monoid[Int] = ???

  //
  // EXERCISE 11
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int)
  implicit val MonoidSum: Monoid[Sum] =
    instanceOf(
      new MonoidClass[Sum] {
        override def zero: Sum = Sum(0)

        override def append(l: => Sum, r: => Sum): Sum = Sum(l.run + r.run)
      }
    )

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int)
  implicit val MonoidProduct: Monoid[Product] =
    instanceOf(
      t = new MonoidClass[Product] {
        override def zero: Product = Product(1)

        override def append(l: => Product, r: => Product): Product = Product(l.run * r.run)
      }
    )

  //
  // EXERCISE 13
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait CollectionClass[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }

  object CollectionClass {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }

  type Collection[F[_]] = InstanceOf[CollectionClass[F]]

  implicit val ListCollection: Collection[List] =
    instanceOf(
      new CollectionClass[List] {
        override def empty[A]: List[A] = List()

        override def cons[A](a: A, as: List[A]): List[A] = a :: as

        override def uncons[A](fa: List[A]): Option[(A, List[A])] = fa match {
          case List() => None
          case x :: xs => Some((x, xs))
        }
      }
    )
}
