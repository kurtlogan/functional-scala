package net.degoes

import scalaz.Scalaz._
import scalaz._
import scalaz.zio._

object FunctionalScala extends App {
  final case class URL private (url: String) {
    final def relative(page: String): Option[URL] = URL(urlParts.take(urlParts.length - 1).mkString("/") + "/" + page)

    private def urlParts: List[String] = url.split("/").toList
  }
  object URL {
    def apply(url: String): Option[URL] =
      scala.util.Try(new java.net.URI(url).parseServerAuthority()).toOption match {
        case None => None
        case Some(_) => Some(new URL(url))
      }
  }

  trait HttpClient[F[_, _]] {
    def getURL(url: URL): F[Exception, String]
  }
  object HttpClient {
    def apply[F[_, _]](implicit F: HttpClient[F]): HttpClient[F] = F

    implicit val HttpClientIO: HttpClient[IO] =
      new HttpClient[IO] {
        def getURL(url: URL): IO[Exception, String] =
          IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString).
            retry(DefaultCrawlSchedule)
      }
  }

  def getURL[F[_, _]: HttpClient](url: URL): F[Exception, String] =
    HttpClient[F].getURL(url)

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]]{
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  val DefaultCrawlSchedule: Schedule[Exception, Unit] =
    Schedule.once
  /// exponential(10.milliseconds).jittered && Schedule.recurs(20)).void

  trait Effect[F[+_, +_]] {
    def monad[E]: Monad[F[E, ?]]

    def fail[E](e: E): F[E, Nothing]

    def redeem[E1, E2, A, B](fa: F[E1, A])(err: E1 => F[E2, B], succ: A => F[E2, B]): F[E2, B]
  }
  object Effect {
    def apply[F[+_, +_]](implicit F: Effect[F]): Effect[F] = F

    implicit val EffectIO: Effect[IO] = new Effect[IO] {
      def monad[E]: Monad[IO[E, ?]] = new Monad[IO[E, ?]] {
        def point[A](a: => A): IO[E, A] = IO.point(a)
        def bind[A, B](fa: IO[E, A])(f: A => IO[E, B]): IO[E, B] = fa.flatMap(f)
      }
      def fail[E](e: E): IO[E, Nothing] = IO.fail(e)
      def redeem[E1, E2, A, B](fa: IO[E1, A])(err: E1 => IO[E2, B], succ: A => IO[E2, B]): IO[E2, B] =
        fa.redeem(err, succ)
    }
  }
  implicit def EffectMonad[F[+_, +_]: Effect, E]: Monad[F[E, ?]] =
    new Monad[F[E, ?]] {
      def point[A](a: => A): F[E, A] = Effect[F].monad.point(a)

      def bind[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] =
        Effect[F].monad.bind(fa)(f)
    }
  implicit class EffectSyntax[F[+_, +_], E1, A](fea: F[E1, A]) {
    def redeem[E2, B](err: E1 => F[E2, B], succ: A => F[E2, B])(implicit F: Effect[F]): F[E2, B] =
      F.redeem(fea)(err, succ)
    def redeemPure[B](err: E1 => B, succ: A => B)(implicit F: Effect[F]): F[Nothing, B] =
      redeem[Nothing, B](
        err.andThen(F.monad[Nothing].point[B](_)),
        succ.andThen(F.monad[Nothing].point[B](_)))
  }

  // Par
  def crawlIO[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = {

    def loop(seeds: Set[URL], visited: Ref[Set[URL]], crawl0: Ref[Crawl[E, A]]): IO[Exception, Crawl[E, A]] =
      IO.parTraverse(seeds) { url =>
        for {
          html  <- getURL[IO](url)
          crawl <- process1(url, html)
          links <- visited.get.map(extractURLs(url, html).toSet.flatMap(router) diff _)
        } yield (crawl, links)
      }
      .map(_.foldMap())
      .flatMap {
        case (crawl1, links) =>
          visited.update(_ ++ seeds).flatMap { _ =>
            crawl0.update(x => x |+| crawl1).flatMap { _ =>
              loop(links, visited, crawl0)
            }
          }
      }

    def process1(url: URL, html: String): IO[Nothing, Crawl[E, A]] =
      processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))

    for {
      set <- Ref(Set.empty[URL])
      crawlRef <- Ref(mzero[Crawl[E, A]])
      crawl <- loop(seeds, set, crawlRef)
    } yield crawl
  }

  def crawl[F[+_, +_]: HttpClient: Effect, E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => F[E, A]): F[Exception, Crawl[E, A]] = {

    def loop(seeds: Set[URL], visited: Set[URL], crawl0: Crawl[E, A]): F[Exception, Crawl[E, A]] =
      (seeds.toList.traverse { url =>
        for {
          html  <- getURL[F](url)
          crawl <- process1(url, html)
          links = extractURLs(url, html).toSet.flatMap(router) diff visited
        } yield (crawl, links)
      }).map(_.foldMap(identity)).flatMap {
        case (crawl1, links) => loop(links, visited ++ seeds, crawl0 |+| crawl1)
      }

    def process1(url: URL, html: String): F[Nothing, Crawl[E, A]] =
      processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))

    loop(seeds, Set(), mzero[Crawl[E, A]])
  }

  final case class ProcessorError[E](error: E, url: URL, html: String)
  def crawlE[F[+_, +_]: Effect: HttpClient, E, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => F[E, A]): F[Exception, Crawl[List[ProcessorError[E]], A]] =

    crawl(seeds,  router, (url, html) =>
      processor(url, html).redeem(
        e => Effect[F].fail(List(ProcessorError(e, url, html))),
        Effect[F].monad[List[ProcessorError[E]]].point[A](_)))

  def toURL(url: String): IO[Exception, URL] =
    IO.fromOption(URL(url)).leftMap(
      _ => new Exception(s"Invalid seed URL: $url"))


  case class CrawlState(content: Map[URL, String], error: URL => Exception)

  case class CrawlTest[+E, +A](run: CrawlState => Either[E, A])

  object CrawlTest {
    implicit val HttpClientCrawlTest: HttpClient[CrawlTest] =
      new HttpClient[CrawlTest] {
        def getURL(url: URL): CrawlTest[Exception, String] =
          CrawlTest(state => state.content.get(url).fold[Either[Exception, String]](Left(state.error(url)))(Right(_)))
      }

    implicit val EffectCrawlTest: Effect[CrawlTest] = new Effect[CrawlTest] {
      override def monad[E]: Monad[CrawlTest[E, ?]] = new Monad[CrawlTest[E, ?]] {
        override def bind[A, B](fa: CrawlTest[E, A])(f: A => CrawlTest[E, B]): CrawlTest[E, B] =
          CrawlTest(state => fa.run(state) match {
            case Left(e) => Left(e)
            case Right(a) => f(a).run(state)
          })

        override def point[A](a: => A): CrawlTest[E, A] =
          CrawlTest(_ => Right(a))
      }

      override def fail[E](e: E): CrawlTest[E, Nothing] = CrawlTest(_ => Left(e))

      override def redeem[E1, E2, A, B](fa: CrawlTest[E1, A])
                                       (err: E1 => CrawlTest[E2, B], succ: A => CrawlTest[E2, B]): CrawlTest[E2, B] =
        CrawlTest(state => fa.run(state) match {
          case Left(e1) => err(e1).run(state)
          case Right(a) => succ(a).run(state)
        })
    }
  }

//  trait Bind[F[_]] {
//    def bind[G[_], A, B](fa: F[A])(f: A => G[B]): G[B]
//  }
//
//  object Bind {
//    def listBind[G[_]: Monad](implicit ev: Monoid[G[?]]: Bind[Option] = {
//      new Bind[Option] {
//        override def bind[G[_], A, B](fa: Option[A])(f: A => G[B]): G[B] = fa match {
//          case None => mzero[G[B]]
//          case Some(v) => f(v)
//        }
//      }
//    }
//  }
//
//  case class OptionT[F[_], A](run: F[Option[A]]) {
//    def map[B](f: A => B)(implicit F: Functor[F]): F[Option[B]] = run.map(_.map(f))
//    def flatMap[B](f: A => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] = run.flatMap {
//      case Some(a) => f(a)
//      case None => F.point(None)
//    }
//  }

  val testData1: CrawlState =
    CrawlState(Map(
      URL("http://scalaz.org").get -> """<a href="index.html">This link</a> <a href="overview.html">bad link</a>""",
      URL("http://scalaz.org/index.html").get -> """<a href="index.html">This link</a> <a href="overview.html">Overview</a>""",
      URL("http://scalaz.org/overview.html").get -> """<a href="http://scalaz.org">Home</a>"""),
      url => new Exception(s"Bad url: $url"))

  val testResult = crawlE(Set(URL("http://scalaz.org").get), Set(_),
    (url: URL, html: String) => CrawlTest(_ => Right(List((url, html))))).run(testData1)

  println("*******************")
  println(testResult)
  println("*******************")

  trait Read[F[_]] {
    def readLine: F[String]
  }

  trait Write[F[_]] {
    def write(s: String): F[Unit]
  }

  type Console[F[_]] = Read[F] with Write[F]

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      seeds <- IO.traverse(args)(toURL).map(_.toSet)
      _     <- console.putStrLn(s"Seeds: ${seeds.mkString("\n")}")
      router    = (url: URL) => if (url.url.contains("zio")) Set(url) else Set.empty[URL]
      processor = (url: URL, html: String) => console.putStrLn(s"Traversing ${url.url}: ${html.take(100)}")
      crawl <- crawlE(seeds, router, processor)
      _     <- console.putStrLn(crawl.error.mkString("\n"))
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

//  def myCode1: Future[Try[Boolean]]
//
//  Task[Either[E, A]]
//  F[E, A]
//
//  trait FromFuture[F[_]] {
//    def fromFuture[A](fa: => Future[A]): F[A]
//  }
//  implicit val MyInstance: FromFuture[IO[Throwable, ?]] with MonadError[IO[Throwable, ?], Throwable] {
//
//  }
//
//  def myNewCode[F[_]: MonadError[?, Throwable]: FromFuture]: F[Boolean]
//
//  case class OptionT[F[_], +A](run: F[Option[A]]) {
//    case class ErrorT[F[_], +E, +A](run: F[Either[E, A]]) {
//      def map[B](f: A => B)(implicit F: Functor[F]): ErrorT[F, E, B] = ???
//      def flatMap[B](f: A => ErrorT[F, E, B])(implicit F: Monad[F]): ErrorT[F, E, B] = ???
//    }
//    object ErrorT {
//      def point[F[_]: Applicative, A](a: => A): ErrorT[F, Nothing, A] =
//        ErrorT[F, Nothing, A](Right(a).point[F])
//    }
//    def myCode1: ErrorT[Future, Error, Unit] = ???
//    def myCode2[F[_]: MonadError[Error, ?]]: F[Unit] = ???
//    type ErrorfulList[A] = ErrorT[List, Error, A]
//    trait MonadError[F[_], E] {
//      def fail[A](e: E): F[A]
//      def attempt[A](fa: F[A]): F[Either[E, A]]
//    }
}