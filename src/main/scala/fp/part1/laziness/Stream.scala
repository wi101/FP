package fp.part1.laziness

sealed trait Stream[+A] { self =>

  import Stream._

  def headOption: Option[A] = self match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = self match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = self match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                   => empty
  }

  def drop(n: Int): Stream[A] = self match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => self
  }

  def takeWhile(p: A => Boolean): Stream[A] = self match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) //if the p(a) returns true b will never evaluated

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b) //if the p(a) returns false b will never evaluated

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(c: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (c(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(self)({
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None
    })

  def takeViaUnfold(n: Int): Stream[A] = unfold(self -> n) {
    case (Cons(h, t), s) if s > 0 => Some(h(), (t(), n - 1))
    case _                        => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(self) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _                    => None
  }

  def zipWith[B](stream: Stream[B]): Stream[(A, B)] = unfold(self -> stream) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()) -> (t1(), t2()))
    case _                            => None
  }

  def zipAll[B](stream: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(self -> stream) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())) -> (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None) -> (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())) -> (Empty, t2()))
      case (Empty, Empty)        => Some((None, None) -> (Empty, Empty))
    }

  def hasSubsequence[A1 >: A](sub: List[A1]): Boolean =
    foldRight((sub.reverse, false)) {
      case (h, (x :: xs, _)) =>
        if (h == x) (xs, true)
        else (sub, false)
      case (_, (Nil, b)) => (Nil, b)
      case _             => (Nil, false)
    }._2

  def startsWith[A1 >: A](s: Stream[A1]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll { case (h1, h2) => h1 == h2 }

  def tails: Stream[Stream[A]] = cons(self, unfold(self) {
    case Cons(_, t) => Some((t(), t()))
    case Empty => None
  })

  def hasSubsequence2[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[A1 >: A, S](z: S)(f: (A1, S) => S) : Stream[S] =
    foldRight((z, Stream(z))){ case (a, (initial, s)) =>
      val b = f(a, initial)
      (b, cons(b, s))
    }._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = Cons(() => hd, () => tl)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {

    def loop(f0: Int, fn: Int): Stream[Int] =
      cons(f0, loop(fn, f0 + fn))

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty
  }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a -> a))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some(n -> (n + 1)))
  def fibsViaUnfold: Stream[Int] = unfold(0 -> 1) {
    case (f0, fn) => Some((f0, (fn, f0 + fn)))
  }

}

object StreamExercise extends App {
  val s = Stream(1, 2, 3, 4)

  val l = s.toList
  println(s"stream: $s >> toList: $l")

  val t = s.flatMap(_ => Stream("a")).toList
  val t3 = l.takeWhile(_ < 3)
  println(s"take 2 first elmt: $t >> toList: $t3")
  println(s"headOption is: ${s.headOptionViaFoldRight}")

  //infinite stream
  val ones: Stream[Int] = Stream.constant(1)
  val ints: Stream[Int] = Stream.from(-1)
  val fibs = Stream.fibs
  println(s"the first 5 element in the infinite stream: ${ones.take(5).toList}")
  println(s"the first 5 element in the infinite stream: ${ints.take(5).toList}")
  println(
    s"the first 8 element in the infinite fibs stream: ${fibs.take(8).toList}")
  //infinite stream
  val onesUnfold: Stream[Int] = Stream.constantViaUnfold(1)
  val intsUnfold: Stream[Int] = Stream.fromViaUnfold(-1)
  val fibsUnfold = Stream.fibsViaUnfold
  println(
    s"using unfold >> the first 5 element in the infinite stream: ${onesUnfold.take(5).toList}")
  println(
    s"using unfold >> the first 5 element in the infinite stream: ${intsUnfold.take(5).toList}")
  println(
    s"using unfold >> the first 8 element in the infinite fibs stream: ${fibsUnfold.take(8).toList}")

  println(
    s"using unfold >> the first 8 element in the infinite fibs stream: ${Stream
      .unfold(1)(s => Some(s -> (s + 1)))
      .take(8)
      .toList}")

  //zip
  val zip: Stream[(Int, Int)] = Stream(1, 2, 3).zipWith(Stream(4, 5, 6))
  val zipAll: Stream[(Option[Int], Option[Int])] =
    Stream(1, 2, 3).zipAll(Stream(4, 5, 6))
  println(s"zipAll , ${zipAll.take(8).toList}")

  val hasSub = Stream(1 to 5: _*).hasSubsequence(List(4, 5))
  println(s"result: $hasSub")

  val isStartWith = Stream(1 to 5: _*).startsWith(Stream(1))
  println(s"is start with: $isStartWith")

  val tails = Stream(1 to 5: _*).tails.toList.map(_.toList)
  println(s"tails: $tails")

  val scanR = Stream(1, 2, 3).scanRight(0)(_ + _).toList
  println(s"scanRight result: $scanR")
}
