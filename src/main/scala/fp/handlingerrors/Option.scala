package fp.handlingerrors

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None    => ob
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionExamples {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //variance = math.pow(x - m, 2)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(va => b.map(vb => f(va, vb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty))((maybeElm, acc) =>
      map2(maybeElm, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List.empty))((elm, acc) =>
      map2(f(elm), acc)(_ :: _))

  //convert thrown Exceptions to values
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case _: Exception => None }

  /**
    * Top secret formula for computing an annual car
    * insurance premium from two key factors.
    */
  def parseInsuranceRateQuote(age: Int,
                              numberOfSpeedingTickets: Int): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def main(args: Array[String]): Unit = {
    val xs: Seq[Double] = List(1, 3, 5)
    val m = mean(xs)
    val v = variance(xs)
    val maybeAbs = absO(v.map(_ * -1))
    println(s"xs: $xs, mean: $m, variance: $v, abs: $maybeAbs")

    val l1 = List(Some(1), Some(2), Some(3))
    val l2 = List(Some(1), Some(2), Some(3), None)
    val l = List("1", "2", "hi")
    println(s" exercise 4:  ${sequence(l1)} | ${sequence(l2)} ")
    println(s" exercise 5:  ${traverse(l)(a => Try(a.toInt))} ")
  }
}
