package fp.part1.handlingerrors

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v)    => Right(f(v))
    case e @ Left(_) => e
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r)    => f(r)
    case e @ Left(_) => e
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r @ Right(_) => r
    case Left(_)      => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      x <- this
      y <- b
    } yield f(x, y)
  // we can do this too: flatMap(a => b.map(f(a, _)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherExamples {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  //convert thrown Exceptions to values
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List.empty)) { (elm, acc) =>
      elm.map2(acc)(_ :: _)
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(List.empty)) { (elm, acc) =>
      f(elm).map2(acc)(_ :: _)
    }

  case class Person(name: Name, age: Age)
  sealed case class Name(value: String)
  sealed case class Age(value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.") else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  def main(args: Array[String]): Unit = {

    val l1 = List(Right(1), Right(2), Right(3))
    val l2 =
      List(Right(1), Right(2), Left("error_1"), Right(3), Left("error_2"))
    val l = List("1", "2", "hi")
    val p1 = mkPerson("", -28)
    println(s" exercise 7: sequence: ${sequence(l1)} | ${sequence(l2)} ")
    println(s" exercise 7: traverse: ${traverse(l)(a => Try(a.toInt))} ")
    println(s" exercise 8: invalid data returns just the first error: $p1 " +
      s"\nIf you want to show all errors in your result define a new data type with a value is a Seq[E]")
  }
}
