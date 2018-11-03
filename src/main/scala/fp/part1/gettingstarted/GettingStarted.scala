package fp.part1.gettingstarted

import scala.annotation.tailrec

object MyModule {
  private def formatFct(x: Int, f: Int => Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, f(x))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(x: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(x, 1)
  }

  def fib(x: Int): Int = {
    @tailrec
    def go(n: Int, sPrec: Int, sNext: Int): Int = {
      if (n == 1) sNext
      else go(n - 1, sNext, sPrec + sNext)
    }
    go(x, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatFct(-42, abs))
    println(formatFct(6, fib))
    println(formatFct(6, x => x + 1))

  }
}

object PolymorphicFunctions {

  def findFirst[T](as: Array[T], p: T => Boolean): Int = {
    @tailrec
    def loop(index: Int): Int = {
      if (index == as.length) -1
      else if (p(as(index))) index
      else loop(index + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean =
      if (index == as.length - 1) true
      else if (ordered(as(index), as(index + 1))) loop(index + 1)
      else false

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
