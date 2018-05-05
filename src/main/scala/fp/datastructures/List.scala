package fp.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = {
    def loop(as: List[A], str: String): String = as match {
      case Nil          => str + ")"
      case Cons(x, Nil) => str + s"$x)"
      case Cons(x, xs)  => loop(xs, str + s"$x, ")
    }
    loop(this, "List(")
  }
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def removeFirstElement[A](list: List[A]): List[A] = list match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], value: A): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(value, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _                   => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def lengthR[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil)((acc, elem) => Cons(elem, acc))

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRightL(a1, a2)(Cons(_, _))

  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A])((acc, elem) => append(acc, elem))

  def transform(as: List[Int]): List[Int] =
    foldRightL(as, Nil: List[Int])((elm, acc) => Cons(elm + 1, acc))

  def toString(as: List[Double]): List[String] =
    foldLeft(as, Nil: List[String])((acc, elm) => Cons(elm.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightL(as, Nil: List[B])((elm, acc) => Cons(f(elm), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightL(as, Nil: List[A])((elm, acc) =>
      if (f(elm)) Cons(elm, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRightL(as, Nil: List[B])((elm, acc) => append(f(elm), acc)) //Or concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addPairList(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) =>
      Cons(ah + bh, addPairList(at, bt))
  }

  // generalize addPairList
  def zipWith[A, B](a: List[A], b: List[B]): List[(A, B)] = (a, b) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) =>
      Cons(ah -> bh, zipWith(at, bt))
  }

  import scala.{List => ScalaList, Nil => ScalaNil}

  def hasSubsequence[A](sup: ScalaList[A], sub: ScalaList[A]): Boolean = {

    def loop(sup1: scala.List[A], sub1: scala.List[A], b: Boolean): Boolean =
      (sup1, sub1) match {
        case (ScalaNil, (_ :: subT)) =>
          if (b) loop(sup, sub1, b = false)
          else false
        case (_, ScalaNil) => b
        case ((supH :: supT), subL @ (subH :: subT)) =>
          if (supH == subH)
            loop(supT, subT, b = true)
          else
            loop(supT, subL, b = false)
      }

    if (sub.length <= sup.length) loop(sup, sub, b = false) else false

  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object ExamplesWithList extends App {
  val ex1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t) //this is the result
    case _                                     => 101
  }

  val l1 = List(1, 2, 3)
  val l2 = List(4, 5, 6)

  import List._

  val resultDrop = drop(l1, 2) // drop the 2 first elements
  val resultDropWhile = dropWhile(l1)(a => a < 10)
  val resultInit = init(l1)
  val exercise8 = foldRight[Int, List[Int]](l1, Nil)(Cons(_, _))
  val exercise9 = lengthR(List(1, 3, 5, 8, 4, 7, 9))
  val exercise10 = foldLeft(l1, 0)(_ + _)

  val l_double = List(3.0, 6.0, 7.0, 9.0, 0.0, 65.0)
  val ex11_sum = sum3(l1)
  val ex11_prod = product3(l_double)
  val ex11_length = lengthL(l_double)

  val ex12 = foldRightL(l1, 0)(_ + _)
  val ex15 = concat(List(l1, l2, List(7), List(8, 9, 10)))
  val ex16 = transform(ex15)
  val ex17 = List.toString(l_double)
  val ex18 = map(ex15)(_ + 1)
  val ex19 = filter(ex15)(_ % 2 > 0)
  val ex20 = flatMap(l1)(i => List(i, i))
  val ex21 = filter2(ex15)(_ % 2 == 0)
  val ex22 = addPairList(l1, l2)
  val ex23 = zipWith(l1, l2)
  val ex24 = hasSubsequence(scala.List(1, 2, 3, 4), scala.List(4, 2))

  println(s"List after drop: $resultDrop")
  println(s"List after drop while: $resultDropWhile")
  println(s"init: $resultInit")
  println(s"exercice 8 result is: $exercise8")
  println(s"exercice 9 result is: $exercise9")
  println(s"exercice 10 result is: $exercise10")
  println(
    s"exercice 11 results are: \nSum: $ex11_sum\nProduct: $ex11_prod\nlength: $ex11_length")
  println(s"exercice 12 result is: $ex12")
  println(s"exercice 15 result is: $ex15")
  println(s"exercice 16 result is: $ex16")
  println(s"exercice 17 result is: $ex17")
  println(s"exercice 18 result is: $ex18")
  println(s"exercice 19 odd values result is: $ex19")
  println(s"exercice 20 result is: $ex20")
  println(s"exercice 21 result is: $ex21")
  println(s"exercice 22 result is: $ex22")
  println(s"exercice 23 result is: $ex23")
  println(s"exercice 24 result is: $ex24")
}
