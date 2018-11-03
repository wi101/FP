package fp.part1.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[T](t: Tree[T]): Int = {
    def treeHelper(th: Tree[T], length: Int): Tree[Int] = th match {
      case Leaf(_) => Leaf(length)
      case Branch(l, r) =>
        Branch(treeHelper(l, length + 1), treeHelper(r, length + 1))
    }
    maximum(treeHelper(t, 0))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeUsingFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximumUsingFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthUsingFold[T](t: Tree[T]): Int =
    fold(t)(_ => 0)((l, r) => 1 + l max r)

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}

object ExamplesWithTree extends App {
  val simpleTree = Branch(Leaf('a'), Leaf('b'))
  val t: Tree[Int] = Branch(
    Branch(Branch(Leaf(5), Leaf(16)), Branch(Leaf(7), Leaf(8))),
    Branch(Leaf(9), Leaf(10)))

  val t1 = Branch(t, t)

  import Tree._

  val ex25Simple = size(simpleTree)
  val ex25 = size(t)
  val ex26 = maximum(t)
  val ex27Simple = depth(simpleTree)
  val ex27 = depth(t)
  val ex28 = map(t)(_ + 1)
  val ex29 = fold(t)(v => Leaf(v.toString + "#"): Tree[String])(Branch(_, _))

  println(s"result of exercise 25: $ex25Simple")
  println(s"result of exercise 25: $ex25")
  println(s"result of exercise 26: $ex26")
  println(s"result of exercise 27 using a simple tree: $ex27Simple")
  println(s"result of exercise 27: $ex27")
  println(s"result of exercise 28: $ex28")
  println(s"result of exercise 29: $ex29")
}
