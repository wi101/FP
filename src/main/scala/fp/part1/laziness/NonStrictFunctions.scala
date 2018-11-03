package fp.part1.laziness

object NonStrictFunctions extends App {

  def `&&`(c1: Boolean, c2: => Boolean): Boolean = if (c1) c2 else c1
  def `||`(c1: Boolean, c2: => Boolean): Boolean = if (c2) c1 else c2
  def `if`[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def evaluateTwice(b: Boolean, i: => Int): Int = if (b) i + i else 0

  val res: Int = evaluateTwice(true, {println("evaluate 2 + 2"); 2})
  //"evaluate 2 + 2" appears twice (because we called i twice, and i will be evaluated everytime when we call it)
  println(s"evaluate result: $res")
}
