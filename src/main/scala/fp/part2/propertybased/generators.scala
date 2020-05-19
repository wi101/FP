package fp.part2.propertybased
import fp.part1.laziness.Stream
import fp.part1.state.GeneralState.State
import fp.part1.state.RNG
import fp.part2.propertybased.Prop._

// run: runs the property
// in case of the failure: the failure message and the number of successful cases
// in case of success: the number of successful cases
case class Prop(run: (MaxSize, TestCases, RNG) => Result) { self =>

  // prepend the given tag message to the failure message
  def tag(msg: String): Prop = Prop {
   self.run(_, _, _) match {
      case Passed => Passed
      case Falsified(e, s) => Falsified( s"$msg \n $e", s)
    }
  }
  // combines between properties
  final def &&(p: Prop): Prop =
    Prop {
      case (_, n, rng) => self.run(n, rng) match {
        case Passed => p.run(n, rng)
        case failed => failed
      }
    }

  final def ||(p: Prop): Prop =
    Prop {
      case (_, n, rng) => self.run(n, rng) match {
        case Passed => Passed
        case Falsified(msg, _) => p.tag(msg).run(n, rng)
      }
    }
}

object Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    val isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    val isFalsified: Boolean = true
  }

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  // Creates a property
  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    case (max, n, rng) =>

      randomStream(g)(rng).zipWith(Stream.from(0)).take(n).map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    case (max, n, rng) =>
      val testCasePerSize = (n + (max - 1)) / max
      val props = Stream.from(0).take(math.min(n, max) + 1).map(i => forAll(g(i))(f))
      val prop = props.map(p => Prop{(max, _, rng) => p.run(max, testCasePerSize, rng)}).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }


    private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](a: A, e: Exception): String =
    s"""
       |test case: $a\n
       |generated an exception: ${e.getMessage}\n
       |stack trace: \n ${e.getStackTrace.mkString("\n")}
     """.stripMargin
}

final case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def zip[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))

  def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
    sizeGen.flatMap(size => Gen.listOfN(size, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  // choose an integer value between the interval [start, stopExclusive]
  final def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    // make a state that tries to generate a number between start and stopExclusive
    val state: State[RNG, Int] = State { rng: RNG =>
      def loop(rng: RNG): (Int, RNG) = {
        val (n, nextRng) = rng.nextInt
        if (n >= start && n < stopExclusive) {
          (n, nextRng)
        } else loop(nextRng)
      }
      loop(rng)
    }
    Gen(state)
  }

  final def unit[A](a: => A): Gen[A] = Gen(State(a -> _))

  final def boolean: Gen[Boolean] = Gen(
    State { rng =>
      val (int, nextRng) = rng.nextInt
      val a = if (int % 2 == 0) true else false
      (a, nextRng)
    }
  )

  // generates a list with n generated elements
  final def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  final def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(check => if (check) g1 else g2)

  final def weighted[A](genWithW1: (Gen[A], Double), genWithW2: (Gen[A], Double)): Gen[A] = {
    val (g1, w1) = genWithW1
    val (g2, w2) = genWithW2

    val p = w1 / (w1 + w2)

    Gen(State(RNG.double).flatMap(d => if (d < p) g1.sample else g2.sample))
  }

  // generate a list of generated values of type A
  final def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(Gen.unit(n)))
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g(_).map(f))

  def flatMap[B](f: A => SGen[B]) =
    SGen(n => g(n).flatMap(a => f(a).g(n)))

  def zip[B](sgen: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n).zip(sgen(n)))

}
