package fp.part1.state

import fp.part1.state.GeneralState.State
import fp.part1.state.StateExercise.Candy.Input

/**
  * How to make any stateful API purely functional?
  * RNG: is an implementation of a Random number generator API
  */
trait RNG {
  //generate a random Int and the new state
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    val nonNegative = if (n < 0) -n - 1 else n
    (nonNegative, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng1) = nonNegativeInt(rng)
    val n1 = n / Int.MaxValue.toDouble
    (n1, rng1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    (i -> d, r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def loop(c: Int, list: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (c > 0) {
        val (n, r) = rng.nextInt
        loop(c - 1, list :+ n)(r)
      } else (list, rng)
    }

    loop(count, List.empty[Int])(rng)
  }

  type Rand[+A] = RNG => (A, RNG) //a randomly generated A, this is a program that depends on some RNG

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng) //returns a constant value

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng1 =>
    val (a, rng2) = s(rng1)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleR: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rngC => {
      val (a, rngA) = ra(rngC)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    g(a)(rngA)
  }
  //nonNegativeLessThan using flatMap
  def nonNegativeLessThan_(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }

  //map using flatMap
  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  //map2 using flatMap
  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

/**
  * General purpose type of State to capture common patterns of stateful programs
  */
object GeneralState {

  //to handle with any type of state, we come up with this type
  //type State[S,+A] = S => (A,S)
  //or
  case class State[S, +A](run: S => (A, S)) { self =>
    //We can generalize the general purpose functions: map, map2, flatMap, sequence
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
    def map2[B, C](fb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => fb.map(b => f(a, b)))
  }

  object State {
    //so we can represent Rand:  type Rand[+A] = RNG => (A, RNG) using State
    type Rand[+A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List.empty[A])) { (f, acc) =>
        f.map2(acc)(_ :: _)
      }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()
  }

}

object StateExercise extends App {
  import RNG._

  val rng = SimpleRNG(42)
  val (i, nextRng) = rng.nextInt
  println(s"random number: $i // next state: $nextRng")

  val (nonN, _) = nonNegativeInt(SimpleRNG(24))
  println(s"nonNegative int: $nonN")

  val (d, _) = double(SimpleRNG(25214903928L))
  println(s"double int: $d")

  val (list, _) = ints(5)(SimpleRNG(0))
  println(s"list of random 5 ints: $list")

  val s = sequence(List.fill(4)(nonNegativeEven))(SimpleRNG(42))
  println(s"sequence: $s")

  val nonNLessThan = nonNegativeLessThan(2)(SimpleRNG(42))
  println(s"non less than 2: $nonNLessThan")

  def rollDie: Rand[Int] = nonNegativeLessThan(6)
  val zero: Int = rollDie(SimpleRNG(5))._1
  println(s"zero: $zero")

  /**
    * Candy machine
    */
  object Candy {

    sealed trait Input

    case object Coin extends Input

    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def update(input: Input): State[Machine, Unit] =
      State(machine => {
        (input, machine) match {
          case (_, Machine(_, 0, _))        => ((), machine)
          case (Coin, Machine(false, _, _)) => ((), machine)
          case (Turn, Machine(true, _, _))  => ((), machine)
          case (Turn, Machine(false, candies, c)) =>
            ((), Machine(true, candies - 1, c))
          case (Coin, Machine(true, candies, coins)) =>
            ((), Machine(false, candies, coins + 1))
        }
      })

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      for {
        _ <- State.sequence(inputs.map(input => update(input)))
        machine <- State.get
      } yield (machine.coins, machine.candies)
    }
  }

  val inputs: List[Input] = List(Candy.Coin,
                                 Candy.Turn,
                                 Candy.Coin,
                                 Candy.Turn,
                                 Candy.Coin,
                                 Candy.Turn,
                                 Candy.Coin,
                                 Candy.Turn)

  val initialMachine: Candy.Machine = Candy.Machine(true, 5, 10)
  val result = Candy.simulateMachine(inputs).run(initialMachine)
  println(s"simulate candy machine to get 4 candies: $result")
}
