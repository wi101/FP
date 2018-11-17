package fp.part1.state

import fp.part1.state.GeneralState.State

object CandyMachine extends App {

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
  val inputs: List[Candy.Input] = List(Candy.Coin,
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
