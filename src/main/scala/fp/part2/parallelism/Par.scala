package fp.part2.parallelism

import java.util.concurrent._
import language.implicitConversions

/**
  *Goals:
  * 1. combine asynchronous computations
  * 2. Avoid to wait for them to finish
  */
object Par {

  /**
    * ExecutorService: simplifies the execution of tasks in asynchronous mode, it provides a pool of threads
    * and API for assigning tasks to it.
    * Executor service can execute Runnable or Callable tasks
    * submit(): submits a Callable or Runnable to an ExecutorService => returns a result of the execution: Future
    * Future has a blocking method: get() => returns the actual result of the Callable task's execution.
    * Calling the get() method while the task is still running will cause execution to block until the task is properly
    * executed and the result is available.
    *
    * Par is a container of a value which can be executed in separate thread that we can `get` / `run` hen it becomes available
    * it requires an ExecutionService to run the computation and returns the result of the execution in a Future
    * It's a description of parallel computation,
    */
  type Par[A] = ExecutorService => Future[A]

  /**
    * Par will be interpreted when we call run with a given ExecutorService
    * `run` extracts the resulting value from Par[A], it executes asynchronous tasks
    */
  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

  /**
    * we will use Callable and not Runnable because
    * in order to use Runnable, we have to define a Thread, its methods doesn't return a meaningful value,
    * we cannot get a clear information about our computation (otherwise we need to have some side effects and mutation -_-)
    * We cannot control Runnable because we can't know about its internal behavior,
    * And Threads maps onto operating system threads, it would be preferable to create logical threads.
    *
    * Callable: we can get a result of the computation
    */
  /**
    * it wraps a single value that's why it's called unit, it's always done and can't be cancelled
    */
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  /**
    * This is a type Future(a): get is a computed value of this future,
    * get blocks the current Thread until the value is available
    * f.get = a
    */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(x: Boolean): Boolean = false
    def get(v: Long, unit: java.util.concurrent.TimeUnit): A = get
    def isCancelled(): Boolean = false
    def isDone(): Boolean = true
  }

  /**
    * Combines two parallel computations ? this doesn't evaluate the call of f in separate threads,
    * We can always do fork(map2(a,b)(f)) if we want the evaluation of f to occur in a separate thread.
    * But let's implement map2 assuming that a & b are in separate threads
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = run(es)(a)
      val bf = run(es)(b)
      UnitFuture(f(af.get, bf.get)) //this impl doesn't respect timeout
    }

  def map2_timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = run(es)(a)
      val bf = run(es)(b)
      Map2Future(af, bf, f)
    }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C)
      extends Future[C] {
    var cache: Option[C] = None

    def cancel(x: Boolean): Boolean = a.cancel(x) || b.cancel(x)
    def get: C = compute(Long.MaxValue)
    def get(v: Long, unit: java.util.concurrent.TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(v, unit))
    def isCancelled(): Boolean = a.isCancelled || b.isCancelled
    def isDone(): Boolean = cache.isDefined

    private def compute(timeout: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val a_value = a.get(timeout, TimeUnit.NANOSECONDS)
        val end = System.nanoTime
        val diff = end - start
        val b_value = b.get(timeout - diff, TimeUnit.NANOSECONDS)
        val res = f(a_value, b_value)
        cache = Some(res)
        res
    }
  }

  /**
    * The given `a` will be evaluated in a separate logical thread
    * performs the task asynchrononsly
    * Marks the computation for concurrent evaluation
    */
  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call: A = a(es).get
      })

  /**
    * wraps the expression `a` for concurrent evaluation
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * evaluates the result of a given function asynchrnonously
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  /**
    * infix syntax for Par to be able to call some functions like we had defined them in a trait Par
    * @example
    *          a.map2(b)(f)
    */
  implicit def toParOps[A](a: Par[A]): ParOps[A] = new ParOps[A](a)

  class ParOps[A](a: Par[A]) {
    def run(implicit es: ExecutorService): Future[A] = Par.run(es)(a)
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(a, b)(f)
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) {
      case (p, acc) =>
        map2(p, acc)(_ :: _)
    }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    val lB: List[Par[B]] = l.map(asyncF(f))
    sequence(lB)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    parMap(as.filter(f))(identity)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = { s =>
    val a_val = run(s)(a).get
    f(a_val)(s)
  }

  def flatten[A](p: Par[Par[A]]): Par[A] = flatMap(p)(identity)

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(
      f3: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb) { case (a, b) => (a, b) }, pc) {
      case ((a, b), c) => f3(a, b, c)
    }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(
      f4: (A, B, C, D) => E): Par[E] =
    map2(
      map2(pa, pb) { case (a, b) => (a, b) },
      map2(pc, pd) { case (c, d) => (c, d) }
    ) {
      case ((a, b), (c, d)) => f4(a, b, c, d)
    }

  def map5[A, B, C, D, E, F](pa: Par[A],
                             pb: Par[B],
                             pc: Par[C],
                             pd: Par[D],
                             pe: Par[E])(f5: (A, B, C, D, E) => F): Par[F] =
    map2(
      map2(
        map2(pa, pb) { case (a, b) => (a, b) },
        map2(pc, pd) { case (c, d) => (c, d) }
      ) {
        case ((a, b), (c, d)) => (a, b, c, d)
      },
      pe
    ) {
      case ((a, b, c, d), e) => f5(a, b, c, d, e)
    }

  /**
    * The law of mapping :
    *     * identity: two expressions are identical or equivalent: id(a) = a
    *     map(unit(1))(_ + 1) is equivalent to unit(2).
    *     * composition: map(map(y)(g))(f) == map (y) ( f compose g )
    *     How to prove it?
    *     Given
    *     f compose g = p compose q
    *     map f compose map g = map p compose map q
    *     Assuming that p = id and q = f compose g
    *     map f compose map g
    *   = map id compose map (f compose g) //id(a) = a
    *   = map (f compose g)
    *   So, map f compose map g = map (f compose g)
    *   See more: https://github.com/quchen/articles/blob/master/second_functor_law.md
    */
  /**
    * To check the identity law
    */
  def equals[A, B](e: ExecutorService)(p1: Par[A], p2: Par[B]): Boolean =
    run(e)(p1).get == run(e)(p2).get

  /**
  * The law of foking:
  * fork(x) should do the same thing as x  but asynchronously:
  * fork(x) == x
  *
  */

}
object example extends App {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse (0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.fork(Par.map2(sum(l), sum(r))(_ + _))
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    Par.map(Par.sortPar(unit(ints.toList)))(_.last)

  def getNbWords(paragraphs: List[String]): Par[Int] =
    Par.map(unit(paragraphs))(_.flatMap(_.split(" ")).size)

  implicit val es = Executors.newFixedThreadPool(2)
  Par.equals(es)(fork(lazyUnit(1)), lazyUnit(1))
  es.shutdown()
}
