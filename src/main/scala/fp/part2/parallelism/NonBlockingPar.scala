package fp.part2.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlockingPar {
  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }
  type Par[A] = ExecutorService => Future[A]

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          es.submit(new Callable[Unit] { def call: Unit = a(es)(cb) })
    }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }
}
