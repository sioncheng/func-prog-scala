package ch7

/**
  * Created by cyq on 23/11/2016.
  */

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions


object Nonblocking {

    sealed trait Future[+A] {
        private[ch7] def apply(k: A => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new AtomicReference[A]
        val latch = new CountDownLatch(1)

        p(es)/*.apply*/((a: A) => {
            println("p(es).apply begin");
            try {
                ref.set(a);
            } catch {
                case e: Exception => println("run exception", e)
            }
            latch.countDown;
            println("p(es).apply end");
        })

        latch.await
        ref.get
    }

    def unit[A](a: A): Par[A] = es => new Future[A] {
        override private[ch7] def apply(k: A => Unit): Unit = {
            println("unit[A](a: A) begin");
            k(a);
            println("unit[A](a: A) end")
        }
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = {
        es.submit(new Callable[Unit]{def call = r})
    }

    def fork[A](a: =>Par[A]): Par[A] = es => new Future[A] {
        override private[ch7] def apply(k: (A) => Unit): Unit = eval(es)(a(es).apply(k))
    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
        override private[ch7] def apply(k: (C) => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None

            val combiner = ch7.Actor[Either[A, B]](es) {
                case Left(a) => br match {
                    case None => ar = Some(a)
                    case Some(b) => Nonblocking.eval(es)(k(f(a,b)))
                }
                case Right(b) => ar match {
                    case None => br = Some(b)
                    case Some(a) => Nonblocking.eval(es)(k(f(a,b)))
                }
            }

            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
        }
    }
}

object NonblockingMain extends App {

    val exec = Executors.newFixedThreadPool(2)

    println("hello")

    val a = Nonblocking.run(exec)(Nonblocking.unit(100))
    println(a)

    val b = Nonblocking.run(exec)(Nonblocking.fork(Nonblocking.unit(1000)))
    println(b)

    val pa = Nonblocking.unit(100)
    val pb = Nonblocking.unit(2.0)
    val mpabc = Nonblocking.map2(pa,pb)(_ * _)
    val c = Nonblocking.run(exec)(mpabc)
    println(c)

    exec.shutdown()
}
