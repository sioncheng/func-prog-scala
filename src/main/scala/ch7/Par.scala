package ch7

/**
  * Created by cyq on 22/11/2016.
  */

import java.util.concurrent._
import language.implicitConversions

//trait Par[+A]{}


object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //promotes a constant value to a parallel computation
  def unit[A](a: =>A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  //extracts a value from a Par by actually performing the computation
  def run[A](a: Par[A]): A = ???

  //combines the results of two parallel computations with a binary function
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  //marks a computation for concurrent evaluation. The evaluation will not
  //actually occur until forced by run
  def fork[A](a: Par[A]): Par[A] = es => {
    es.submit(new Callable[A]{def call = a(es).get})
  }

  //wraps its unevaluated argument in a Par and marks it for concurrent evaluation
  def lazyUnit[A](a: =>A): Par[A] = fork(unit(a))

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.run(sumL) + Par.run(sumR)
    }
  }

  def sumP(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt((ints.length / 2))
      Par.map2(sumP(l), sumP(r))(_ + _)
    }
  }
}


object ParMain extends App {
  val exec = Executors.newCachedThreadPool()
  val seq = Range(0, 1000)
  println(Par.sumP(seq)(exec).get)
}