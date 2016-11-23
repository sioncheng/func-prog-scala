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
    def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    //extracts a value from a Par by actually performing the computation
    def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

    //combines the results of two parallel computations with a binary function
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
    }

    //marks a computation for concurrent evaluation. The evaluation will not
    //actually occur until forced by run
    def fork[A](a: Par[A]): Par[A] = es => {
        es.submit(new Callable[A] {
            def call = a(es).get
        })
    }

    //wraps its unevaluated argument in a Par and marks it for concurrent evaluation
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))


    def sum(ints: IndexedSeq[Int]): Par[Int] = {
        if (ints.size <= 1) {
            Par.unit(ints.headOption.getOrElse(0))
        } else {
            val (l, r) = ints.splitAt((ints.length / 2))
            Par.map2(sum(l), sum(r))(_ + _)
        }
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def sortPar(parList: Par[List[Int]]) = map(parList)((x: List[Int]) => x.sorted)

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = (es: ExecutorService) => {
        //UnitFuture(List(1))
        if (ps.size == 0) {
            UnitFuture(Nil)
        } else if (ps.size == 1) {
            UnitFuture(List(ps.head(es).get))
        } else {
            val (l, r) = ps.splitAt(ps.length / 2)
            Par.map2(Par.sequence(l), Par.sequence(r))((a,b) => a ::: b)(es)
        }
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =  {
        if (as.size == 0) {
            unit(Nil)
        } else if (as.size == 1) {
            if (f(as.head)) unit(Nil) else unit(as)
        } else {
            val (l, r) = as.splitAt(as.length / 2)
            Par.map2(Par.parFilter(l)(f), Par.parFilter(r)(f))((a, b) => a ::: b)
        }
    }
}


object ParMain extends App {
    val exec = Executors.newCachedThreadPool()
    val seq = Range(0, 1000)
    println(Par.sum(seq)(exec).get)

    println(Par.sortPar(Par.unit(seq.toList.reverse))(exec).get)

    //println(Par.asyncF((x:Int) => x * 2.0)(10)(exec).get())

    var pl1 = Par.unit(1)
    var pl2 = Par.unit(2)
    var pl3 = Par.unit(3)
    val ps = List(pl1, pl2, pl3)
    println(Par.sequence(ps)(exec).get)

    println(Par.parFilter(Range(0,10).toList)(_ % 2 == 0)(exec).get)
}