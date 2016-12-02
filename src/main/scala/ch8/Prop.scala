package ch8

import ch6.RNG
import ch6.State
import ch8.Prop.{Result, TestCases}

/**
  * Created by cyq on 28/11/2016.
  */

case class Gen[A](sample: State[RNG,A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => this.listOfN(n))
}

object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

    def unit[A](a: => A): Gen[A] = {
        Gen(State.unit(a))
    }

    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
        val genList = List.fill(n)(g.sample)
        val state = State.sequence(genList)
        Gen(state)
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
        Gen.boolean.flatMap(b => if (b) g1 else g2)
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val probability = g1._2.abs / (g1._2.abs + g2._2.abs)
        val state = State(RNG.double).flatMap( d => if (d > probability) g1._1.sample else g2._1.sample)
        Gen(state)
    }
}

object Prop {
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int
    type Result = Option[(FailedCase, SuccessCount)]
}

case class Prop(run: TestCases => Result)


object PropMain extends App {
    println("hello prop")
    val gen = Gen.choose(1, 100)
    val rng = RNG.simple(1000)
    println(gen.sample.run(rng)._1)
    println(Gen.boolean.sample.run(rng)._1)
    println(Gen.listOfN(5,gen).sample.run(rng)._1)
    println(gen.listOfN(6).sample.run(rng)._1)
    val gen2 = Gen.choose(100,200)
    println(Gen.union(gen,gen2).sample.run(rng)._1)
    println(Gen.weighted((gen, 0.01),(gen2,0.00324)).sample.run(rng)._1)
}