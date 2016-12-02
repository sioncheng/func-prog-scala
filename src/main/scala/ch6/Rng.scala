package ch6

/**
  * Created by chengyongqiao on 19/11/2016.
  */


trait RNG {
    def nextInt: (Int, RNG)
    def positiveInt: (Int, RNG)
}


object RNG {

    def simple(seed: Long): RNG = new RNG {
        override def nextInt: (Int, RNG) = {
            val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                ((1L << 48) - 1)
            ((seed2 >>> 16).asInstanceOf[Int],
                simple(seed2))
        }

        override def positiveInt: (Int, RNG) = {
            val (rndInt, newRng) = nextInt
            if (rndInt == Int.MinValue) (0, newRng) else (Math.abs(rndInt), newRng)
        }

    }


    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt // (rng: RNG) => rng.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)// (rng: RNG) => (a, rng)

    def rngToIntAndRng(r: RNG):(Int, RNG) = r.nextInt

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
        (rng: RNG) => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    }

    def positiveMax(n: Int): Rand[Int] = {
        (rng: RNG) => {
            //val s:Rand[Int] = (x:RNG) => {x.positiveInt}
            val f = (x: Int) => if (x < n) x else x % n
            //RNG.map(s)(f)(rng)
            RNG.map(_.positiveInt)(f)(rng)
        }
    }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
        (rng: RNG) => {
            val (a, _) = ra(rng)
            val (b, rng3) = rb(rng)
            (f(a,b), rng3)
        }
    }

    def nonNegative: Rand[Int] =
        rng => {
            val (a, rng2) = rng.nextInt
            if (a < 0) {
                (-1 * a, rng2)
            } else {
                (a, rng2)
            }
        }

    def nonNegativeInt = nonNegative

    def nonNegativeLessThan(n: Int): Rand[Int] = RNG.map(RNG.nonNegative){_ % n}

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, r1) = f(rng)
            g(a)(r1) // We pass the new state along
        }

    def _nonNegativeLessThan(n: Int): Rand[Int] = {
        val g = (i: Int) => {
            val mod = i % n
            if (mod < n) RNG.unit(mod) else RNG._nonNegativeLessThan(n)
        }

        RNG.flatMap(RNG.nonNegative)(g)
    }

    def boolean(rng: RNG): (Boolean, RNG) =
        rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

    def double(rng: RNG): (Double, RNG) = {
        val (i,r) = rng.nextInt
        (i / 1000000.0, r)
    }
}




object RngMain extends App {

    val rng1 = RNG.simple(1020324)

    println("same seed to generate random integer")

    val (rndInt1, _) = rng1.nextInt
    val (rndInt2, _) = rng1.nextInt
    println(rndInt1, rndInt2, rndInt1 == rndInt2)

    println("pass new seed through to generate random integer")

    val (rndInt3, rng2) = rng1.nextInt
    val (rndInt4, _) = rng2.nextInt
    println(rndInt3, rndInt4, rndInt3 == rndInt4)


    println("positive random integer")
    val (rndInt5, _) = rng2.positiveInt
    val (rndInt6, _) = rng2.positiveInt
    val (rndInt7, _) = rng2.positiveInt
    println(rndInt5, rndInt6, rndInt7)

    val u1 = RNG.unit(1)
    val (i1, rng2copy) = u1(rng2)
    println(i1)
    println(rng2 == rng2copy)

    val (rndInt8, rng8) = RNG.rngToIntAndRng(rng2)
    println(rndInt8, rng8)

    println("map")
    val rngDouble = RNG.map(RNG.int)(_ * 2.0)
    println(rngDouble(rng8)._1)
    println(rngDouble(rng8)._1)

    println("positiveMax")
    val max = RNG.positiveMax(100)
    println(max(rng8)._1)
    println(max(rng8)._1)
    println(max(rng2)._1)
    println(max(rng2)._1)

    println("map2")
    val map2f = RNG.map2[Int, Int, Int](RNG.int, RNG.int)((x,y) => x + y)
    val (m21, _) = map2f(rng8)
    val (m22, _) = map2f(rng8)
    println(m21, m22)

    println("nonNegativeLessThan 10")
    println(RNG.nonNegativeLessThan(10)(rng8))

    println("_nonNegativeLessThan 10")
    println(RNG.nonNegativeLessThan(10)(rng8))
}
