package ch2

object MyModule {

	def abs(n: Int): Int = {
		if (n < 0) -n else n
	}

	def factorial(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, acc: Int): Int = {
			if (n <= 0) acc else go(n-1, n*acc)
		}

		go(n, 1)
	}


	private def formatResult(name: String, n: Int, f: Int => Int) = {
		val msg = "The %s of %d is %d"
		msg.format(name, n, f(n))
	}

	def fib(n: Int): Int = {

        /*
        def fibRec(n: Int): Int = {

            if (n <= 1) {
                0
            } else {
                if (n == 2) {
                    1
                } else {
                    fib(n - 1) + fib(n - 2)
                }
            }
        }

        fibRec(n)
        */

        @annotation.tailrec
        def fibTailRec(n: Int, acc1: Int, acc2: Int, flag: Int): Int = {

            if (n <= 2) {
                if (n == 2) {
                    1
                } else {
                   0
                }
            } else {
                if (n == flag) {
                    acc1 + acc2
                } else {
                    fibTailRec(n, acc2, acc1 + acc2, flag + 1)
                }
            }
        }

        fibTailRec(n, 0, 1, 3)
    }

    def findFirst[A](arr: Array[A], f: A => Boolean): Int = {

        @annotation.tailrec
        def go(n: Int): Int = {
            if (n >= arr.length) {
                -1
            } else {
                if (f(arr(n))) {
                    n
                } else {
                    go(n + 1)
                }
            }
        }

        go(0)
    }

    def isSorted[A](as: Array[A], order:(A,A) => Boolean): Boolean = {

        @annotation.tailrec
        def go(n: Int): Boolean = {
            if (n >= as.length - 1) {
                true
            } else {
                if (order(as(n), as(n+1))) {
                    go(n + 1)
                } else {
                    false
                }
            }
        }

        go(0)
    }

    def curry[A, B, C](f: (A,B) => C): A => (B => C) =
        (a: A) => (b: B) => f(a,b)

    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
        /*def ff(a: A, b: B): C = {
            val fa = f(a)
            fa(b)
        }
        ff
        */
        (a: A, b: B) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

	def main(args: Array[String]): Unit = {
		println(formatResult("absolute value", -34, abs))
		println(formatResult("factorial value", 3, factorial))
        for (n <- 1 to 10) {
            println(fib(n))
        }
        println(findFirst(Array(1,2,3,4), (x: Int) => x == 3))
        println(findFirst(Array("A","B","C","D"), (x: String) => x.length() == 2))
        println(isSorted(Array(1,2,3,4), (x:Int, y:Int) => x < y))
        println(isSorted(Array(1,2,3,4), (x:Int, y:Int) => x+2 > y))

        def ff(a: Int, b: Int): String = (a + b).toString
        val ffCurry = curry(ff)
        println(ffCurry(1))
        println(ffCurry(1)(2))
        val ffUncurry = uncurry(ffCurry)
        println(ffUncurry)

        val c = compose((b: Double) => b.toString, (a: Int) => a * 1.0)
        println(c(1))
    }
}