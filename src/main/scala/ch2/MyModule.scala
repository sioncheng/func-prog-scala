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

	def main(args: Array[String]): Unit = {
		println(formatResult("absolute value", -34, abs))
		println(formatResult("factorial", -34, abs))
        for (n <- 1 to 10) {
            println(fib(n))
        }
	}
}