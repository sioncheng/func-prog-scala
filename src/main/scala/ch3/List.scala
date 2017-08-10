package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def setHead[A](newHead: A, xs: List[A]):List[A] = xs match {
		case Nil => Nil
		case Cons(x, xs) => Cons(newHead, xs)
	}

	def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
		case Nil => l2
		case Cons(h, t) => Cons(h, append(t, l2))
	}

	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
		def dropWhileInner(remains: List[A], rest: List[A], f: A => Boolean) : List[A] = 
		rest match {
			case Nil => remains
			case Cons(x, xs) if f(x) => dropWhileInner(remains, xs, f)
			case Cons(x, xs) if !f(x) => dropWhileInner(append(remains, List(x)), xs, f)
		}

		dropWhileInner(Nil, l, f)
	}

	def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	def reverse[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => foldLeft(xs, List(x))((b,a) => append(List(a), b))
	}

	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = List.foldLeft(List.reverse(as), z)((x,y) => f(y,x))

	def foldSum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)
	def foldProduct(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
	def length[A](ns: List[A]):Int = foldLeft(ns, 0)((x,y) => x + 1)

	def tail[A](as: List[A]): List[A] = {
        drop(as, 1)
    }

    def drop[A](as: List[A], n: Int): List[A] = {
        if (n <= 0) {
            as
        } else {
            as match {
                case Nil => Nil
                case Cons(_, xs) => drop(xs, n-1)
            }
        }
    }
}

object ListMain extends App {

	val ex1: List[Double] = Nil
	val ex2: List[Int] = Cons(1, Nil)
	val ex3: List[String] = Cons("a", Cons("b", Nil))
	val ex4: List[String] = List("a", "b", "c")
	val ex5: List[String] = List.setHead("aa", ex4)
	val ex6: List[String] = List.dropWhile(ex5)(x => "b" == x)
	var ex7: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

	println(ex1)
	println(ex2)
	println(ex3)
	println(ex4)
	println(ex5)
	println(ex6)
	println(List.foldProduct(ex1))
	println(List.foldSum(ex2))
	println(List.length(ex1))
	println(List.length(ex2))
	println(List.length(ex3))
	println(List.reverse(ex3))
	println(List.foldLeft(ex7,1)(_ - _))
	println(List.foldRight(ex7,1)(_ - _))
    println(List.tail(ex7))
    println(List.drop(ex7,1))
}