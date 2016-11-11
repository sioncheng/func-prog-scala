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

	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	/*def foldRightInner[A, B](as: List[A], z: B)(f: (A, B) => B)(acc: B) = as match {
		case Nil => acc
		case Cons(x, xs) => 
	}*/

def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???

	def foldSum(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
	def foldProduct(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
}

object ListMain extends App {

	val ex1: List[Double] = Nil
	val ex2: List[Int] = Cons(1, Nil)
	val ex3: List[String] = Cons("a", Cons("b", Nil))
	val ex4: List[String] = List("a", "b", "c")
	val ex5: List[String] = List.setHead("aa", ex4)
	val ex6: List[String] = List.dropWhile(ex5)(x => "b" == x)

	println(ex1)
	println(ex2)
	println(ex3)
	println(ex4)
	println(ex5)
	println(ex6)
	println(List.foldProduct(ex1))
	println(List.foldSum(ex2))
}