package ch5

/**
  * Created by chengyongqiao on 16/11/2016.
  */
sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }



    def toList(): List[A] = {

        def toListInner[A](stream: Stream[A])(acc: List[A]): List[A] = {
            stream match {
                case Empty => acc
                case Cons(h, t) => toListInner(t())(acc ::: List(h()))
            }
        }
        toListInner(this)(Nil)
    }

    def take(n: Int): List[A] = {

        def takeInner[A](stream: Stream[A])(n: Int)(an: Int, acc: List[A]): List[A] = {
            stream match {
                case Empty => acc
                case Cons(h, t) if(an < n) => takeInner(t())(n)(an+1, acc ::: List(h()))
                case _ => acc
            }
        }

        takeInner(this)(n)(0, Nil)
    }

    def drop(n: Int): Stream[A] = {
        def dropInner[A](stream: Stream[A])(n: Int)(an: Int): Stream[A] = {
            stream match {
                case Empty => Empty
                case Cons(h, t) if(an < n) => dropInner(t())(n)(an+1)
                case _ => stream
            }
        }

        dropInner(this)(n)(0)
    }

    def dropWhile(p: A => Boolean): Stream[A] = {
        //dropWhileInner is not simple, there should be a way to make it more simple
        def dropWhileInner[A](stream: Stream[A])(p: A => Boolean)(acc: Stream[A]): Stream[A] = {
            stream match {
                case Empty => acc
                case Cons(h, t) => {
                    if (p(h())) {
                        dropWhileInner(t())(p)(acc) //drop head
                    } else {
                        acc match {
                            case Empty => dropWhileInner(t())(p)(Stream.cons(h(),acc))
                                //re-assembly
                            case Cons(h1, t1) => dropWhileInner(t())(p)(Stream.cons(h1(), Stream(t1().toList() ::: List(h()): _*) ))
                        }
                    }
                }
            }
        }

        dropWhileInner(this)(p)(Empty)
    }

    def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty[B])( (h, t) => Stream.cons(f(h), t) )

    def filter(f: A => Boolean): Stream[A] =
        foldRight(Stream.empty[A])( (h, t) => if (f(h)) Stream.cons(h,t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
        foldRight(s)((h,t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty[B])((h,t) => f(h) append t)

    def tails: Stream[Stream[A]] = {
        Stream.unfold(this)(s => s match {
            case Empty => None
            case s => Some((s, s drop 1))
        }).append(Stream(Stream.empty))
    }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*) : Stream[A] = {
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    def constant[A](a: A): Stream[A] = {
        lazy val tail: Stream[A] = Cons(() => a, () => tail)
        tail
    }

    val ones:Stream[Int] = constant(1)

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    val fibs: Stream[Int] = {
        def fibsInner(a: Int, b: Int): Stream[Int] = {
            Stream.cons(a, fibsInner(b, a+b))
        }

        fibsInner(0,1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((h,s)) => cons(h, unfold(s)(f))
            case None => empty
        }
    }

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))
}


object StreamMain extends App {

    val st = Stream(1,2,3)

    println("headOption")
    println(st.headOption)

    println("toList")
    println(st.toList())

    println("take(n)")
    println(st.take(0))
    println(st.take(2))
    println(st.take(20))

    println("drop(n)")
    println(st.drop(0).toList())
    println(st.drop(1).toList())
    println(st.drop(10).toList())

    println("dropWhile")
    println(st.dropWhile(_ % 2 == 0).toList())
    println(st.dropWhile(_ % 1 == 0).toList())
    println(st.dropWhile(_ > 10).toList())
    println(st.dropWhile(_ < 10).toList())

    println("exists")
    println(st.exists(_ == 3))
    println(st.exists(_ > 4))

    println("forAll")
    println(st.forAll( _ > 0))
    println(st.forAll( _ > 10))

    println("map")
    println(st.map(_ * 2).toList())

    println("filter")
    println(st.filter(_ % 2 == 0).toList())

    println("append")
    val st2 = Stream(5,6,7)
    println(st.append(st2).toList())

    println("constant")
    println(Stream.ones.take(10))

    println("from")
    println(Stream.from(10).take(5))

    println("fibs")
    println(Stream.fibs.take(10))

    println("fromViaUnfold")
    println(Stream.fromViaUnfold(10).take(5))

    println("tails")
    println(st.tails.toList())
}