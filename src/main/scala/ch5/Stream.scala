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
}