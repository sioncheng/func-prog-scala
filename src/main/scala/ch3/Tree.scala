package ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {

    def size[A](t: Tree[A]): Int = {

        def go(acc: Int): Int = {
            t match {
                case Leaf(_) => acc + 1
                case Branch(x,y ) => size(x) + size(y)
            }
        }

        go(0)
    }

    def maximum(t: Tree[Int]): Int = {

        def go(m: Int): Int = {
            t match {
                case Leaf(v) => if (m > v) m else v
                case Branch(l, r) => {
                    val lm = maximum(l)
                    val rm = maximum(r)
                    if (lm > rm) lm else rm
                }
            }
        }

        go(Int.MinValue)
    }

    def depth[A](t: Tree[A], target: A, f:(A, A) => Boolean): Int = {

        def go(tt: Tree[A], acc: Int): (Boolean, Int) = {
            tt match {
                case  Leaf(x) => {
                    if (f(x, target)) {
                        (true, acc)
                    } else {
                        (false, acc)
                    }
                }
                case Branch(l, r) => {
                    val go1 = go(l, acc + 1)
                    val go2 = go(r, acc + 1)
                    if (go1._1) {
                        go1
                    } else {
                        if (go2._1) {
                            go2
                        } else {
                            (false, acc + 1)
                        }
                    }
                }
            }
        }

        val gg = go(t, 0)
        if (gg._1) {
            gg._2
        } else {
            0
        }
    }

    def map[A, B](t: Tree[A], f: A => B): Tree[B] = {

        def go(tt: Tree[A]): Tree[B] = {
            tt match {
                case Leaf(x) => Leaf(f(x))
                case Branch(l, r) => {
                    val lt = go(l)
                    val rt = go(r)
                    Branch(lt, rt)
                }
            }
        }

        go(t)
    }

    def fold[A, B](t: Tree[A], init: B, f: Tree[A] => B, g: (B,B) => B) = {

        def go(tt:  Tree[A], acc: B): B = {
            tt match {
                case Branch(l, r) => g(acc, g(f(l), f(r)))
                case x => g(acc, f(x))
            }
        }

        go(t, init)
    }

    def mapByFold[A, B](t: Tree[A], f: A => B): Tree[B] = {
        val init = null
        def ff (a: Tree[A]): Tree[B] = {
            a match {
                case Branch(l, r) => {
                    Branch(ff(l),ff(r))
                }
                case Leaf(x) => Leaf(f(x))
            }
        }
        val gg: (Tree[B], Tree[B]) => Tree[B] = (x,y) =>  {
            x match {
                case null => y
                case _ => Branch(x,y)
            }
        }

        fold(t, init, ff, gg)
    }
}

object TreeMain extends App {
    println(Tree.size(new Leaf[Int](1)))

    val tree = Branch[Int](Branch[Int](Leaf(1), Leaf(3)), Branch[Int](Leaf(4), Leaf(5)))
    println(Tree.maximum(tree))
    println(Tree.depth[Int](tree, 2, _ == _))
    println(Tree.depth[Int](tree, 3, _ == _))

    val tree2 = Tree.map[Int, Double](tree, (x: Int) => x * 1.0)
    println(tree2)

    val tree3 = Tree.mapByFold[Int, Double](tree, (x: Int) => x * 1.0)
    println(tree3)
}
