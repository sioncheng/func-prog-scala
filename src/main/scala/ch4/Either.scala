package ch4

/**
  * Created by cyq on 2016/11/11.
  */

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: =>Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap(aa => b map (bb => f(aa, bb)))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherModule {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list") else Right(xs.sum / xs.length)
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

}

object EitherMain extends App {
  val ex1 = IndexedSeq(1.0,2,3,4)
  val ex2 = IndexedSeq()
  println(EitherModule.mean(ex1).orElse(Right(200)))
  println(EitherModule.mean(ex2).orElse(Right(100)))
  println(EitherModule.safeDiv(4,2).map(_ * 2))
  println(EitherModule.safeDiv(2,0).map2(Right(2.0))(_ * _))
  println(EitherModule.safeDiv(4,2).flatMap(EitherModule.safeDiv(_, 0)))
}
