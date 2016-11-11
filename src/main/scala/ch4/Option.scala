package ch4

/**
  * Created by cyq on 2016/11/11.
  */

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
    def map[B](f : A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def flatMap[B](f : A => Option[B]) : Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    def getOrElse[B >: A] (default: => B) : B = this match {
        case None => default
        case Some(a) => a
    }

    def orElse[B >: A] (ob: => Option[B]) : Option[B] = this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this match {
        case None => None
        case Some(a) => if (!f(a)) None else Some(a)
    }
}

object Option {
    def Try[A](a: => A): Option[A] = {
        try Some(a)
        catch { case e : Exception => None}
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object SeqModule {
    def mean(xs: Seq[Double]) : Option[Double] = {
        if (xs.isEmpty) None else Some(xs.sum / xs.length)
    }
}

case class Employee(name: String, department: String)

object Employee {
    def lookupByName(name: String): Option[Employee] = {
        if ("jack" == name) Some(Employee("jack", "it")) else None
    }

    def joeDepartment: Option[String] = lookupByName("joe").map(_.department)
}

object Insurance {

    def insuranceRateQuote(age: Int, numberOfSpeedingTicks: Int): Double = age * 2.0 + numberOfSpeedingTicks * 4.0

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTicks: String): Option[Double] = {
        val optAge: Option[Int] = Option.Try(age.toInt)
        val optTicks: Option[Int] = Option.Try(numberOfSpeedingTicks.toInt)

        optAge match {
            case None => None
            case Some(a) => optTicks match {
                case None => None
                case Some(b) => Some(insuranceRateQuote(a, b))
            }
        }

    }

}

object OptionMain extends  App {
    println(SeqModule.mean(Nil))
    println(SeqModule.mean(List(1,2,3)))

    println(Some(2).map(_ * 2))
    val nilInt: Option[Int] = None
    println(nilInt.map(_ * 2))

    def fmf(x: Int): Option[Double] = {
        if (x % 2 == 0) Some(x * 2.0) else None
    }

    println(Some(2).flatMap(fmf))

    println(nilInt.getOrElse(1213))
    println(Some(2).getOrElse(1213))

    println(nilInt.orElse(Some(1)))
    println(Some(2).orElse(Some(1)))

    println(nilInt.filter(_ % 2 == 0))
    println(Some(2).filter(_ % 2 == 0))
    println(Some(3).filter(_ % 2 == 0))

    println(Employee.lookupByName(""))
    println(Employee.lookupByName("jack"))
    println(Employee.joeDepartment)

    println(Insurance.parseInsuranceRateQuote("20", "4"))
    println(Insurance.parseInsuranceRateQuote("20", "dsf"))
}