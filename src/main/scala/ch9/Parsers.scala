package ch9

/**
  * Created by cyq on 28/11/2016.
  */

trait JSON
object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
}


//case class ParseError()

trait Parsers[Parser[+_]] { concreteInstance => //self is used widely

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A) (implicit f: A => Parser[String]) : ParserOps[String]
    = ParserOps(f(a))

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    def char(c: Char): Parser[Char] ={
        string(c.toString).map(_.charAt(0))
    }

    def orString(s1: String, s2: String): Parser[String]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def many[A](p: Parser[A]): Parser[List[A]]

    def map[A, B](a: Parser[A]) (f: A => B): Parser[B]

    def slice[A](p: Parser[A]): Parser[String]



    case class ParserOps[A](p: Parser[A]) {
        def | [B>:A] (p2: Parser[B]): Parser[B] = concreteInstance.or(p, p2)
        def or [B>:A] (p2: Parser[B]): Parser[B] = concreteInstance.or(p, p2)
        def map[B](f: A=>B): Parser[B] = concreteInstance.map(p)(f)
        def many = concreteInstance.many(p)
        def slice: Parser[String] = concreteInstance.slice(p)
    }

    case class ParseError()

    object Laws {
        import ch8._
        def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
            Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
        }

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
            equal(p, p.map(a => a))(in)
        }
    }
}

object Parsers {

}

object ParsersMain extends App {

    println("hello parsers")

    val P : Parsers[String]

    println(P.char('c'))

}
