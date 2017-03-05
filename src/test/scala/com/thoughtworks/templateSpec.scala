package com.thoughtworks

import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class templateSpec extends FreeSpec with Matchers {

  @template
  def plus(x: Any, y: Int with Singleton) = x + y

  plus(2, 3) should be(5)
  plus("2", 3) should be("23")

  "`Int with Singleton` parameter" - {
    "should accept `???`" in {
      "plus(math.random, ???)" should compile
    }
    "should not accept non-stable expression `math.random.toInt`" in {
      "plus(math.random, math.random.toInt)" shouldNot compile
    }
    "should not accept String" in {
      """plus(2, "not a Int")""" shouldNot typeCheck
    }
  }

  "A template function returns a String" - {
    @template
    def concat(x: Any, y: Any): String = x + y

    "should compile for String concatenation" in {
      concat(2, "m") should be("2m")
      concat("$", 3) should be("$3")
    }

    "should not compile for Double" in {
      // Disable this line because ScalaTest does not support it.
       "plus2(1.0, 3.5)" should compile
    }
  }

  "HList" - {
    "should index by Int literal" in {
      import templateSpec._
      val hlist = "foo" :: 1 :: false :: HNil

      hlist(0) should be("foo")
      hlist(1) should be(1)
      hlist(2) should be(false)
    }
    "should not accept out of bound index" in {
      "hlist(3)" shouldNot typeCheck
    }
  }
}
object templateSpec {

  sealed trait HList {

    final def ::(head: Any): head.type :: this.type = {
      new (head.type :: this.type)(head, this)
    }

  }

  case object HNil extends HList

  final case class ::[Head, Tail](head: Head, tail: Tail) extends HList {
    def apply(i: 0): Head = {
      head
    }

    @template
    def apply(i: Int with Singleton): Any = {
      tail(i - 1)
    }

  }

}
