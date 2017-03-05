package com.thoughtworks

import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class templateSpec extends FreeSpec with Matchers {

  @template
  def plus(x: Any, y: Int) = x + y

  plus(2, 3) should be(5)
  plus("2", 3) should be("23")

  "plus(2, ???)" should compile
  """plus(2, "not a Int")""" shouldNot typeCheck

  @template
  def plus2(x: Any, y: Any): String = x + y

  plus2(2, "m") should be("2m")
  plus2("$", 3) should be("$3")

  // Disable this line because ScalaTest does not support it.
  // "plus2(2, 3)" shouldNot compile

  "HList should index by Int literal" in {
    import templateSpec._
    val hlist = "foo" :: 1 :: false :: HNil

    hlist(0) should be("foo")
    hlist(1) should be(1)
    hlist(2) should be(false)
    "hlist(3)" shouldNot typeCheck
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
