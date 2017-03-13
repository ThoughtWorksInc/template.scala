package com.thoughtworks

import com.thoughtworks.templateSpec.Point2
import org.scalatest.{FreeSpec, Matchers}

import scala.language.existentials

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class templateSpec extends FreeSpec with Matchers {

  @template
  def max(x: Any, y: Any) = {
    if (x > y) x else y
  }

  max(1.0, 2.0) should be(2.0)
  max(10, 5) should be(10)

  "max('foo, 'bar)" shouldNot typeCheck

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
      "plus2(1.0, 3.5)" shouldNot typeCheck
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

  "call-by-name parameter should be called more than once" in {
    @template
    def duplicate(element: => Int): (Int, Int) = {
      (element, element)
    }

    {
      var seed = 0
      duplicate {
        val id = seed
        seed += 1
        id
      }
    } should be((0, 1))
  }

  Point2(1, 3).sumComponents() should be(4)
  Point2(1, 3) + Point2(100, 200) should be(Point2(101, 203))
  Point2(1, 3) restrictedPlus Point2(100, 200) should be(Point2(101, 203))
  Point2(1.5, 0.3) + Point2(100, 200) should be(Point2(101.5, 200.3))
  "Point2(1.5, 0.3) restrictedPlus Point2(100, 200)" shouldNot typeCheck
  Point2(1.5, 0.3) returnRestrictedPlus Point2(100.0f, 200.0f) should be(Point2(101.5, 200.3))
  "Point2(100.0f, 200.0f) returnRestrictedPlus Point2(1.5, 0.3)" shouldNot compile

  Point2(Point2(0.1, 0.2), Point2(1.0, 2.0)) + Point2(Point2(10, 20), Point2(100, 200)) should be(
    Point2(Point2(10.1, 20.2), Point2(101.0, 202.0)))
}

object templateSpec {

  sealed trait HList {

    final def ::(head: Any): head.type :: this.type = {
      new (head.type :: this.type)(head, this)
    }

  }

  case object HNil extends HList

  final case class ::[Head, Tail <: HList](head: Head, tail: Tail) extends HList {
    def apply(i: 0): head.type = {
      head
    }

    @template
    def apply(i: Int with Singleton): Any = {
      tail(i - 1)
    }

  }

  case class Point2[T](x: T, y: T) {
    type A = T

    @template def restrictedPlus(rhs: Point2[A]): Point2[A] = Point2[A](x + rhs.x, y + rhs.y)

    @template def returnRestrictedPlus(rhs: Point2[_]): Point2[A] = Point2[A](x + rhs.x, y + rhs.y)

    @template def +(rhs: Point2[_]): Point2[_] = Point2(x + rhs.x, y + rhs.y)

    @template def sumComponents() = x + y
  }

}
