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

}
