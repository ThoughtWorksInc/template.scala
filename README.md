# template.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Join the chat at https://gitter.im/ThoughtWorksInc/template.scala](https://badges.gitter.im/ThoughtWorksInc/template.scala.svg)](https://gitter.im/ThoughtWorksInc/template.scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/template.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/template.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/template.scala/template/latest.svg)](https://index.scala-lang.org/thoughtworksinc/template.scala/template)

**template.scala** is a library for creating inline functions, similar to C++ templates.

## Usage

``` sbt
scalaVersion := "2.12.1" // or "2.11.8"

libraryDependencies += "com.thoughtworks.template" %% "template" % "latest.release" % Provided

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full)
```

A template function is created with a `@template` annotation.

``` scala
@template
def max(x: Any, y: Any) = {
  if (x > y) x else y
}
```

Unlike normal functions, a template function will not be type-checked until using it. Thus it does not raise a type error on `x > y` because the real types of `x` and `y` have not been determined.

``` scala
val i: Int = max(1, 2)
val d: Double = max(8.0, 0.5)
```

The `max` function will be type-checkd and inlined whenever being invoked.

If the type of `x` does not support `>` method, it does not compile:

``` scala
val s: Symbol = max('foo, 'bar)
```

```
<macro>:1: value > is not a member of Symbol
def max(x: Any, y: Any) = if (x > y) x else y
                                ^
```

## Side effects

By default, the side effects in arguments of `@template` functions will be evaluated before the execution of the function body. For example:

``` scala
max({
  println("x = 1")
  1
}, {
  println("y = 2")
  2
})
```

The output is

``` scala
x = 1
y = 2
```

However, you can use call-by-name parameter to force the side effects re-evaluate whenever the argument is referenced. For example:



``` scala
@template
def callByNameMax(x: => Any, y: => Any) = {
  if (x > y) x else y
}

callByNameMax({
  println("x = 1")
  1
}, {
  println("y = 2")
  2
})
```

The output is

``` scala
x = 1
y = 2
y = 2
```


## Recursive template functions

Template functions can be recursive, as long as the number of calls are finite and can be determined at compile-time.

The following code creates a heterogeneous list.

``` scala
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
```

Then you can index elements in the HList via template function `apply`.

``` scala
val hlist = "foo" :: 1 :: false :: HNil

val s: String = hlist(0)
val i: Int = hlist(1)
val b: Boolean = hlist(2)

hlist(3) // Compile error
```

Note that the above `HList` example need [TypeLevel Scala](http://typelevel.org/scala/) and [`-Yliteral-types`](http://docs.scala-lang.org/sips/pending/42.type.html) flag.

## Limitations

* By default, `@template` functions are inline, not sharing similar implementations like C++ templates.
* `@template` functions do not support type parameters. You can create type aliases instead. See [the test case](https://github.com/ThoughtWorksInc/template.scala/blob/53121bc7dd42de0b933a1b2ef6ac882b4d06c12d/src/test/scala/com/thoughtworks/templateSpec.scala#L120) for example.
* Recursive `@template` functions must be resolved at compile-time.
