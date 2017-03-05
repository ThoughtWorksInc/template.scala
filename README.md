# template.scala

**template.scala** is a library for creating template functions, similar to C++ templates.

## Usage

A template function is created with a `@template` annotation.

``` scala
@template
def max(x: Any, y: Any) = {
  if (x > y) x else y
}
```

Unlike normal functions, a template function will not be type-checked until using it. Thus it does not raise a type error on `x > y` because the real types of `x` and `y` have not been determinated.

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

