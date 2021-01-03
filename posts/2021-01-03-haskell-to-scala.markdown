---
title: Learning Scala from a Haskell perspective
---

Next term, we will be doing a group compiler project. We've agreed on using
either Scala or Kotlin (we want a JVM language, just not Java). I'm leaning
towards Scala for its emphasis on functional programming, so this will be my
exploration of Scala where I note down interesting things. I will attempt to
make comparisons between Haskell and Scala concepts, and talk about where the
comparison breaks down too. This will mostly act as a reference for myself, but
hopefully other people might find it useful.

# SBT - Scala Build Tool

SBT is a project manager like Stack - it manages the Scala version and
dependencies to be used by a project, as well as provide an interface for
compiling, running and testing your project. Here are the commands and their
stack versions (though they're mostly the same):

SBT         | Stack
---         | -----
sbt clean   | stack clean
sbt compile | stack build
sbt test    | stack test
sbt run     | stack run
sbt console | stack ghci

# Values and Variables

`val` identifies an immutable variable (value) whereas `var` identifies a
mutable one:

```Scala
val x = 1 //immutable
x = 10 //illegal

var y = 0 //mutable
y = 2 //legal
```

so sticking to `val` would be good for doing pure functional programming ala
Haskell. Here, types are inferred from the values contained in the variables.
However, both variable types can be explicitly typed if desired:

```Scala
val x: Int = 1
var y: String = "hello"
```

Typed `val`s correspond very closely to regular typed Haskell expressions, since
they cannot be reassigned (although it might still be possible to assign a
mutable object to a val - in which case its not immutable "all the way down").

```Haskell
x :: Int
x = 1
```

On the other hand, `var`s may be reassigned. The new value must have a type
matching the variable, which might be implicitly derived from the original 
value.

```Scala
var y = 1
y = "hello"

error: type mismatch;
 found   : String("hello")
 required: Int
```

The type of a var is fixed upon declaration, but if you allow Scala to
infer the type then it will infer the most specific type possible (it seems).

# Control Structures

All control structures in Scala return a value [^1], because Scala is an
expression oriented language. This allows for them to be used flexibly either as
"pure" values, or as imperative-style constructs (in which case the `unit` type
is returned, signifying a non-meaningful value). Although, you can always be
evil by including side-effects **and** return a value at the same time.

## If-Else & Case expressions

As a first example of flexibility, we can use if-else ala Haskell to define
a value.

```Scala
// Note to self: remember to bracket your conditional and not use a "then"
val x = if (y < 10) y else "too big"
```

The type of `x` will be inferred to be `Any`, the most specific type
supertyping both `Int` and `String`.  If both alternatives had the same type
(e.g. `Int`), then `x` will be of type `Int`.

However, we can also use if-else as an imperative top-level construct:

```Scala
// Use brackets to run multiple expressions in body
// last expression in brackets is the one returned
if (y < 10)
  println(y)
else {
  println("This number is:")
  println("too big")
}
```

Although we don't use it, this returns the `unit` type as printing returns no
meaningful value.

This flexibility also applies for case expressions, which uses the keyword
`match`:

```Scala
// Use it like Haskell case expressions
val x = y match {
  case 1 | 2 | 3 => "small"
  case 4 | 5 | 6 => "medium"
  case _ => "large"
}

// Or use it imperatively
y match {
  case 1 | 2 | 3 => println("small")
  case 4 | 5 | 6 => println("medium")
  case _ => println("large")
}
```

One additional feature is to match different subtypes on each case. This is not
possible in Haskell where there is no subtyping. To do this, just add a type
annotation on the pattern match:

```Scala
y match {
  case n: Int => n + 1
  case str: String => str + " + 1"
  case x => x // otherwise, don't modify it
}
```

## Loops

While loops are purely imperative: they always return the `unit` type.  With the
exception of having a (vacuous) return value, they seem to just be good ol' Java
while loops.

```Scala
while (y < 10) {
  println(y)
  y += 1
  // This expression is ignored, while always evaluates to Unit
  y
}
```

In Scala, the only for loops are foreach loops. They also return the unit type,
although they can pattern match on collection elements unlike Java foreach
loops:

```Scala
val numberPairs = List((1,2),(3, 4))

for ((left, right) <- numberPairs) println(s"$left VS $right")
```

At first glance, this pattern matching notation (with the arrow and all that)
really screams **list comprehension**. Lo and behold, we can use foreach loops
as list comprehensions by adding the keyword `yield`:

```Scala
val numberPairs = List((1,2),(3, 4))

for ((left, right) <- numberPairs) yield left + right
```

This will return a collection of `left + right`s. And if you really want to be
evil? Do other stuff inside the yield before returning:

```Scala
for ((left, right) <- numberPairs) yield {
  println(left)
  println(right)
  x += lef≡t
  left + right
}
```

## For loops and do notation

It turns out, the list comprehension-y feel of for expressions makes a lot of
sense. This is because they are just syntactic sugar for a series of calls to
the following methods:

- `List[A].map[B]: (A => B) => List[B]`
- `List[A].flatMap[B]: (A => Iterable[B]) => List[B]`
- `List[A].filter: (A => Boolean) => List[A]`
- `List[A].foreach: (A => Any) => Unit`

I'm not sure if these are the actual types of the methods (or if they're even
syntactically correct), but I think they carry the concept just fine. These
functions are encapsulated in the Iterable trait, which means any class
extending the trait can be for looped (notice that the type of `flatMap` is the
same as `(>>=)`). You can also just implement these methods without becoming an
iterable. Anyway, here are [some
examples](https://stackoverflow.com/questions/1052476/what-is-scalas-yield/1059501#1059501)
of desugaring that I found on StackOverflow.

```Scala
desugar[for(x <- c1; y <- c2; z <-c3) {...}]
  ≡ c1.foreach(x => c2.foreach(y => c3.foreach(z => {...})))

desugar[for(x <- c1; y <- c2; z <- c3) yield {...}]
  ≡ c1.flatMap(x => c2.flatMap(y => c3.map(z => {...})))

desugar[for(x <- c; if cond) yield {...}]
  ≡ c.filter(x => cond).map(x => {...})

desugar[for(x <- c; y = ...) yield {...}]
  ≡ c.map(x => (x, ...)).map((x,y) => {...})
```

In particular, notice the parallel between the second example and Haskell's
do-notation:

```
for(
  x <- c1
  y <- c2
  z <- c3
) yield {...}

do
  x <- c1
  y <- c2
  z <- c3
  return ...
```

and their desugarings:

```Scala
c1.flatMap(x => c2.flatMap(y => c3.map(z => {...})))

c1 >>= (x -> c2 >>= (y -> c3 >>= (z -> return ...)))
```

These are just monads! They're even exactly the same length! Although the
condition does seem to be stronger as filter is required as well. From these
examples, I tried to piece together the desugaring mechanism:

```Scala
desugar[for (a <- ma; b1; b2; ...; bk; ...) {...}] 
  ≡ ma.desugar[b1]
      .desugar[b2]
      ...
      .desugar[bk]
      .foreach((a, c1, ..., cj) => desugar[for(...) {...}])
```

where each `b` has to match either `c = expr` or `if cond`. `c1, ..., cj` is the
list of assigned variable `c`s that appear in the `b`s. Now, here's how `b`s are
desugared:

```Scala
// in this context, c1 .. cj are any previous assignments
desugar[c = expr] ≡ map   ((a, c1, ..., cj) => (a, c1, ..., cj, expr))
desugar[if cond]  ≡ filter((a, c1, ..., cj) => cond)
```

We also need a base case:

```Scala
desugar[for (a <- ma; b1; b2; ...; bk) {...}]
  ≡ ma.desugar[b1]
      .desugar[b2]
      ...
      .desugar[bk]
      .foreach((a, c1, ..., cj) => {...})
```

For loops that `yield` require the use of `flatMap` and `map` instead.

```Scala
desugar[for (a <- ma; b1; b2; ...; bk; ...) {...}] 
  ≡ ma.desugar[b1]
      .desugar[b2]
      ...
      .desugar[bk]
      .flatMap((a, c1, ..., cj) => desugar[for(...) {...}])

desugar[for (a <- ma; b1; b2; ...; bk) yield {...}]
  ≡ ma.desugar[b1]
      .desugar[b2]
      ...
      .desugar[bk]
      .map((a, c1, ..., cj) => {...})
```

# Functions VS Methods

In Scala, functions are values but methods are not. Functions can be defined
using anonymous functions 

```Scala
val add = (x: Int, y: Int) => x + y

// If you want a curried version:
val add = (x: Int) => (y: Int) => x + y
```

or placeholder syntax.[^2]

```Scala
val add = (_: Int) + (_: Int)
```

On the other hand, methods are defined using the `def` keyword. However, they
cannot be assigned to a `val` or `var` as they are not values.

```Scala
def addM(x: Int, y: Int) = x + y
val add = addM

error: missing argument list for method addM
       Unapplied methods are only converted to functions when a function type is
       expected.
       You can make this conversion explicit by writing `addM _` or `addM(_,_)` 
       instead of `addM`.
```

As mentioned in the error message, methods cannot be assigned to variables, but
they can be converted to functions. This conversion can be done by either adding
an underscore in front of the function (`addM _`) or by applying the method to
placeholders. The former method is called
[η-expansion](https://tpolecat.github.io/2014/06/09/methods-functions.html). We
can see how it works by checking the type:

```Scala
:type addM _
(Int, Int) => Int

// Curried version:
def addM(x: Int)(y: Int) = x + y
:type addM _
Int => (Int => Int)
```

When passing in a method to a higher-order function, it will automatically
expand it for you without having to use an underscore.

# Traits and the diamond inheritance problem

Traits are like a combination of Java interfaces (or Haskell typeclasses) and
Java abstract classes. They contain a set of fields and methods that a class
inheriting the trait should have. A field/method can be given a default
implementation. F

[^1]: This is not technically correct as using `return` is specifically reserved
  for functions, but its nice to think about it like that.

[^2]: I have to explicitly type the placeholders as there's no context for their
  type here. When passing a placeholdered expression to a higher-order function,
  Scala should be able to infer the placeholders' types, removing the need for
  this.

