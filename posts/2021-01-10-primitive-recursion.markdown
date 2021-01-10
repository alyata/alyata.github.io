---
title: Primitive Recursion in Haskell
---

I'm currently working through the "Computability and Logic" textbook[^1], and
one of the chapters is on primitive recursive functions. Primitive recursive
functions are a model of computation where everything (and yes I mean
everything, including constants) is a function of the form $\mathbb{N}^k
\rightarrow \mathbb{N}$. You don't need to know much about how it works, except
that it is a very weird (and slightly excruciating) way of building functions,
which is perfect for a puzzle challenge!

I will give a brief overview of the building blocks in primitive recursion,
followed by an implementation of each construct in Haskell. Then, I will give
some examples of functions implemented using primitive recursion. Finally, there
will be some fun and games where you have to implement functions using my
Haskell implementation of primitive recursion. All the exercises are
shamelessly taken from the book.

# What is Primitive Recursion?

In primitive recursion (PR), you start out with a small set of primitive
functions, and have to compose them to build up more complex functions. 

## Primitive Functions

The first primitive function is

$$z(n) = 0$$

AKA the function that ignores the one input you give it, and returns a zero. As
you can see, this function looks very daft, as it's essentially just a constant,
but alas this is what we have. The next function should look more useful:

$$s(n) = n + 1$$

The successor function $s$ increments whatever single input it is given and
returns it. So now we have two primitive functions, but they all only take in
one input. However, remember that we're trying to build up functions that can
take in multiple arguments ($\mathbb{N}^k$). The way we do this is with our
third and last primitive function: $id$. More precisely, $id$ is actually a
family of functions that returns one of its inputs unmodified. The most basic
$id$ is the one that returns its one and only input:

$$id^1_1(x_1) = x_1$$

Next up is a pair of $id$s that returns either the first or the second input,
respectively:

$$\begin{aligned}
id^2_1(x_1, x_2) &= x_1 \\
id^2_2(x_1, x_2) &= x_2
\end{aligned}$$

and so on, *ad infinitum*. Or really, *ad nauseam* because we'll only be dealing
with functions with at most 4 arguments this time around.

So how does this look in Haskell? Before we start, the first thing we need to do
is make our own natural numbers data type, using the Peano definition:

```Haskell
data Nat = Zero | Succ Nat deriving Eq
```

So `Succ (Succ (Succ Zero))` would be `3`, for example. To make our lives
easier, we can also implement convenience functions which let us use integer
literals to define `Nat` constants, as well as print them as regular integers.

```Haskell
instance Num Nat where
  fromInteger 0 = Zero
  fromInteger x = if x > 0 then Succ $ fromInteger (x - 1) else undefined
  -- leave all the other instance functions undefined for now, we will implement
  -- them later using primitive recursion

λ> 3 :: Nat
Succ (Succ (Succ Zero))

-- not good enough, we also want to print Nats nicely
instance Show Nat where
  show n = show (toInt n)
    where
      toInt :: Nat -> Int
      toInt Zero     = 0
      toInt (Succ n) = succ (toInt n)

λ> 3 :: Nat
3
```

Now, I can give the definitions of the primitive functions. I tried to make it
as close to mathematical notation as possible, so all the functions are
uncurried and I write my parameters in brackets.

```Haskell
z :: Nat -> Nat
z(_) = 0

s :: Nat -> Nat
s(n) = Succ(n) -- notice that I don't use (+). It hasn't been defined!

id_1_1 :: Nat -> Nat
id_1_1(a) = a

id_2_1, id_2_2 :: (Nat, Nat) -> Nat
id_2_1(a, _) = a
id_2_2(_, b) = b

id_3_1, id_3_2, id_3_3 :: (Nat, Nat, Nat) -> Nat
id_3_1(a, _, _) = a
id_3_2(_, b, _) = b
id_3_3(_, _, c) = c

id_4_1, id_4_2, id_4_3, id_4_4 :: (Nat, Nat, Nat, Nat) -> Nat
id_4_1(a, _, _, _) = a
id_4_2(_, b, _, _) = b
id_4_3(_, _, c, _) = c
id_4_4(_, _, _, d) = d

-- ad nauseam...

-- use default id to mean id_1_1
id = id_1_1
```

Because they're primitive functions, their definitions are axiomatic and can be
defined using regular Haskell mechanics (pattern matching, data constructors.
etc). However, when defining other PR functions, you will have to strictly stick
to only using these three primitive functions and other PR functions you've
defined using the function combinators I'm about to explain below.

## Function Combinators

Now we want to combine the primitive functions to build up new functions. Here's
the catch - you have to combine functions using one of two really specific
function combinators. The first combinator is called Composition or just $Cn$
for short. Given a PR function $f(x_1, ..., x_k)$ and $k$ PR functions $g_1(x_1,
..., x_n), ..., g_k(x_1, ..., x_n)$, $Cn[f, g_1, ..., g_k]$ defines a new
function s.t.

$$Cn[f, g_1, ..., g_k](x_1, ..., x_n) = f(g_1(x_1, ..., x_n), ..., g_k(x_1, ...,
x_n))$$

With this, we can define our first family of non-primitive function: $const_n$.
Given a natural number $n$, $const_n(x)$ ignores its input and returns $n$. To
begin with, $const_0$ is really just a synonym for $z$:

$$const_0 = z$$

But we can use this to inductively define higher $const$ functions:

$$const_{n+1} = Cn[s, const_n]$$

The $const$ functions act as constants in a world where everything is a
function.

In Haskell, I implemented $Cn$ as a higher order function, where the "givens"
are supplied as part of an extra curried parameter:

```Haskell
cn :: ((Nat, ..., Nat) -> Nat, -- f
       (a -> Nat), ..., (a -> Nat)) -- g1, ..., gk
   -> (a -> Nat) -- the created function

```

Here, the `a` represents the polymorphic function input that could be any
$n$-tuple. Of course, you can always plug in something that is not a tuple,
but... please don't, thank you 🙂. Anyway, that's not the real problem here. The
real problem is that the number of $g$ functions depend on the number of inputs
f has - cn is a dependent function. The dumb solution, which I'm going to use,
is to instead define multiple `cn` functions which take in different number of
parameters.

```Haskell
cn1 :: (Nat -> Nat, a -> Nat) -> (a -> Nat)
cn1(f, g1) = \a -> f(g1(a))

cn2 :: ((Nat, Nat) -> Nat, a -> Nat, a -> Nat) -> (a -> Nat)
cn2(f, g1, g2) = \a -> f(g1(a), g2(a))

cn3 :: ((Nat, Nat, Nat) -> Nat, a -> Nat, a -> Nat, a -> Nat) -> (a -> Nat)
cn3(f, g1, g2, g3) = \a -> f(g1(a), g2(a), g3(a))

-- ad nauseam again...
```

With this, I can define $const$ as

```Haskell
const :: Nat -> Nat -> Nat
const Zero     = z
const (Succ n) = cn1(s, const n)
```

As before, I supply the given $n$ as an extra curried parameter. Because $n$ is
supposed to only be a constant, you are only allowed to plug in literals into
that first argument, e.g. `const 2`.

Next up is our second combinator, the titular Primitive Recursion, or $Pr$ for
short. This combinator creates an inductively defined function based on the
functions it is given. More specifically, given functions $f(x_1, ..., x_n)$
and $g(x_1, ..., x_n, y_1, y_2)$, $Pr[f, g]$ defines a function s.t.

$$\begin{alignedat}{2}
&Pr[f, g](x_1, ..., x_n, 0) &&= f(x_1, ..., x_n) \\
&Pr[f, g](x_1, ..., x_n, y + 1) &&= g(x_1, ..., x_n, y, Pr[f, g](x_1, ..., x_n, y))
\end{alignedat}$$

so $Pr[f, g]$ defines a function that is defined inductively on its last
input[^2]. It is important for two reasons:

1. It allows you to iterate over a given input.
2. It gives you a branching construct, i.e. do $f$ if $y$ is zero and do $g$
   otherwise.

To illustrate each point, I will give some examples. We will start by defining
addition inductively:

$$\begin{alignedat}{2}
&add(x, 0) &&= x \\
&add(x, y + 1) &&= s(add(x, y))
\end{alignedat}$$

Now we create functions $f$ and $g$ to represent the body of the function:

$$\begin{alignedat}{2}
&f(x) &&= x \\
&g(x, y_1, y_2) &&= s(y_2)
\end{alignedat}$$

In PR, we can represent these as

$$\begin{alignedat}{2}
&f &&= id^1_1 \\
&g &&= Cn[s, id^3_3]
\end{alignedat}$$

so I can define addition using the $Pr$ combinator:

$$add = Pr[f, g] = Pr[id^1_1, Cn[s, id^3_3]]$$

In this case we can think of $add(x, y)$ as the procedure "iterate from 0 to y,
adding 1 to x each time".

Another use case for $Pr$ is doing if-else branching. Suppose that we want to
implement the sign function

$$sgn(y) = 
\begin{cases} 
  0 & if \space y = 0 \\
  1 & otherwise 
\end{cases}$$

The first thing we need to do is put it in "inductive" form

$$\begin{alignedat}{2}
&sgn(0)     &&= 0 \\
&sgn(y + 1) &&= 1
\end{alignedat}$$

and then express the body as functions $f$ and $g$

$$\begin{alignedat}{2}
&f() &&= 0 \\
&g(y_1, y_2) &&= 1
\end{alignedat}$$

Wait what? $f$ has no inputs? Does that mean $f$ is a constant? We can see this
goes really wrong if we try to express it in PR. $f = const_0$ doesn't work
because $const_0$ has one input. It needs an input, even if its only to be
ignored. In fact, we have so far not encountered any way to construct constants,
just functions (with at least 1 input). The solution to this is somewhat of a
hack - we define a $Pr$ function with 2 inputs, but ignore the first (you can
see that ignoring inputs is a common pattern).

$$sgn'(x, y) =
\begin{cases} 
  0 & if \space y = 0 \\
  1 & otherwise 
\end{cases}$$

<p style="text-align:center;"> AKA </p>

$$sgn' = Pr[const_0, Cn[const_1, id^3_2]]$$

$sgn'$ is then used to define $sgn$:

$$sgn(x) = sgn'(x, x)$$

<p style="text-align:center;"> AKA </p>

$$sgn = Cn[sgn', id^1_1, id^1_1]$$

To make our lives easier, we define a new $Pr_1$ combinator based on $Pr$ that
applies this pattern. Given functions $f(x)$ and $g(y_1, y_2)$

$$Pr_1[f, g](x) = Pr[f, g'](x, x)$$

where $g'(x, y_1, y_2) = g(y_1, y_2)$. In purely PR notation, this is expressed
as

$$Pr_1[f, g] = Cn[Pr[f, Cn[g, id^3_2, id^3_3]], id^1_1, id^1_1]$$

Notice that while its possible to restrict $g$ from using the ignored input $x$,
its not possible to restrict $f$ from using $x$. As usual, please be a model
citizen and only stick to using $f = const_n$ functions.

Now, we can define $Pr$ in Haskell as we did with $Cn$, by defining `pr1`,
`pr2`, `pr3`, etc.

```Haskell
pr1 :: (Nat -> Nat, (Nat, Nat) -> Nat) 
    -> (Nat -> Nat)
pr1(f, g) = cn2(pr2(f, cn2(g, id_3_2, id_3_3)), id_1_1, id_1_1)

pr2 :: (Nat -> Nat, (Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat) -> Nat)
pr2(f, g) = h
  where
    h(x1, 0)      = f(x1)
    h(x1, Succ y) = g(x1, y, h(x1, y))

pr3 :: ((Nat, Nat) -> Nat, (Nat, Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat, Nat) -> Nat)
pr3(f, g) = h
  where
    h(x1, x2, 0)      = f(x1, x2)
    h(x1, x2, Succ y) = g(x1, x2, y, h(x1, x2, y))

pr4 :: ((Nat, Nat, Nat) -> Nat, (Nat, Nat, Nat, Nat, Nat) -> Nat)
    -> ((Nat, Nat, Nat, Nat) -> Nat)
pr4(f, g) = h
  where
    h(x1, x2, x3, 0)      = f(x1, x2, x3)
    h(x1, x2, x3, Succ y) = g(x1, x2, x3, y, h(x1, x2, x3, y))

-- ad nauseam as usual

-- use default pr to mean pr2
pr = pr2
```

And the implementations for $add$ and $sgn$:

```Haskell
add :: (Nat, Nat) -> Nat
add = pr(id, cn1(s, id_3_3))

sgn :: Nat -> Nat
sgn = pr1(const 0, cn1(const 1, id_2_1))
```

From now on, I won't give the "mathematical" PR definition of a function
anymore, as they look pretty much the same as the code.

# Some More Examples

To build up more intuition about primitive recursion, I will lay down some more
examples. These examples, along with `add`, `const n` and `sgn`, will form the
basis of a prelude which you can use to solve the exercises later.

## Example 1 - multiplication

We've seen before how to build the $sum(x, y)$ function as $y$ increments to
$x$. The multiplication function similarly can be described as repeated
additions.

$$\begin{alignedat}{2}
&mult(x, 0) &&= 0 \\
&mult(x, y + 1) &&= x + mult(x, y)
\end{alignedat}$$

This is easily transcribed to PR, remembering that in the recursive case of the
$Pr$ combinator, $id^3_1$ would be $x$ and $id^3_3$ would be the recursive call.

```Haskell
mult :: (Nat, Nat) -> Nat
mult = pr(const 0, cn2(add, id_3_1, id_3_3))
```

To go even further, exponentiation is just repeated $mult$s,
super-exponentiation is repeated exponentiation, etc.

## Example 2 - sum and product series

Sometimes we want to add or multiply the output of a given function $f$ up to a
certain input value.

$$\begin{alignedat}{2}
&sum[f](x, y) &&= \sum_{i = 0}^y f(x, i) \\
&prod[f](x, y) &&= \prod_{i = 0}^y f(x, i)
\end{alignedat}$$

This can be implemented once again by providing the given function as a curried
parameter:

```Haskell
sum :: ((Nat, Nat) -> Nat) -> ((Nat, Nat) -> Nat)
sum f = pr(cn2(f, id, z),
           cn2(add, id_3_3, cn2(f, id_3_1, cn1(s, id_3_2))))

prod :: ((Nat, Nat) -> Nat) -> ((Nat, Nat) -> Nat)
prod f = pr(cn2(f, id, z),
            cn2(mult, id_3_3, cn2(f, id_3_1, cn1(s, id_3_2))))
```

## Example 3 - logical operators

In order to represent boolean values, we use $1$ to represent $\mathtt{true}$
and $0$ for $\mathtt{false}$. The simplest boolean operator is negation, which
can quite simply be given as

$$neg(x) = \begin{cases}
1 & if \space x = 0 \\
0 & otherwise
\end{cases}$$

So, this will be another case of using $Pr$ not for iteration, but just
as a branching construct:


```Haskell
not :: Nat -> Nat
not = pr1(const 1, cn1(const 0, id_2_1))
```

Next, we can define conjunction and disjunction in terms of multiplication and
addition respectively, making sure to use $sgn$ to cast any positive result to a
$1$. I find it easier to try implementing a function in terms of other functions
because composition is easier to think about than primitive recursion.

$$\begin{alignedat}{1}
and(x, y) &= sgn(mult(x, y)) \\
or(x, y) &= sgn(add(x, y))
\end{alignedat}$$

This is rather elegant to implement using $Cn$, as you can see.

```Haskell
and :: (Nat, Nat) -> Nat
and = cn1(sgn, mult)

or :: (Nat, Nat) -> Nat
or = cn1(sgn, add)
```

# Exercises

As promised - here are the exercises. Each exercise will have you implementing 
one or more PR function, using only the primitive functions, function
combinators and functions defined in the prelude. You should also use functions
from previous exercises.

The files and solutions for the exercises can be found
[here](https://github.com/alyata/primitive-recursion).

## Exercise 1

Implement

- The difference function
  $$diff(x, y) = \begin{cases}
  x - y & if \space x > y \\
  0     & otherwise
  \end{cases}$$

- The ordering relation ($\leq$)
  $$leq(x, y) = \begin{cases}
  1 & if \space x \leq y \\
  0 & otherwise
  \end{cases}$$

- You might also want to define $\geq$, $<$, $>$, $=$ based on $\leq$ for
  convenience.

## Exercise 2

Implement

- the absolute difference function
  $$absdiff(x, y) = \begin{cases}
  x - y & if \space x > y \\
  y - x & otherwise
  \end{cases}$$

- the maximum function (and minimum, if you feel like it)
  $$max(x, y) = \begin{cases}
  x & if \space x > y \\
  y & otherwise
  \end{cases}$$

## Exercise 3

Implement integer division by defining the quotient and remainder functions. You
might find it easier to define the remainder function first and then use it in
your quotient function definition.

## Exercise 4

It's possible to define a pair-encoding function that assigns a unique natural
number to each possible pair of natural numbers. Implement one such function:

$$\begin{alignedat}{2}
pair(0, 0) &= 0 \\
\\
pair(0, 1) &= 1 \\
pair(1, 0) &= 2 \\
\\
pair(0, 2) &= 3 \\
pair(1, 1) &= 4 \\
pair(2, 0) &= 5 \\
\\
pair(0, 3) &= 6 \\
pair(1, 2) &= 7 \\
pair(2, 1) &= 8 \\
pair(3, 0) &= 9 \\
\dots
\end{alignedat}$$

This function can be more succintly defined as

$$pair(x, y) = \frac{(x + y)^2 + 3x + y}{2}$$

## Exercise 5

For this final exercise, define the pair-decoding functions $fst$ and $snd$. If
$pair(x, y) = n$ then $fst(n) = x$ and $snd(n) = y$. Consider defining a helper
function to help you define $fst$ or $snd$.

[^1]: Boolos, George & Burgess, John & Jeffrey, Richard. (2007). Computability 
      and Logic. 10.1017/CBO9780511804076.
[^2]: This means primitive recursive computations always terminate, so it is not
      turing complete.
