---
title: Type Inhabitation in Template Haskell
---

\usepackage{mathtools}
<!--Production Rule -->
\newcommand{\pr}[2]{
    #1 & ::= & #2\\
}
<!--% OR in production rule WITHOUT single trailing white space-->
\newcommand{\gor}{|} 

<!--% OR in production rule WITH single trailing white space-->
\newcommand{\gors}{\ |\ } 

<!--% Alias for \Rightarrow using Sipser[2013] the definition for yields -->
\newcommand{\yields}{\(\Rightarrow\)} 

<!--% Using Sipser[2013] definition for derives-->
\newcommand{\derives}{\(\xRightarrow[]{\star}\)} 

\newenvironment{grammar}[1]{ % grammar as an environment, 1: columns 2:data 3: derivations
    \begin{array}{r c l}
        #1
    \end{array}
}{}

\require{algpseudocode}

\renewcommand{\implies}{\rightarrow}
\newcommand{\bottom}{\perp}

Template Haskell allows us to write metaprograms, in the code-as-data sense of
programs that manipulate Haskell code (in syntax tree form, represented as
ADTs). In principle, this allows us to write a program that takes in the code of
a Haskell type, and finds a term that satisfies this type - this is the type
inhabitation problem. I will attempt to do exactly this, but only for the
fragment of Haskell that corresponds to the pure simply typed lambda calculus
with only type variables and function types:

$$
\begin{grammar}{
\pr{A, B}{\phi \gors A \implies B}
}\end{grammar}
\qquad
\begin{grammar}{
\pr{M, N}{x \gors \lambda x.M \gors M N}
}\end{grammar}
$$

$$
\begin{prooftree}
\AxiomC{}
\RightLabel{$(\textrm{Ax})$}
\UnaryInfC{$\Gamma, A \vdash A$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{$\Gamma, A \vdash B$}
\RightLabel{$(\implies \textrm{I})$}
\UnaryInfC{$\Gamma \vdash A \implies B$}
\end{prooftree}
\qquad
\begin{prooftree}
\AxiomC{$\Gamma \vdash A \implies B$}
\AxiomC{$\Gamma \vdash A$}
\RightLabel{$(\implies \textrm{E})$}
\BinaryInfC{$\Gamma \vdash B$}
\end{prooftree}
$$

# Basics of (Untyped) Template Haskell

We will be working with the untyped variant of Template Haskell where we can
build up program terms that won't type check. The typed variant is safer to work
with but more complicated (perhaps a future post). The key components of Template
Haskell are:
1. the `Exp`, `Type`, `Dec` and `Pat` data types which represent the syntax
   trees of expressions, types, function declarations and patterns (for pattern
   matching) respectively
2. the `quote` monad typeclass (concretely instantiated by the `Q`
   datatype) which encapsulates information about the syntax tree we are
   building, including auxiliary information such as variable counters for
   generating fresh variables. Given a term of type `Q Exp` for example we 
   can unquote it using the `$(...)` operator which during compilation is 
   replaced by the code built up in the `Q Exp` term we pass in.

Here's an example generating the n-argument `curry` function, 
courtesy of the [Haskell wiki](https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial):

```Haskell
{-# LANGUAGE TemplateHaskell #-}

module Curry where
import Control.Monad
import Language.Haskell.TH

-- given n, construct the lambda term that performs currying for n arguments
curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"                         -- newName generates a fresh name based on the
  xs <- replicateM n (newName "x")          -- given string by appending a counter number
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)   -- builds the code for \f x1 ... xn -> f (x1 ... xn)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where 
    mkCurryDec ith = do
      cury <- curryN ith
      let name = mkName $ "curry" ++ show ith          -- mkName verbatim converts the given 
      return $ FunD name [Clause [] (NormalB cury) []] -- string into a name, unlike newName
      -- builds the curry declarations curry15 = \f x1 ... x15 -> f (x1 ... x15)
```

As long as these functions are defined *in another module*, we can import them
and unquote the result of applying these functions:

```Haskell
import Curry

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 = $(curryN 4)

-- you can also unquote the list of declarations directly. This declares curry1,
-- curry2 ... curry100
$(genCurries 100)
```

# A Naive Attempt at Type inhabitation
There's a simple type inhabitation algorithm that we can start with, that finds
normal forms of a given type. Here is its pseudocode:

```
inhabitation Γ (A -> B) = λx. M
  where
    x is a fresh variable
    M = inhabitation (Γ, x:A) B

inhabitation Γ B = x M_1 ... M_n
    where
      x:A_1 -> ... -> A_n -> B ∈ Γ (for some n ≥ 0)
      M_i = inhabitation Γ A_i (for each 1 ≤ i ≤ n)
```

This algorithm does not terminate for certain types such as `(A -> A) -> A`: to
begin with, we add `x:A -> A` to the empty context and recursively attempt to
inhabit `A` with this new context. In attempting to do so, the only suitable
variable in the context is `x:A -> A`, which has `A` as its only input type.
Hence we must recursively attempt to inhabit `A` with the same context as
before, so we are stuck in an infinite loop. We hopefully can do better than
this, but its a good place to start.

To begin with, what should the type of the function be? For the context, we
represent this as a list of tuples mapping variable names to types. The function
is expected to return a program of the given type, but it may fail, so we should
return a `Q (Maybe Exp)`. However, dealing with nested monads is annoying so
instead, we go for the transformer `MaybeT Q Exp`.

```Haskell
inhabitation :: [(Name, Type)] -> Type -> MaybeT Q Exp
```

In the first case for types `A -> B`, we have a rather straightforward
translation from pseudocode:

```Haskell
inhabitation cxt (AppT (AppT ArrowT t1) t2) = do -- arrow type constructor is partially applied
  x <- lift $ newName "x"
  body <- inhabitation ((x, t1):cxt) t2
  return $ LamE [VarP x] body
```

In the second case, things are slightly more complicated, mostly due to having
to deal with the recursive structure of repeated arrows and lambda applications:

```Haskell
inhabitation cxt t = do
  (head, headType) <- MaybeT . return $ find correctReturnType cxt
  args <- mapM (inhabitation cxt) (getInputTypes headType)
  return $ foldl AppE (VarE head) args
  where
    -- This is used to check whether there is an x:A_1 -> ... -> A_n -> B ∈ Γ
    correctReturnType :: (Name, Type) -> Bool
    correctReturnType (x, AppT (AppT ArrowT _) ret)
      | t == ret  = True
      | otherwise = correctReturnType (x, ret)
    correctReturnType (_, ret) = t == ret

    -- This collects the input types A_1 ... A_n into a list so we can
    -- recursively map the inhabitation function, and fold the result into a
    -- repeatedly applied term
    getInputTypes (AppT (AppT ArrowT input) rest)
      = input : getInputTypes rest
    getInputTypes ret
      = []
```

With this, we can set up a convenience function to wrap around `inhabitation` by
converting the output into a `Q (Maybe Exp)` and actually setting up a
declaration.

```Haskell
-- this will give an exception if the inhabitation returned empty-handed
inhabit :: String -> Q Type -> Q [Dec]
inhabit name typ = do
  t <- typ
  term <- liftM fromJust $ inhabitation' t
  return $ [FunD (mkName name) [Clause [] (NormalB term) []]]

inhabitation' :: Type -> Q (Maybe Exp)
inhabitation' (ForallT _ _ t) = inhabitation t -- See below for why we need to unwrap foralls
inhabitation' t = runMaybeT $ inhabitation' [] t
```

`inhabit` should now allow us to construct function declarations built by type
inhabitation. However, we have to build up a `Type` syntax tree to pass in,
which is tedious. Luckily, there's a quasi-quoter feature of Template Haskell
that lets us convert regular typed code into syntax tree objects. This is best
shown by example:

```Haskell
[t|Int -> Int|] ≡ AppT (AppT ArrowT (ConT $ mkName "Int")) (ConT $ mkName "Int")

[t|a -> b -> a|] ≡ an error, 
because the type variables a and b occur free. The alternative is to either
build the syntax tree by constructors, or bind it using a forall and then later
remove the forall types (as I did in inhabitation').

[t|forall a. a -> a|] ≡ ForallT [PlainTV a_2 SpecifiedSpec] [] 
                        (AppT (AppT ArrowT (VarT a_2)) (VarT a_2))
```

Finally, this allows us to declare new functions by type inhabitation:

```Haskell
$(inhabit "i" [t|forall a. a -> a|])
$(inhabit "k" [t|forall a b. a -> b -> a|])
$(inhabit "s" [t|forall a b c. (a -> b -> c) -> (a -> b) -> a -> c|])
```

Because the declarations are not typed, the compiler will infer the principal
type from the program that was found, which leads to a curious effect:

```Haskell
$(inhabit "k'" [t|forall a. a -> a -> a|])
-- but ghci gives:
-- ghci> :t k'
-- k' :: p1 -> p2 -> p2
```

Attempting to inhabit types with constants such as `Int` will often fail unless
you populate the context with the built-in and prelude functions first, and of
course, `$(inhabit "y" [t|forall a. (a -> a) -> a|])` causes the compiler to
endlessly loop.

# The Ben-Yelles Type Inhabitation Algorithm
