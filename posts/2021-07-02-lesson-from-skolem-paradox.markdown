---
title: A simple lesson from Skolem's "paradox"
---

Here is a lesson that I learned the hard way.

Skolem's paradox is the apparent mismatch between the following:

  1. *Within* $\mathbf{ZFC}$, we may show that nonenumerable sets exist, 
     such as $\mathbb{R}$. So, there must be uncountably many sets in 
     $\mathbf{ZFC}$. Let's call this the "internal view", as it is a theorem
     within $\mathbf{ZFC}$.

  2. $\mathbf{ZFC}$ is a first order theory. Therefore, by the Löwenheim-Skolem
     theorem, it has a model $\mathfrak{M}$ with an enumerable domain, i.e.
     there are only countably many sets in $\mathbf{ZFC}$. Let's call this the
     "external view" as it is a theorem about $\mathbf{ZFC}$.

So the paradox is the seeming contradiction between the internal view and
external view of $\mathbf{ZFC}$. At first glance, I took this to mean something
was deeply wrong with first-order logic or $\mathbf{ZFC}$ or both. But upon
further reading, it became clear that this paradox is a fake one, and I fell for
it hook, line and sinker.

To discredit this faux paradox, I will point out and resolve the ambiguity of
both the internal and external views.

Let's start by tackling the internal view. $\mathbf{ZFC}$ is a formal theory, a
collection of sentences composed of symbols. It is **not** a model. Hence, the phrase
"there must be uncountably many sets in $\mathbf{ZFC}$" is, by itself,
meaningless. To read a meaning out of the phrase, you must implicitly assume an
interpretation/model of the symbols in $\mathbf{ZFC}$, a personal bias of what the
symbols mean to **you**, which is that $\mathbf{ZFC}$ is a theory of pure sets.
Let's call this implicit interpretation $\mathfrak{S}$. In this interpretation,
the internal view holds.

Now in the second point, we invoked the Löwenheim-Skolem theorem to explicitly
put forward a different, maybe even esoteric interpretation $\mathfrak{M}$ with
an enumerable domain. Calling the objects in this domain "sets", the
external view must hold of $\mathfrak{M}$.

But as the internal and external views are statements about different models,
they are therefore mutually independent. No paradox.

The real problem is this: we constructed $\mathbf{ZFC}$ to precisely capture
just the notion of pure sets (which has to be nonenumerable) yet the
Löwenheim-Skolem theorem means it also captures additional unwanted notions. We
cannot change the first order theory we use as the Löwenheim-Skolem theorem
applies just as well to any theory. So first order logic must be incapable of
exactly capturing the notion of pure sets. But this is a different problem
altogether for another day.

**TL;DR:** The lesson is that formal theories are just symbols - don't read too
much meaning into what the symbols mean.


