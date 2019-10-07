+++
title = "Why Infinity is Strange"
description = "A brief note on some results on infinity"
date = "2019-10-06"
categories = [ "math","infinity","set theory","cantor" ]
mmark = true
draft = false
+++

Infinite Sets
-------------------------

I think I finally understand [Cantor](https://en.wikipedia.org/wiki/Georg_Cantor)'s proof about non-countable (or
non-enumerable) sets (I've been very slowly working through Boolos and
Jeffrey's
[Computability and Logic](https://www.goodreads.com/book/show/1556746.Computability_and_Logic);
I find their notation and general approach to be quite hard to follow,
so it's been a struggle).

This particular result has to do with infinity, which is a
conceptually difficult topic to grasp, even when considered
non-mathematically. Part of what Cantor discovered is that infinite
sets can have different cardinalities (where **cardinality** here
corresponds the number of items in a given set). For example,  the cardinality of the natural (or counting) numbers
$\mathbb{N}$ is much smaller than that of the real numbers
$\mathbb{R}$ (we won't talk directly about the real number, but instead about the **power set** of the natural numbers $\mathcal{P}(\mathbb{N})$,
which has the same cardinality as $\mathbb{R}$). More generally, Cantor showed that
infinity is not a singular concept, and that there are in fact **many different
types of infinity** (which, at first glance, is a bewildering proposition). 

Let's start with some definitions. Somewhat informally, we will call
an **enumerable set** as any set  that can be aligned
to a subset of the natural counting numbers (e.g., the set of chairs
in my kitchen, of which there are 4). A **countably infinite
set**, then, is a set that can be aligned to all of the natural
counting numbers. For example, let's imagine we have a simple formal language
$\mathcal{L} = \{ a^{*} \}$; we can argue that the string set
associated with this language is countably infinite by doing the following
alignment (such that the number of $a$s in each string is mapped
**uniquely** to each associated counting number):

$$\lambda  \to 0, a \to 1, aa \to 2, aaa \to 3, aaaa \to 4, ...$$

More formally, you can define an infinite set as a function from
values in that set to the natural numbers, e.g.,

$$f : \mathcal{L} \to \mathbb{N}$$

To use some terminology
that will become useful later, we will say that for an infinite set to
be countably infinite, there must exist some *bijective* relation, or
*one-to-one* mapping, between the given set and  the natural numbers (i.e.,
for each item $x$ in the **domain** of $f$, which in this case would would be our infinite set,
there should exist a single item in **co-domain** $f(x) \in
\mathbb{N}$ and vice versa).

With these ideas, let's now prove a relatively simple, yet puzzling, theorem about countably infinite sets. 

**Theorem 1** For any two countably infinite sets $A$ and $B$ (for
  convenience, we will assume that $A$ and $B$ are disjoint, i.e., $A
  \cap B = \emptyset$), the
  union of $A$ and $B$, $A \cup B$, is also countably infinite. 
  
**Proof** (rough sketch following the discussion [here](https://math.stackexchange.com/questions/49758/proving-that-a-union-of-countably-infinite-sets-is-countably-infinite)) Given that $A$ and $B$ are countably
  infinite, we know that there exist two bijective functions $f : A
  \to \mathbb{N}$ and $f' : B \to \mathbb{N}$. To prove the theorem,
  we need to come up with a new bijective  function $g : (A \cup B) \to \mathbb{N}$. Below defines such
  a function:

$$
g(x) =
\begin{cases}
2f(x) & \text{if }x \in A, \\\\\\
2f'(x)+1 & \text {if }x \in B
\end{cases}
$$

which simply assigns items in $A$ to the **even numbers** (of which there
are a countably infinite amount) and items in $B$ to **odd numbers** (of
which there also are a countably infinite amounnt)  □

Now why is this result puzzling? Well, typically when we combine two
of anything, the result is larger than the
individual pieces we combined. For example, if I combine the set of chairs in my
kitchen with the set of chairs in your kitchen (assuming that you have chairs), the resulting set is the sum of our
two sets and hence is larger. Countable infinity doesn't work this way; when you combine
two countably infinite sets, the resulting set has exactly the same
cardinality. 

In the 17th century, Galileo had several observations about
infinity, even without the  set theoretic machinery that was
developed by Cantor much later in the 19th century. What he noticed
is that if you can create a one-to-one mapping between square numbers (i.e., the numbers that
are the product of other natural numbers, e.g., $0,1,4,9,16,...$) and the natural
numbers (this is similar in spirit to what we did in Theorem 1). More
technically, he made the observation that the function  $ f : n \to n^{2} $ is
bijective for all $n \in \mathbb{N}$.  This led him to say the following about infinity in his book [*Two New Sciences*](https://en.wikipedia.org/wiki/Two_New_Sciences): 

> This is one of the difficulties which arise when we attempt, with
> our finite minds, to discuss the infinite, assigning to it those
> properties which we give to the finite and limited; but this I think
> is wrong, for we cannot speak of infinite quantities as being the  one greater or less than or equal to another. [He then goes on to discuss his result on square numbers].
    
As a mathematical matter, it turns out that his last
point about how we cannot speak of infinite quantities as being smaller or
greater than one another is not quite right. Even though countably
infinite sets are all equal to one another, there are infinities with
larger cadinalities. This is the result that we turn to next. 

Cantor's Theorem
-------------------------

Cantor's Theorem is about the relative cardinality of sets and their
[power sets](https://en.wikipedia.org/wiki/Power_set), as mentioned  at the beginning. More specifically, the
theorem states that the cardinality of a given set $A$ is strictly
less than  its power set (i.e., the set of all sets constructed from
$A$, plus the empty set $\emptyset$). Below states this more formally (where $|\cdot|$
denotes the cardinality of a set, and $\mathcal{P}(\cdot)$ denotes the
power set): 

**Cantor's Theorem** $$\text{For any set } A, |A| \lt | \mathcal{P}(A)|$$

**The Finite case**: Notice that we didn't specify whether the set $A$ is finite or
infinite, meaning that this result applies to both.  For finite sets, this is straightforward to prove; if you
attempt to construct a power set from a given set of cardinality $n$, you will notice
that the cardinality of the resulting power set will always be $2^{n}$
(it's encouraged that you try to do this yourself). For example, imagining the set $A = \\{ a ,
b\\}$ of size 2, the power set is the following: 

**Example 1** $$\mathcal{P}(A) = \\{ \\{ a \\} , \\{ b \\} , \\{ a , b \\} , \emptyset  \\} $$

(note the existence of the empty set $\emptyset$, which is always a
subset of every set). Since $n > 2^{n}$, clearly $2^{n}$ is always going to be larger than
$n$ for any $n \in \mathbb{N}$ (the full
proof requires a bit more work, but it relates to what comes next
related to proving this for infinite sets).

**The Infinite case**: The more suprising part of Cantor's theorem is
that this applies to infinite sets, e.g., for a countably infinite
set such as the set of natural numbers: 

$$| \mathbb{N} | \lt | \mathcal{P}(\mathbb{N})  |$$

To demonstrate this, we will do the following: first, we will start with a
definition of a set that is **undeniably** in $\mathcal{P}(\mathbb{N})$,
then show that if we try to assign it to a specific natural number to
this set  (as is required to make the mapping **bijective**), we will run into a
contradiction. Let's imagine that we were to map the natural numbers
to its power set. What we would have  are individual
mappings such as those shown below: 
$$
0 \to \\{ 0, 1 \\} \\\\\\
1 \to \\{ 3, 4 \\} \\\\\\
2 \to \\{ 5, 6 \\} \\\\\\
3 \to \\{ 3, 5 \\} \\\\\\
... \to ...
$$
Notice that some of the sets on the right side contain the numbers on
the to which they are indexed (i.e., for $0$ and
$3$,  these numbers are included in their corresponding sets $\\{ 0,1 \\}$ and $\\{ 3,
5\\}$), whereras the others do not. Based on this, let's make the following
definition: 

**Definition** Let's  define the function $d : \mathbb{N} \to
\mathcal{P}(\mathbb{N})$, and the  set D of all numbers that are not included in their corresponding set: 
$$
D = \bigg\\{ x \mid x \in \mathbb{N} \land  x \notin d(x) \bigg\\}
$$
Using again the example mapping above, $D$ would include the numbers $1$ and
$2$. Since $D$ is a set of positive numbers, **it clearly must
appear in the set** $\mathcal{P}(\mathbb{N})$. This definition, it
turns out, leads to a contradiction, which makes it impossible for $d$
to be bijective. Somewhat unconventionally, we will state this fact as
a lemma:

**Lemma 1** 


