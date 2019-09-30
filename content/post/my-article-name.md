+++
title = "What is Kolmogorov Complexity?"
description = "A brief note on Kolmogorov complexity."
date = "2014-09-29"
categories = [ "undecidability", "complexity" ]
mmark = true
+++

The Basic Idea
-------------------------

I've been reading about Kolmogorov complexity, with the aim of understanding certain metamathematical results (including general incompleteness). I seem to be on the verge of understanding specifically [Chaitin's reformulation](https://arxiv.org/pdf/chao-dyn/9406002.pdf) of Gödel's results using the [Berry paradox](https://en.wikipedia.org/wiki/Berry_paradox).

First, before getting to the paradox let's  define Kolmogorov complexity (or Kolmogorov-Chaitin-Solomonoff complexity; so-called after the 3 individauals who independently discovered it).  Assuming a given string (or string rendering of a particular problem), the Kolmogorov complexity of that string is defined as  the **size of the smallest program** that is needed for generate that string. For example, let's assume that we have the following two strings of size $n=8$:
$$
x  = 01010101 \\ 
x' = 11101001
$$
In the first case, a reasonably compact Python program for generating $x$ is the following:
```python
print("01"*4)
```
which, excluding the Python parts `",print, (,), *}` and the prefix `"01"` (since these wil stay constant for all such patterns longer than $n=8$), the size of this program reduces the size of the number $4$ (or $n/2$), which in bits is  equivalent to the following (using the laws of [number-bit conversion](https://www.exploringbinary.com/number-of-bits-in-a-decimal-integer/); this part about the conversion tripped me up a bit at first, but is essential for the result in the next Section):
$$ \left\lfloor \log_{2}(4) \right\rfloor + c $$
where $c (\ge 1)$ is the constant that covers the programming language parts and the constant pattern `01`.

Now what about the second string $x'$? Clearly, this pattern is more complex, in that sense that it is difficult to come up with a more compact  program than the following:
```python
print("11101001")
```
which simply returns the total pattern `"11101001"`, and is therefore of size equal to the size of the input (for now, forget the point about the $log_{2}$ bit conversion). 

With these ideas in mind, we can then quantify the notion of a random string as one whose smallest program is greater than or equal to the size of the string plus this constant $c$ (in other words, we cannot find a program smaller than the one that simply encodes and returns the full string). We can further define the set $R$ of all random strings as follows (where $K(x)$ stands for `Kolmogorov complexity`):

$$R = \bigg\\{ x \mid K(x) \ge | x | \bigg\\}$$

I find this definition of randomness to be very satisfying. In the simplest terms, it says that a string is random if we can't come up with a clever pattern to describe or generate it. To me it is very easy to imagine many such strings, though this notion of randomness turns out to be fundamentally problematic, as described next. 


The Result 
-------------------------

Now we can ask the question: can we come up with a general algorithm to find this set $R$ and determine if a given $x$ is random?  Somewhat shockingly, it turns out we can't according to the following (it took me some time to find a readable proof; this one is based on this blog post [here](https://jeremykun.com/tag/kolmogorov-complexity/), where as similar set of examples is used as above, the notes [here](http://theory.stanford.edu/~trevisan/cs154-12/notek.pdf) and [this wonderful  textbook](https://www.springer.com/gp/book/9781489984456), which I'm still working through): 

**Theorem** $R$ is not decidable. 

**Proof**. Let's imagine a computer program $M$ (e.g., a Turing machine, Python program, whatever), that computes $R$. If such a program exists, then we can use it for all possible inputs in sorted order  (we can further represent this program $M$ as a string of size $| M |^{\texttt{bits}}$, where $| \cdot |^{\texttt{bits}}$ is the size in of program string in bits, as discussed above).

To find if a particular string $x \in R$ (where $|x|=n$), we can then use another program $M'$ that will do a look up on $R$ for all strings of size $n$ and return **true** if it finds the input $x$. Imagine that we represent this latter program as a string $s\_{n}$ built from a string rendering of $M'$  (of size $| M' |^{\texttt{bits}}$) concatenated with the number $n$ (where again, the bit representation of $| n |^{\texttt{bits}}$ is approximately  $log_{2}(n)$). In other words:
$$| s\_{n} |^{\texttt{bits}} =  | M |^{\texttt{bits}} + | n |^{\texttt{bits}}$$
The problem is that according to our definition of randomness, the following must hold:
$$K(s\_{n}) \ge n $$
whereas $s\_{n}$ will have a length of $c + \log\_{2} n$. Therefore, we have the following:
$$n \le \log\_{2}(n) + c$$
which cannot hold for most $n$. Therefore $R$ cannot be computed in the general case.


Why Does This Happen?
-------------------------




<!--more-->


