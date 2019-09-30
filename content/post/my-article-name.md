+++
title = "What is Kolmogorov Complexity?"
description = "A brief note on Kolmogorov complexity."
date = "2014-09-29"
categories = [ "undecidability", "probability" ]
mmark = true
+++

The Basic Idea
-------------------------

I've been reading about Kolmogorov complexity, with the aim of understanding certain metamathematical results (including general incompleteness). I seem to be on the verge of understanding specifically Chaitin's reformulation of Godel's results using the Berry paradox (which, interestingly, was first published by Bertrand Russell but named after a librarian in Oxford named Berry who first articulated the problem to Russell). First, what is Kolmogorov complexity (or Kolmogorov-Chaitin-Solomonoff complexity; so-called after the 3 individauals who independently discovered it)? Assuming a given string (or string rendering of a particular problem), the Kolmogorov complexity of that string is the size of the smallest program that is needed for generate that string. For example, let's assume that we have the following two strings of size `n=8`
```
x  = 01010101 
x' = 11101001
```
In the first case, a reasonably compact Python program for generating $x$ is the following:
```python
print("01"*4)
```
which, excluding the Python parts `",print, (,), *}` and the prefix `"01"` (since this pattern will stay constant for all such patterns longer than `n=8`), the size of this program reduces the size of the number `4` (or `1/n`), which in bits is  equivalent to the following (using the laws of number-bit conversion; this part about the conversion tripped me up a bit at first):
$$ \left\lfloor \log_{2}(4) \right\rfloor + c $$
where $$c (\ge 1)$$ is the constant that covers the programming language parts and the constant pattern `01`. 


<!--more-->


