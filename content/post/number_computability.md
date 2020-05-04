+++
title = "Number Theory Meets Computability Theory"
description = "Some notes on the history and origins of the Lisp programming language."
date = "2019-09-29"
categories = ["computability theory","number theory","polynomials","Hilberts 10 Problem"]
mmark = true
draft = false
+++


The Problem
-------------------------

In this article, we consider the problem of solving certain types of equations (called *polynomial equations*). For example, imagine you are given the equation
$$
\begin{align}
4 - x^{2} = 0
\end{align}
$$
and asked to do the following: determine if there exists a solution for the variable $x$ in integers. Since it is easy to come up with a single solution in this case, namely $x=2$, we can use this specific solution to answer this question in the affirmative. In contrast, it should be clear that a slightly modified equation such as $2 - x^{2} = 0$ *does not have a solution*, which can easily be checked by hand. 

Of course, not all equations of this form are as straightforward to solve. If we allow for a few additional variables and slightly larger constants, we quickly stumble upon innocent looking equations such as the following
$$
\begin{align}
(x^{3} + y^{3} + z^{3}) = 114,
\end{align}
$$
whose solution (if it exists) continues to elude the many number theorists who are actively working on this and other related equations involving *sums of three cubes* of the form $x^{3} + y^{3} + z^{3} = a$.
