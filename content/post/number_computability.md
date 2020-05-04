+++
title = "Number Theory Meets Computability Theory"
description = "Some notes on the history and origins of the Lisp programming language."
date = "2019-09-29"
categories = ["computability theory","number theory","polynomials","Hilberts 10 Problem"]
mmark = true
draft = true
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
whose solution (if it exists) continues to elude the many number theorists who are actively working on this and other related equations involving *sums of three cubes* of the form $x^{3} + y^{3} + z^{3} = a$. As before, it suffices to find a single solution for variables $x,y,z$, however searching the infinite space of integers, especially in the absence of a broader mathematical theory, can easily lead one astray.

We can also ask seemingly more complicated follow-up questions, such as whether the specific equation below (known as the *Ljundgrenn* equation):
$$
\begin{align}
x^{2} - 2y^{4} + 1 = 0
\end{align}
$$
not only has a solution, but a fixed set of unique solutions (the Norwegian mathematician Wilhelm Ljundgreen, after whom the equation is named, proved in 1942 that this equation indeed only has the following two  solutions: $(x=1,y=1)$ and $(x=239,y=13)$). We can also ask whether *no solutions exist* for a given equation under certain conditions, for example whether it is true that the following equation (or family of equations):
$$
\begin{align}
x^{n} + y^{n} = z^{n},
\label{eq:fermat}
\end{align}
$$
has *no integer solutions* for any $n>2$, as was first conjectured by Pierre Fermat in the 17th century and proved by Andrew Wiles in 1994\footnote{The story behind this conjecture is likely to be the most repeated anecdote in mathematics. Fermat had apparently scribbled this conjecture in 1637 into the margins of Diophantus' \emph{Arithmetica} and claimed that he had \emph{discovered a truly remarkable proof }that was too complex to fit in the margins. As mentioned above, the ultimate proof didn't arrive until over 350 years later.}. Answering questions of this type, which are commonly found in number theory proper, often require considerable amounts of mathematical sophistication and ingenuity. 
