+++
title = "Number Theory Meets Computability Theory"
description = "Some notes on the history and origins of the Lisp programming language."
date = "2020-04-03"
categories = ["computability theory","number theory","polynomials","Hilbert's 10 Problem", "diophantine equations"]
mmark = true
draft = false
+++

![where is the image](https://www.nlp-kyle.com/files/cantor_infinity.jpeg)


Solving Equations
-------------------------

In this article, we consider the problem of solving certain types of equations (called [*polynomial equations*](https://mathworld.wolfram.com/Polynomial.html)). For example, imagine you are given the equation
$$
\begin{align}
4 - x^{2} = 0
\end{align}
$$
and asked to do the following: determine if there exists a solution for the variable $x$ in integers. Since it is easy to come up with a single solution in this case, namely $x=2$, we can use this specific solution to answer this question in the affirmative. In contrast, it should be clear that a slightly modified equation such as $2 - x^{2} = 0$ *does not have a solution*, which can easily be checked by hand. 

Of course, not all equations of this form are as straightforward to solve. If we allow for a few additional variables and slightly larger constants, we quickly stumble upon innocent looking equations such as the following[^1]
$$
\begin{align}
(x^{3} + y^{3} + z^{3}) = 114,
\end{align}
$$
whose solution (if it exists) continues to elude the many number theorists who are actively working on this and other related equations involving [**sums of three cubes**](https://www.quantamagazine.org/why-the-sum-of-three-cubes-is-a-hard-math-problem-20191105/) of the form $x^{3} + y^{3} + z^{3} = a$. As before, it suffices to find a single solution for variables $x,y,z$, however searching the infinite space of integers, especially in the absence of a broader mathematical theory, can easily lead one astray.[^2]

We can also ask seemingly more complicated follow-up questions, such as whether the specific equation below (known as the *Ljundggren* equation):
$$
\begin{align}
x^{2} - 2y^{4} + 1 = 0
\end{align}
$$
not only has a solution, but a fixed set of unique solutions (the Norwegian mathematician [*Wilhelm Ljundggren*](https://en.wikipedia.org/wiki/Wilhelm_Ljunggren), after whom the equation is named, proved in 1942 that this equation indeed only has the following two  solutions: $(x=1,y=1)$ and $(x=239,y=13)$). We can also ask whether *no solutions exist* for a given equation under certain conditions, for example whether it is true that the following equation (or family of equations):
$$
\begin{align}
x^{n} + y^{n} = z^{n},
\label{eq:fermat}
\end{align}
$$
has *no integer solutions* for any $n>2$, as was [*first conjectured by Pierre Fermat*](https://mathworld.wolfram.com/FermatsLastTheorem.html) in the 17th century and proved by [*Andrew Wiles*](https://en.wikipedia.org/wiki/Andrew_Wiles) in 1994[^3]. Answering questions of this type, which are commonly found in number theory proper, often require considerable amounts of mathematical sophistication and ingenuity.

Rather than focusing on solving specific equations and coming up with specialized solutions, as number theorists tend to do, the main focus of this article will be on a larger and much more grandiose question, namely: does there exist a universal solution for solving arbitrary (polynomial) equations? In other words, **can we automate the process of equation solving and devise a universal algorithm** that can determine, given any polynomial equation regardless of its number of variables or its difficulty, whether it has a solution? 

**The Search for a Universal Equation Solver** A version of this last question was asked by the German mathematician [*David Hilbert*](https://en.wikipedia.org/wiki/David_Hilbert) in 1900, and is problem number 10 of 23 in the famous [**Hilbert Problems**](https://en.wikipedia.org/wiki/Hilbert%27s_problems) (we examine the exact phrasing of his question in the next section). In the words of [*Martin Davis*](https://en.wikipedia.org/wiki/Martin_Davis_(mathematician)), such problems were among the premier mathematical problems *that the nineteenth century [had] left for the twentieth century to solve*. Given the long history of work on polynomial equation solving, dating back to Diophantus in the 3rd century AD and even before, the idea of a universal algorithm is not only grandiose but, as Davis once wrote, utopian.

The 20th century did solve this problem, though the solution likely would have baffled and annoyed David Hilbert, who had dreamed of reducing all of higher mathematics to a definitive set of formal axioms and algorithmic principles.  In 1970, the Russian mathematician [*Yuri Matiyasevich*](https://en.wikipedia.org/wiki/Yuri_Matiyasevich), building on the work of a rather eclectic and tenacious group of American researchers that includes [*Martin Davis*](https://en.wikipedia.org/wiki/Martin_Davis_(mathematician)), [*Julia Robinson*](https://en.wikipedia.org/wiki/Julia_Robinson) and [*Hilary Putnam*](https://en.wikipedia.org/wiki/Hilary_Putnam),  provided the final piece in the proof that ultimately lead to the following *negative solution*:  **no such universal method or algorithm exists for solving arbitrary polynomial equations** (or more specifically, what are called *diophantine equations*). A large part of the final proof involves a rather astonishing and unexpected link between number theory and basic concepts from computer science and *computability theory*.

While this result closely relates to other *impossibility* results discovered in the early days of computer science by [*Kurt Gödel*](https://en.wikipedia.org/wiki/Kurt_G%C3%B6del), [*Alonzo Church*](https://en.wikipedia.org/wiki/Alonzo_Church), [*Alan Turing*](https://en.wikipedia.org/wiki/Alan_Turing), [*Emil Post*](https://en.wikipedia.org/wiki/Emil_Leon_Post)[^4] and others, it is not at all obvious at first sight that Hilbert's 10th problem is a problem that computer science would have much to say about. One might have thought that any algorithmic solution to the general problem of solving (polynomial) equations would  rely solely on principles from number theory and arithmetic. This turns out not to be the case and one of the big technical ideas to come out of this work is that one can link notions from computer science about *computable* or *recursive* sets with sets of solutions to equations.


To see how this works, consider the general form of the three cube problem shown below (which we might think of as denoting the *family* of all the three cube equations):
$$
\begin{align}
x^{3} + y^{3} + z^{3} = a.
\label{eq:cube}
\end{align}
$$
Solutions to this equation (i.e., particular values $a$ for which there exist solutions for the variables) can then be represented as sets of the following form:
$$
\begin{align}
S = \Big\\{ a \mid x^{3} + y^{3} + z^{3} = a  \text{ holds for some integers $x,y,z$}\Big\\},
\end{align}
$$
Asking whether a three square equation has solutions for a particular $a$ (e.g., 114) in this setting then reduces to asking whether $a$ is in the set $S$ (e.g., $114 \in S$?). Similarly, asking whether there are a unique set of solutions, or whether no solutions exist, involves asking questions about the size and scope of $S$ (is $S$ the empty set? is $S$ of size $k$?,...). When formulated in this way, the problem quickly starts to look like the types of problems encountered in theoretical computer science.

At its heart, Hilbert's 10th problem is what we now call a [*decision problem*](https://en.wikipedia.org/wiki/Decision_problem); what we want to prove is that there exists an algorithm that can decide (i.e., return a yes/no answer) whether a given equation has a solution or not, or equivalently whether a number is included in a set of solutions. Based on the work of Alonzo Church and Alan Turing, who were among the first to study modern decision problems, we know that some problems can be *undecidable*; that is, it can be proven that no such algorithm can exist no matter how hard one tries. This is what happens in the case of Hilbert's 10th problem: it is possible to define hypothetical sets of solutions for which it is provably impossible to build an algorithm that can decide set-membership. As a consequence, this undecidability proves that a general algorithm cannot exist (the surprising part is that we can talk about such sets without having to say very much about their corresponding equations, and in a way that entirely side-steps the practical issues involved with *explicitly* finding its members).

Using the language of computability theory, we can specifically say the following: **any positive solution to Hilbert's 10th problem (i.e., proof that a universal algorithm exists for solving arbitrary diophantine equations) would imply a positive solution to the Halting Problem** of [Turing (1936)](https://www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf), which is the most well-known undecidable problem in computer science. The goal of this article is explain what this means and to provide enough of the technical computer science background that is needed to sketch out this fascinating result.[^5]

Diophantine Equations and Sets
-------------------------

First, let's consider Hilbert's original description of the problem:

>Given a **Diophantine equation** with *any number of unknown quantities*
> and with rational integral numerical coefficients:
> To **devise a process** according to which it can be determined
> in a finite number of operations **whether the equation is solvable in rational integers**.

We will consider each part of this problem statement in turn. Hilbert's notion of a *process ..[involving] a finite number of operations* is what we would now call an **algorithm**, which was a rather fuzzy concept in 1900 (we will talk about algorithms in the next section). By *diophantine equation*, Hilbert's is referring to certain types of polynomial equations that characterize most of the equations we have considered so far.  A *polynomial* in our case will mean the following:

**Definition 1** A **polynomial** expression over $n$ variables/*unknown quantities* $x\_{1},x\_{2},..,x\_{n}$, denoted as $p(x\_{1},...,x\_{n})$, is any finite sum of *monomials*, or expressions of the form:
$$
cx\_{1}^{k\_{1}},...,x\_{n}^{k\_{n}},
$$
where $c$ is an integer *coefficient* (i.e., positive and negative integers and zero, denoted as $\mathbb{Z}$) and $k\_{1},...,k\_{n}$ are natural numbers including zero (denoted as $\mathbb{N}$).

Examples polynomial expressions include $x\_{1}^{2} - 4x\_{1} + 3$ (where, for convenience, subtraction is used in place of addition with a negative number, $+ -4x$), $4x\_{1}^{3} + 6x\_{2}$, $x\_{1}+x\_{2} + ... + x\_{4}$ (with all coefficients $c$ equal to 1) and so on.[^6] When talking about polynomials it is important to specify the range of their variables. Diophantine equations are special types of polynomial equations that restrict the range of variables in the manner specified below.

**Definition 2** A **diophantine equation** is specific type of polynomial expression $p(x\_{1},...,x\_{n}) = 0$ (also known as a *polynomial equation* in *traditional form*) restricted to **integer** unknowns $x\_{1},...,x\_{n}$ (or what Hilbert calls *rational integers*).

We will show momentarily that it suffices to modify the problem such that variables are restricted to natural numbers, which is a inconsequential variant of Hilbert's original description. We also note that it is sometimes easier to transform diophantine equations out of their **traditional form** $p(\cdot) = 0$ into equations of the following type:
$$
\begin{align}
p\_{l}(x\_{1},...,x\_{n}) = p\_{r}(x\_{1},...,x\_{n})
\end{align}
$$
where $p\_{l}$ and $p\_{r}$ are two separate diophantine equations defined over the same variables. For example, transformations of this type become convenient when we want to remove negative terms in an equation, which we might do with the following non-trivial diophantine equation:
$$
4x^{3}y - 2x^{3}z^{3} - 3y^{2}x + 5z = 0,
$$
to arrive at:
$$4x^{3}y + 5z = 2x^{2}z^{3} + 3y^{2}x
$$
by transposing the negative terms.


As briefly discussed in the last section, one of the big ideas is to define diophantine equations with additional variables called **parameters** $a$, as in $p(a,x\_{1},x\_{2},...,x\_{n})$ (where $x\_{1},...,x\_{n}$ continue to be what we previously called *unknowns*}), which allow us to describe abstract **families of diophantine equations**. This gives rise to an important concept called a **diophantine set**.

**Definition 3**  A **diophantine representation** of a diophantine equation $p$ with integer *unknowns* $x\_{1},...,x\_{n}$ and a *parameter* $a$[^7] is the set (which we will henceforth call a **diophantine set**):
$$
\begin{align}
S = \Big\\{ a \mid \exists x\_{1},...,x\_{n} [ p(a,x\_{1},...,x\_{n}) = 0 ]\Big\\}
\end{align}
$$
We will also say that a given set of numbers $\\{a\_{1},a\_{2},...\\}$ **is diophantine** if and only if it has a diophantine representation.



[^1]: This example is taken from [(Poonen 2008)](http://www-math.mit.edu/~poonen/papers/h10_notices.pdf). Other examples and explanations are adapted throughout from the following very readable surveys: [(Smith 2011)](https://www.logicmatters.net/resources/pdfs/MRDP.pdf),[(Pastern 2019)](https://imaginary.org/sites/default/files/snapshots/snapshots-2019-003.pdf)

[^2]: Considerable empirical progress was made in 2019 on sum of three cubes problems when solutions for $a=33$ and $a=42$ were discovered by Andrew Booker and colleagues (see [(Booker 2019)](https://arxiv.org/abs/1903.04284)). In the former case, his investigation involved looking at positive and negative integers in the range of $10^{16}$, which required the equivalent of 23 years of continuous computation on a single computer; this resulted in the following highly unintuitive variable solutions: $(x=886612897528752,y=-877840544286223,z=-2736111468807 04)$. In the latter case, finding a solution required (the equivalent of) 1.3 million hours of compute time, which is likewise an unfathomable amount of computation time.

[^3]: The story behind this conjecture is likely to be the most repeated anecdote in mathematics. Fermat had apparently scribbled this conjecture in 1637 into the margins of Diophantus' *Arithmetica* and claimed that he had *discovered a truly remarkable proof* that was too complex to fit in the margins. As mentioned above, the ultimate proof didn't arrive until over 350 years later.

[^4]: Emil Post famously had the following to say about Hilbert's 10th problem nearly 25 years before its final resolution: it `begs for an unsolvability proof'.

[^5]: We will only give a cursory overview of the number theoretic aspects of this problem that helped Matiyasevich and others to arrive at the final solution. The full details of this can be found in the surveys [(Davis 1973)](http://www.math.umd.edu/~laskow/Pubs/713/Diophantine.pdf).

[^6]: When trying to map specific polynomials into a sum of monomials in the form provided, it is important to recall that each $k\_{j}$ exponent can be 0, which maps any number to 1. Therefore, in $p(x\_{1},x\_{2}) = 4x\_{1}^{3} + 6x\_{2}$, the first **term** $4x\_{1}^{3}$ in the sum (whose coefficient is $4$) is equal to $4x\_{1}^{3}x\_{2}^{0}$, whereas the second term is equal to $6x\_{1}^{0}x\_{2}^{1}$. Likewise, for any term without an explicit coefficient, it can be assumed that the coefficient is 1.

[^7]: ] We note that it is also possible to consider equations with tuples of parameters, $(a\_{1},..,a\_{m})$, however our simplified version will suffice to prove the main result.
