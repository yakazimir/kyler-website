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

**Definition 1**: A **polynomial** expression over $n$ variables/*unknown quantities* $x\_{1},x\_{2},..,x\_{n}$, denoted as $p(x\_{1},...,x\_{n})$, is any finite sum of *monomials*, or expressions of the form:
$$
cx\_{1}^{k\_{1}},...,x\_{n}^{k\_{n}},
$$
where $c$ is an integer *coefficient* (i.e., positive and negative integers and zero, denoted as $\mathbb{Z}$) and $k\_{1},...,k\_{n}$ are natural numbers including zero (denoted as $\mathbb{N}$).

Examples polynomial expressions include $x\_{1}^{2} - 4x\_{1} + 3$ (where, for convenience, subtraction is used in place of addition with a negative number, $+ -4x$), $4x\_{1}^{3} + 6x\_{2}$, $x\_{1}+x\_{2} + ... + x\_{4}$ (with all coefficients $c$ equal to 1) and so on.[^6] When talking about polynomials it is important to specify the range of their variables. Diophantine equations are special types of polynomial equations that restrict the range of variables in the manner specified below.

**Definition 2**: A **diophantine equation** is specific type of polynomial expression $p(x\_{1},...,x\_{n}) = 0$ (also known as a *polynomial equation* in *traditional form*) restricted to **integer** unknowns $x\_{1},...,x\_{n}$ (or what Hilbert calls *rational integers*).

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


As briefly discussed in the last section, one of the big ideas is to define diophantine equations with additional variables called **parameters** $a$, as in $p(a,x\_{1},x\_{2},...,x\_{n})$ (where $x\_{1},...,x\_{n}$ continue to be what we previously called *unknowns*), which allow us to describe abstract *families of diophantine equations*. This gives rise to an important concept called a **diophantine set**.

**Definition 3**:  A **diophantine representation** of a diophantine equation $p$ with integer *unknowns* $x\_{1},...,x\_{n}$ and a *parameter* $a$[^7] is the set (which we will henceforth call a **diophantine set**):
$$\small
\begin{align}
S = \Big\\{ a \mid \exists x\_{1},...,x\_{n} [ p(a,x\_{1},...,x\_{n}) = 0 ]\Big\\}
\end{align}
$$
We will also say that a given set of numbers $\\{a\_{1},a\_{2},...\\}$ **is diophantine** if and only if it has a diophantine representation.

Interestingly, this set construction will allow us to talk about number theoretic concepts without having to say very much about specific equations. That is, unlike in ordinary number theory where one usually starts with a specific equation or family of equations and attempts to arrive at a set of solutions and parameters, the **idea here is that we will start with a set and try to arrive at a family of equations to demonstrate that the set is diophantine**.

**Examples** Let's illustrate this idea by starting with the set of natural numbers $\mathbb{N}$. **Is this set diophantine?** The answer is yes, which can be demonstrated by exploiting the following important theorem:


**Theorem 1**: [**Four-square theorem**](https://en.wikipedia.org/wiki/Lagrange%27s_four-square_theorem) (Lagrange 1770) Any natural number $a$ can be expressed a sum of four integer squares:

$$
\begin{align}
a = x\_{0}^{2} + x\_{1}^{2} + x\_{2}^{2} + x\_{3}^{2}
\end{align}
$$

Using this equation, we therefore have our polynomial equation that defines exactly the natural numbers. To make it have the desired diophantine form, we can simply move $a$ to the other side of the equation to arrive at $a - (x\_{0}^{2} + x\_{1}^{2} + x\_{2}^{2} + x\_{3}^{2}) = 0$. Now to ask if a given number such as 4 is in the set of natural numbers, we can also also ask whether $4 - x\_{0}^{2} + x\_{1}^{2} + x\_{2}^{2} + x\_{3}^{2} = 0$ has a solution.

What about the set of [**composite numbers**](https://mathworld.wolfram.com/CompositeNumber.html) (i.e., non-prime positive integers): $\{4, 6, 8, 9, 10, 12, ...\}$? Here we have a slightly less intuitive equation:
$$
\begin{align}
(x\_{1} + 2)(x\_{2} +2) - a = 0,
\end{align}
$$
which again gives us what we need to say that composite numbers are diophantine. How about the set of composite numbers that are odd? Well, we already have an equation for composite numbers, and we can express odd numbers with the following equation:
$$
\begin{align}
2x\_{1} + 1 - a = 0
\end{align}
$$
Defining this set can then be accomplished by combing both equations and squaring each individual equation (i.e., to ensure that they evaluate to 0):
$$
\begin{align}
(2x\_{1} + 1 - a)^{2} + ((x\_{2}+2)(x\_{3}+2) - a)^{2} = 0
\end{align}
$$
In general, rather than talking about individual equations or family of equations, we can even discuss **systems of equations** by combing multiple diophantine equations (or decomposing complex equations into simpler ones) in the manner illustrated above.

**Solutions in Natural Numbers** The four-square theorem introduced above can be used to make a few important points about diophantine equations as detailed in [(Matiyasevich 1993, Chapter1)](https://mitpress.mit.edu/books/hilberts-10th-problem). First, if you imagine that we have the following diophantine equation:
$$
\begin{align}
p(x\_{1},..,x\_{n}) = 0,
\end{align}
$$
with solutions in arbitrary integers $\mathbb{Z}$, then clearly this solution includes solutions in natural numbers since $\mathbb{N} \subset \mathbb{Z}$. On the other hand, if this equation has a solution in natural numbers, then this solution also includes a solution in arbitrary integers since we can use the four square theorem to rewrite each $x\_{j}$ in the equation as:
$$
\begin{align}
x\_{1} &= y\_{1,1}^{2} + y\_{1,2}^{2} + y\_{1,3}^{2} + y\_{1,4}^{2} \\\\ 
x\_{2} &= y\_{2,1}^{2} + y\_{2,2}^{2} + y\_{2,3}^{2} + y\_{2,4}^{2} \\\\ 
&... \\\\ 
x\_{n} &= y\_{n,1}^{2} + y\_{n,2}^{2} + y\_{n,3}^{2} + y\_{n,4}^{2}
\end{align}
$$
resulting in:
$$
\begin{align}
p((y\_{0,1}^{2} + y\_{0,2}^{2} + y\_{0,3}^{2} + y\_{0,4}^{2}),...,(y\_{n,1}^{2} + y\_{n,2}^{2} + y\_{n,3}^{2} + y\_{n,4}^{2})) = 0,
\end{align}
$$
where each $y_{j} \in \mathbb{Z}$. The important point is the following: **To establish the unsolvability of Hilbert's 10th problem in its original form, it is sufficient to establish the unsolvability of its analog for non-negative solutions** [(Matiyasevich 1993)](https://mitpress.mit.edu/books/hilberts-10th-problem). For this reason, we limit ourselves exclusively to variables in natural numbers. *Thus, the problem of solving Hilbert's problem will crucially rely on understanding properties of set of natural numbers* and algorithms over sets, which we turn to in the next section.

Computability Theory 
-------------------------

In this section, we detail the basic ideas from computability theory that inform us about sets and their algorithmic properties. This centers around a discussion of **recursively enumerable** and **recursive** sets and, most importantly, understanding the differences between both classes of sets.

**Definition 4**: A **recursively enumerable** (or *listable,computable,semi-decidable*) set is any subset $A \subseteq \mathbb{N}$ for which there exists an algorithm/Turing Machine/program that can print, with possible repeats, all the members of $A$ and nothing more[^8].

We will not delve into what exactly an algorithm means in this context, other than to say that it can be any effective procedure that solves the task at hand. Evoking [**Church's thesis**](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis), an algorithm being *effective* means that there should exist an accompanying [**Turing Machine**](https://plato.stanford.edu/entries/turing-machine/), which one might think of as a (correct and precise) mathematical model or simulation of the target algorithm and its hardware. The following result connects these notions more directly with ordinary functions.

**Lemma 1**: The following definitions are equivalent:

1. A is recursively enumerable (according **Definition 4**).
2. A is empty or in the range of a total (computable) function $f : \mathbb{N} \to \mathbb{N}$
3. A is in the domain of a partial (computable) function $f : \mathbb{N} \to \mathbb{N}$

We leave the proof an an exercise to the reader (though we describe an example below). We will also gloss over the meaning of a [**computable function**](https://en.wikipedia.org/wiki/Primitive_recursive_function), though it is worth pointing out that virtually all numerical functions encountered in ordinary mathematics (e.g., addition, multiplication, exponentiation, integer square root, ...)  are computable.

**Example** Let's take the set of square numbers considered already, i.e., $A = \\{ 1, 4, 9, 16, 25,.. \\}$. Is this set recursively enumerable? Yes, according to the following argumentation. We know that the following (diophantine) equation only has solution for $a$ only when $a$ is a square number:
$$
\begin{align}
a - x^{2} = 0,
\end{align}
$$
which we can exploit to define the following (computable) function (defined for all $x \in \mathbb{N}$):
$$
\begin{align}
f(x) = x^{2}
\end{align}
$$
Putting all this together, it is clear that the range of this function i.e.,
$$\small
\text{ran}(f) =  \Big\\{ f(1),f(2),f(3),f(4),.. \Big\\}
$$
will only be square numbers and that $A$ is recursively enumerable (thus satisfying condition 2). 

What about condition 3? Here we can define the square root function for $sqrt : \mathbb{N} \to \mathbb{N}$ (which is a computable function). Clearly, this function is a [**partial function**](https://en.wikipedia.org/wiki/Partial_function), or its domain is only a subset of $\mathbb{N}$, which is exactly the set of square numbers.

Notice that we didn't need to mention anything about an explicit algorithm or Turing Machine to make this case. What's more, **we did the following curious thing** (which started in the last section): we started by asking a question about an explicit set (which is often the starting point for many computer science or logic problems), then transformed this set into an equation. We could have gone the other way, i.e., start with an equation (which is often the starting point for a number theorist), then move from that equation to a set (then directly to an algorithm).

One alternative way of defining a recursively enumerable set, which is consistent with the definition provided, is a set where there exists an algorithm for verifying membership among inputs that are in the set.  Generalizing from this, there is a more general class of sets that imposes stricter conditions:

**Definition 5** A **recursive** (or *computable,decidable*) set is any subset $A \subseteq \mathbb{N}$ for which there exist an algorithm that can determine (full) *set membership* (i.e., definitively decide for any arbitrary number $x$ whether $x \in A$ or $x \notin A$).

Using the following result (as before, we leave the proof as a exercise for the reader):

**Lemma 2**: If a set $A$ and its complement $\overline{A}$ are both recursively enumerable, then $A$ is recursive.

we can demonstrate that the set of square numbers is recursive by showing that the set of non-square numbers is recursively enumerable. Here we will sketch an *algorithm* for doing this, which does the following: loops through/enumerates  each $i \in \mathbb{N}$, and print $i$ in the case when $\text{sqrt}$(i)$ (which is a computable function) does not return a whole number, and simply ignore the rest[^9].

The most important result for Hilbert's 10th problem from computer science is that not all recursively enumerable sets are recursive, which we examine in some detail below.

**Not all recursively enumerable sets are recursive** One common way to discover recursively enumerable sets that are not recursive is by exploiting the undecidability of the **Halting Problem**, which is one of the most famous theoretical results in computer science that was proved by Alan Turing. We take a brief detour to discuss this problem, then as a corollary provide an explicit set of numbers that it recursively enumerable though not recursive.


**Definition 6** In our simplified version of the Halting Problem, we will consider the following set $K$ that we call the **Halting Set**:
$$\small
\begin{align}
K = \Big\\{ (M\_{x},y) \mid \text{program $x$ }  (M\_{x}) \text{ halts on input } y \Leftrightarrow  M\_{x}(y) \downarrow  \Big\\}
\end{align}
$$
which consists of all pairs of **Turing machines** $M\_{x}$ identified by the integer code $x$ (importantly, we will assume that we can assign numerical ids to all Turing machines M, which is a technique often referred to as [*Gödelization*](https://www.encyclopediaofmath.org/index.php/G%C3%B6delization)) and inputs $y$ such that $M\_{x}$ **halts** or terminates on input $y$, which we denote using the symbol $\downarrow$ (in contrast, $\uparrow$ will either mean *never halts* or *undefined*). The **Halting Problem** is therefore the problem of determining membership in $K$ given any arbitrary $M\_{x}$ and $y$.

This definition seem simple enough, however it leads to difficulties centering around cases where the indices $x$ and inputs $y$ match one another (i.e., the numeric id $x$ happens to match the input $y$). These give rise to **diagonalization arguments**  of the sort first discovered by [*Cantor in relation to infinity*](https://www.nlp-kyle.com/post/infinity/).

**Theorem 2**: The Halting Problem is undecidable (i.e., there exists no universal algorithm/program/Turing machine for deciding membership in $K$).

**Proof** Let's imagine that $K$ is decidable (hence making the Halting Problem decidable). Then it is possible to define another Turing machine $M'$ that does the following (since $K$ being decidable would allow us to compute the membership conditions on the right):

$$\small
\begin{align}
    M'(x) = \begin{cases}
               0               & \text{if }  M\_{x} \text{ does not halt on } x \Leftrightarrow (M\_{x},x) \notin K  \\\\\\
               \uparrow               & \text{if } M\_{x} \text{ does halt on x} \hspace{.7cm} \Leftrightarrow (M\_{x},x) \in K \\\\\\
           \end{cases}
\end{align}
$$
In simpler terms, we want $M'$ to terminate on programs with matching indices that do not halt:
$$\small
\begin{align} 
M' \textbf{ halts on }x \text{(i.e., returns 0)}\Leftrightarrow \textbf{ program }x \textbf{ does not halt on } x
\end{align}
$$
However, this leads to a contradiction when we recognize that $M'$ (in virtue of being a valid Turing Machine that we assume halts) has its own index, say $e$, and that it too **can be run on its own input $e$**. This gives rise to the following:
$$\small
\begin{align} 
M\_{e} \textbf{ halts on }e &\Leftrightarrow \hspace{-1.5cm}&\text{ $[$program }x ] \textbf{ does not halt on } x  &&\hspace{-1.5cm}\text{(definition)} \\\\ 
M\_{e} \textbf{ halts on }e &\Leftrightarrow \hspace{-1.5cm}&[M\_{e}] \textbf{ does not halt on } e  &&\hspace{-1cm}\text{(substitution with $e$)}
\end{align}
$$
the last of which is a clear contradiction. Translating this into an assertion about set membership, this amounts to saying
\begin{align}
(M\_{e},e) \in K  \Leftrightarrow (M\_{e},e) \notin K,
\end{align}
which is again a contradiction, thus making our assertion that $K$ is decidable not tenable. □

The main point here is that **for any universal algorithm that tries to determine membership in $K$, there will inevitably  be  inputs for which deciding membership will give raise to a contradiction** that makes it impossible for any algorithm to decide whether or not it is in the set (and hence will cause an algorithm to run forever. We note that the Halting Set as defined is not itself a set of numbers of the type we defined above (i.e., a subset of $\mathbb{N}$), however it can be used to build an explicit set of numbers. The standard example is called the **diagonal halting set** defined as
$$\small
\begin{align}K\_{0} = \Big\\{ x \mid M\_{x}(x)\downarrow \Big\\},
\end{align}
$$
which can also be used as the basis of the proof above. We consider a different example set $A$ that  looks superficially closer the types of sets encountered in number theory, as described in the following corollary (which is the most important result in this section):

**Corollary 1**: There exists recursively enumerable sets that are not recursive.

**Proof** We have already basically proven this with $K$, though will make the case again using a specific example set from [(Poonen 2008)](http://www-math.mit.edu/~poonen/papers/h10_notices.pdf) to emphasize the larger point. Imagining again that we have an enumeration of Turing machines $M\_{1}$, $M\_{2}$, ..., consider the following set $A \subset \mathbb{N}$:
$$\small
\begin{align}
A = \Big\\{  j = 2^{x}3^{y} \mid M\_{x}(y)\downarrow  \Big\\}
\end{align}
$$
We will first establish  **$A$ is recursively enumerable**, which we  can do by imagining the following (completely impractical) algorithm:  using our enumeration of programs, we can  loop through all numbers and pairs $x,y=1,...,\infty$ and execute each $M\_{x}(y)$ in parallel. In cases where $M\_{x}(y)\downarrow$ is true, the computation has to stop after some finite number of steps, at which point we can simply print $2^{x}3^{y}$ (this will potentially lead to repeats, though this is fine according to our definition). We can then simply ignore cases where the programs run forever, since these are clearly cases where $M\_{x}(y)\downarrow$ is not true.

In terms of this set not being recursive, clearly deciding membership in $A$ would require solving membership in $K$, which we have already shown is not possible given the undecidability of the Halting Problem. Therefore, for some inputs the program might run forever, which we can safely ignore in this case. □


[^1]: This example is taken from [(Poonen 2008)](http://www-math.mit.edu/~poonen/papers/h10_notices.pdf). Other examples and explanations are adapted throughout from the following very readable surveys: [(Smith 2011)](https://www.logicmatters.net/resources/pdfs/MRDP.pdf),[(Pastern 2019)](https://imaginary.org/sites/default/files/snapshots/snapshots-2019-003.pdf)

[^2]: Considerable empirical progress was made in 2019 on sum of three cubes problems when solutions for $a=33$ and $a=42$ were discovered by Andrew Booker and colleagues (see [Booker (2019)](https://arxiv.org/abs/1903.04284)). In the former case, his investigation involved looking at positive and negative integers in the range of $10^{16}$, which required the equivalent of 23 years of continuous computation on a single computer; this resulted in the following highly unintuitive variable solutions: $(x=886612897528752,y=-877840544286223,z=-2736111468807 04)$. In the latter case, finding a solution required (the equivalent of) 1.3 million hours of compute time, which is likewise an unfathomable amount of computation time.

[^3]: The story behind this conjecture is likely to be the most repeated anecdote in mathematics. Fermat had apparently scribbled this conjecture in 1637 into the margins of Diophantus' *Arithmetica* and claimed that he had *discovered a truly remarkable proof* that was too complex to fit in the margins. As mentioned above, the ultimate proof didn't arrive until over 350 years later.

[^4]: Emil Post famously had the following to say about Hilbert's 10th problem nearly 25 years before its final resolution: it `begs for an unsolvability proof'.

[^5]: We will only give a cursory overview of the number theoretic aspects of this problem that helped Matiyasevich and others to arrive at the final solution. The full details of this can be found in the surveys [(Davis 1973)](http://www.math.umd.edu/~laskow/Pubs/713/Diophantine.pdf).

[^6]: When trying to map specific polynomials into a sum of monomials in the form provided, it is important to recall that each $k\_{j}$ exponent can be 0, which maps any number to 1. Therefore, in $p(x\_{1},x\_{2}) = 4x\_{1}^{3} + 6x\_{2}$, the first **term** $4x\_{1}^{3}$ in the sum (whose coefficient is $4$) is equal to $4x\_{1}^{3}x\_{2}^{0}$, whereas the second term is equal to $6x\_{1}^{0}x\_{2}^{1}$. Likewise, for any term without an explicit coefficient, it can be assumed that the coefficient is 1.

[^7]: We note that it is also possible to consider equations with tuples of parameters, $(a\_{1},..,a\_{m})$, however our simplified version will suffice to prove the main result.

[^8]: Note that we can make this definition more complex both by considering subsets of $\mathbb{N}^{m}$ (i.e., sets of $m-$tuples over $\mathbb{N}$), or  sets over $\mathbb{Z}$, however our restricted definition will suffice for proving our main results.

[^9]: Clearly this algorithm is impractical since it requires looping through an infinite number of numbers in $\mathbb{N}$. When we argue about the existence of algorithms in mathematics, we are allowed to make unrealistic, even outlandish, assumptions about the amount of resources and time we are allowed (e.g., that we have an infinite of time/memory/parallel computations/..). Part of the reason why we describe abstract algorithms here in terms of Turing Machines, as opposed to Python or Java programs is that they permit such excesses (e.g., by providing a infinite memory in the form of an **infinite memory tape** on which we can read and write). 
