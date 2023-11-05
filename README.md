# Gillispie

A Common lisp implementation of implemention of *Gillespie's
Stochastic Simulation* procedure(s) for biochemical systems.

The implementation is essentially an exploitation of Common Lisp
capabilities; it tries to be "efficient" and "usable", by defining a
*Domain Specific Language* based on macros.


## Standard Disclaimers about Stochastic Gillespie/Montecarlo Simulations 

As noted in "Numerical Recipes in C++" (reference), stochastic
simulations (hence Gillespie style ones) are a great way to burn
computer cycles.  Of course, Common Lisp is not the Fortran IV used in
Gillespie's original article (Fortran is more efficient), and your
watch has more computing power than the IBM 360 Gillespie's wrote his
code on, and yet you should be careful, especially because the current
implementation does not implement tau-leaping (yet).

Another caveat is that Gillespie was keenly aware of the constraints
under which his simulations produce a decent approximation of a trace
consistent with a solution of the *Chemical Master Equation*.  Using
the algorithm to model intra-cellular phenomena introduces, per se, an
extra layer of ... "approximation".


## See Also

The **Jillespie** system (written in Julia) came first; although I
have a long story of teaching this material.  A nice Python version,
simpler but very clean, and the inspiration of these exercise is
sueskind's.


### A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.


Enjoy

Marco Antoniotti, Milan, Italy, (C) 2023
