:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(a,d).
on(b,c).
on(c,d).

compatible(ContainerA,ContainerB) :-
	\+ on(ContainerA,ContainerB).


schedule() :-

