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

shortest_path([LH,RH|Tail],Out):-
	compatible(LH,RH),

schedule(List, Resources):-
	List = [A,B,C,D,E],
	Tasks = [task(A,1,_,2,0),
		 task(B,3,_,2,0),
		task(C,4,_,2,0),
		task(D,5,_,2,0),
		task(E,6,_,2,0)],
	cumulative(Tasks, [limit(6)]),
	label('Starting').