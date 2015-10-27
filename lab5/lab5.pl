:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

:- dynamic
	on/2.
on(1,4).
on(2,1).
on(3,2).

schedule(S, Done):-
	S= [S1,S2,S3,S4],
	E= [E1,E2,E3,E4],
	D= [D1,D2,D3,D4],
	domain(S, 0, 100),
	domain(E, 0, 100),
	D=[1,2,3,4],
	Done in 1..100,
	ready(E,Done),
	Tasks = [task(S1,2,E1,2,D1),
		 task(S2,1,E2,4,D2),
		 task(S3,2,E3,2,D3),
		 task(S4,1,E4,1,D4)],
	
	cumulative(Tasks, [limit(4)]),
	%constrain(D),
	labeling([minimize(Done)], [Done|S]).



constrain([]).
constrain([X]):- retract(on(X,Y)).
constrain([H1,H2|Tail]):-
	\+ on(H2,H1),
	constrain([H1|Tail]),
	constrain([H2|Tail]).
		
ready([], _).
ready([E|Es], Done) :-
	Done #>= E,
	ready(Es, Done).
