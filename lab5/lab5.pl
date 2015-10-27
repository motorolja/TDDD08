:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

:- dynamic
	on/2.
on(1,4).
on(2,3).
on(3,4).

schedule(S, Done):-
	S= [S1,S2,S3,S4],
	E= [E1,E2,E3,E4],
	D= [D1,D2,D3,D4],
	domain(S, 0, 100),
	domain(E, 0, 100),
	D=[1,2,3,4],
	Done in 1..100,
	ready(E,Done),
	start(S,E),
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

start([_X],[_Y]).
start([S1,S2|STail],[E1,E2|ETail]):-
	(evaluate(S1,S2,E1,E2) -> start([S2|STail],[E2|ETail]);
	start([S2|STail],[E2|ETail])).

evaluate(S1,S2,E1,E2):-
	on(S1,S2)->S2#>=E1.
	%on(S2,S1)->S1#>=E2).
	

reverse([],[]).
reverse([X|Xs],YsX) :-
	reverse(Xs,Ys),
	append(Ys,[X],YsX).
%ontop(X,Y):- on