?- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(1,4).
on(2,3).
on(3,4).

schedule(Ss, Result) :-
	Ss = [S1,S2,S3,S4],
	REnd = [R1,R2,R3,R4],
	domain(Ss, 1, 30),
	domain(REnd,1,50),
	domain([Resources], 1,50),
	Tasks = [task(S1,2,R1,2,1),
		 task(S2,1,R2,4,2),
		 task(S3,2,R3,2,3),
		 task(S4,1,R4,1,4)],
	maximum(Resources,REnd),
	%order(Ss),
	cumulative(Tasks, [limit(4)]),
	
	append(Ss, [Resources], Vars),
	labeling([minimize(Resources)], Vars),
	max(REnd,R),
	min(Ss,S),
	Result is (R-S)*4.

order([]).
order([X]).
order([H1,H2|T]):-
	(\+ on(H1,H2) -> order(T); (\+ on(H2,H1) -> order(T))).

checkblock(X, [H|List]):- 

max([X],X).
max([X|Xs],X):- max(Xs,Y), X >=Y.
max([X|Xs],N):- max(Xs,N), N > X.

min([X],X).
min([X|Xs],X):- min(Xs,Y), X =<Y.
min([X|Xs],N):- min(Xs,N), N < X.