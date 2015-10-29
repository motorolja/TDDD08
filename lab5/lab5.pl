:- use_module(library(clpfd)).

/* container(Identifier,Resources,Time) */
container(1,2,2).
container(2,4,1).
container(3,2,2).
container(4,1,1).

/* on(ContainerA,ContainerB) */
on(1,4).
on(2,3).
on(3,4).


schedule(S, Sum):-
        findall([X,Y,Z], container(X,Y,Z), Containers),
        to_list(Containers, R, D, I),
        length(R, N),
        length(S, N),
        length(E, N),
        domain(S, 0, 100),
        domain(E, 0, 100),
        Done in 1..100,
	Limit in 1..100,
        ready(E,Done),
        start(S,E,I),
        tasks(S, D, E, R, Tasks),
        cumulative(Tasks, [limit(Limit)]),
	Sum #= Limit*Done,
        labeling([minimize(Sum)], [Sum|S]).

to_list([[X,Y,Z]|Containers], [Y|R], [Z|D], [X|I]):- to_list(Containers, R, D, I).
to_list([], [], [], []).

constrain(S1, S2, E1, E2, I1, I2):- on(I1, I2), S2 #>= E1.
constrain(S1, S2, E1, E2, I1, I2):- on(I2, I1), S1 #>= E2.
constrain(S1, S2, E1, E2, I1, I2):- \+(on(I1, I2)), \+(on(I2, I1)).


ready([], _).
ready([E|Es], Done) :-
        Done #>= E,
        ready(Es, Done).

start([],[],[]).
start([S1|STail],[E1|ETail],[I1|ITail]):- start2(S1, STail, E1, ETail, I1, ITail), start(STail, ETail, ITail).

start2(X, [], Y, [], Z, []).
start2(S1, [S2|STail], E1, [E2|ETail], I1, [I2|ITail]):- constrain(S1, S2, E1, E2, I1, I2), start2(S1, STail, E1, ETail, I1, ITail).

tasks([], [], [], [], []).
tasks([S|Ss], [D|Ds], [E|Es], [R|Rs], [T|Ts]) :-
        T = task(S, D, E, R, 0),
        tasks(Ss, Ds, Es, Rs, Ts).
