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
        findall([X,Y,Z], container(X,Y,Z), Containers),
        to_list(Containers, R, D),
        length(R, N),
        length(S, N),
        length(E, N),
        domain(S, 0, 100),
        domain(E, 0, 100),
        Done in 1..100,
        ready(E,Done),
        start(S,E),
        /*Tasks = [task(S1,2,E1,2,0),
                 task(S2,1,E2,4,0),
                 task(S3,2,E3,2,0),
                 task(S4,1,E4,1,0)],*/
        tasks(S, D, E, R, Tasks),
        cumulative(Tasks, [limit(4)]),
        labeling([minimize(Done)], [Done|S]).

to_list([[X,Y,Z]|Containers], [Y|R], [Z|D]):- to_list(Containers, R, D).
to_list([], [], []).

constrain(S1, S2, E1, E2):- on(S1, S2), S2 #>= E1.
constrain(S1, S2, E1, E2):- on(S2, S1), S1 #>= E2.
constrain(S1, S2, E1, E2):- \+(on(S1, S2)), \+(on(S2, S1)).


ready([], _).
ready([E|Es], Done) :-
        Done #>= E,
        ready(Es, Done).

start([],[]).
start([S1|STail],[E1|ETail]):- start2(S1, STail, E1, ETail), start(STail, ETail).

start2(X, [], Y, []).
start2(S1, [S2|STail], E1, [E2|ETail]):- constrain(S1, S2, E1, E2), start2(S1, STail, E1, ETail).

tasks([], [], [], [], []).
tasks([S|Ss], [D|Ds], [E|Es], [R|Rs], [T|Ts]) :-
        T = task(S, D, E, R, 0),
        tasks(Ss, Ds, Es, Rs, Ts).
