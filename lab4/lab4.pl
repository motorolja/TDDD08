%2M from left to right
boat([CLeft, MLeft, leftside, CRight, MRight], [CLeft, MLeft2, rightside, CRight, MRight2]):-
    MLeft2 is MLeft-2,
    MRight2 is MRight+2,
    statecheck(CLeft, MLeft2, CRight, MRight2).

%2C from left to right
boat([CLeft, MLeft, leftside, CRight, MRight], [CLeft2, MLeft, rightside, CRight2, MRight]):-
    CLeft2 is CLeft-2,
    CRight2 is CRight+2,
    statecheck(CLeft2, MLeft, CRight, MRight).

%1M and 1C from left to right
boat([CLeft, MLeft, leftside, CRight, MRight], [CLeft2, MLeft2, rightside, CRight2, MRight2]):-
    CLeft2 is CLeft-1,
    MLeft2 is MLeft-1,
    CRight2 is CRight+1,
    MRight2 is MRight+1,
    statecheck(CLeft2, MLeft2, CRight2, MRight2).

%1M from left to right
boat([CLeft, MLeft, leftside, CRight, MRight], [CLeft, MLeft2, rightside, CRight, MRight2]):-
    MLeft2 is MLeft-1,
    MRight2 is MRight+1,
    statecheck(CLeft, MLeft2, CRight, MRight2).

%1C from left to right
boat([CLeft, MLeft, leftside, CRight, MRight], [CLeft2, MLeft, rightside, CRight2, MRight]):-
    CLeft2 is CLeft-1,
    CRight2 is CRight+1,
    statecheck(CLeft2, MLeft, CRight2, MRight).


% -------------------------------------------------

%2M from right to left
boat([CLeft, MLeft, rightside, CRight, MRight], [CLeft, MLeft2, leftside, CRight, MRight2]):-
    MRight2 is MRight-2,
    MLeft2 is MLeft+2,
    statecheck(CLeft, MLeft2, CRight, MRight2).

%2C from right to left
boat([CLeft, MLeft, rightside, CRight, MRight], [CLeft2, MLeft, leftside, CRight2, MRight]):-
    CRight2 is CRight-2,
    CLeft2 is CLeft+2,
    statecheck(CLeft2, MLeft, CRight2, MRight).

%1M and 1C from right to left
boat([CLeft, MLeft, rightside, CRight, MRight], [CLeft2, MLeft2, leftside, CRight2, MRight2]):-
    CRight2 is CRight-1,
    MRight2 is MRight-1,
    CLeft2 is CLeft+1,
    MLeft2 is MLeft+1,
    statecheck(CLeft2, MLeft2, CRight2, MRight2).

%1M from right to left
boat([CLeft, MLeft, rightside, CRight, MRight], [CLeft, MLeft2, leftside, CRight, MRight2]):-
    MRight2 is MRight-1,
    MLeft2 is MLeft+1,
    statecheck(CLeft, MLeft2, CRight, MRight2).

%1C from right to left
boat([CLeft, MLeft, rightside, CRight, MRight], [CLeft2, MLeft, leftside, CRight2, MRight]):-
    CRight2 is CRight-1,
    CLeft2 is CLeft+1,
    statecheck(CLeft2, MLeft, CRight2, MRight).


statecheck(CLeft, MLeft, CRight, MRight):-
    MLeft>=0, CLeft>=0, MRight>=0, CRight>=0, MLeft>=CLeft, MRight>=CRight;
    MLeft>=0, CLeft>=0, CRight>=0, MLeft>=CLeft, MRight=0;
    CLeft>=0, MRight>=0, CRight>=0, MLeft=0, MRight>=CRight.


depthsearch([CLeft, MLeft, X, CRight, MRight],[CLeft, MLeft, X, CRight, MRight], _, NodeList):-
    printer(NodeList).


depthsearch([CLeft1, MLeft1, Start, CRight1, MRight1], [CLeft2, MLeft2, Goal, CRight2, MRight2], Oldnodes, NodeList):-

    boat([CLeft1, MLeft1, Start, CRight1, MRight1], [CLeft3, MLeft3, GoalTemp, CRight3, MRight3]),

    \+(member([CLeft3, MLeft3, GoalTemp, CRight3, MRight3], Oldnodes)),

    depthsearch([MLeft3, CLeft3, GoalTemp, MRight3, CRight3], [CLeft2, MLeft2, Goal, CRight2, MRight2],

         [[CLeft3, MLeft3, GoalTemp, CRight3, MRight3]|Oldnodes],

         [[[CLeft1, MLeft1, Start, CRight1, MRight1],[CLeft3, MLeft3, GoalTemp, CRight3, MRight3]]|NodeList]).


printer([]) :- nl.
printer([[Start,Goal]|NodeList]) :- printer(NodeList), write(Start), write(' -> '), write(Goal), nl.

rundepth(CR,MR,CL,ML):- CTot is CL+CR,
    MTot is ML+MR,
    depthsearch([CR,MR,leftside,CL,ML],[0,0,rightside,CTot,MTot],[[CR,MR,leftside,CL,ML]],_).

%-----------------------------------
breadthfirst(ChildList, [CLeftG, MleftG, CRightG, MRightG], Solution):-
	breadthfirst1([[ChildList]], [CLeftG, MleftG, CRightG, MRightG], Solution).

breadthfirst1(ChildList, [CLeftG, MleftG, CRightG, MRightG], Solution):-
    getchilds(ChildList, NewChilds),
    checkgoal([CLeftG, MleftG, CRightG, MRightG], NewChilds,Solution).

breadthfirst1(ChildList, [CLeftG, MleftG, CRightG, MRightG], Solution):-
    getchilds(ChildList, NewChilds),
    breadthfirst1(NewChilds, [CLeftG, MleftG, CRightG, MRightG], Solution).

breadthfirst1([],[],[]).

getchilds([],[]).
getchilds([[[CLeft, MLeft, CRight, MRight]|T]|ChildList], NewChilds):-
    findall([CLeft2, MLeft2, CRight2, MRight2],
	    boat([CLeft, MLeft, Start, CRight, MRight], [CLeft2, MLeft2, Goal, CRight2, MRight2]), ChildList0),
    getchilds(ChildList, ChildList2),
    merger(ChildList0, [[CLeft, MLeft, CRight, MRight]|T] , ChildList1),
    append(ChildList1,ChildList2, NewChilds).

merger([],Parent,[]). % slår ihop parent med childs
merger([Child|ChildList], Parent, ReadyList):- merger(ChildList, Parent, ReadyList2),
	append([Child],Parent, ReadyList1),
	append([ReadyList1], ReadyList2, ReadyList).

checkgoal([],[],[]).
checkgoal([CLeftG, MLeftG, CRightG, MRightG], [[[CLeftG, MLeftG, CRightG, MRightG]|T]|Tail],[[CLeftG, MLeftG, CRightG, MRightG]|T]).
checkgoal([CLeftG, MLeftG, CRightG, MRightG], [[[CLeft, MLeft, CRight, MRight]|T]|Tail], Out) :- checkgoal([CLeftG, MLeftG, CRightG, MRightG], Tail, Out).
