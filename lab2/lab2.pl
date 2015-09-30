/* Lab 2 TDDD08 - Logic Programming */

/* Insertion sort */

isort([],[]).
isort([Head|Tail],Sorted):- isort(Tail,SortedTail),insert_element(Head,SortedTail,Sorted).

insert_element(X,[Y|Tail],[Y|NotTail]):- X>Y, insert_element(X,Tail,NotTail).
insert_element(X,[Y|Tail],[X,Y|Tail]):- X=<Y.
insert_element(X,[],[X]).

/* Quick sort */
qsort([],[]).
qsort([Head|Tail],Sorted):-
	splice_list(Head,Tail,LH,RH), qsort(LH,ListL), qsort(RH,ListR), append(ListL,[Head|ListR],Sorted).

splice_list(_Value,[],[],[]).
splice_list(Value,[Head|Tail],[Head|TailL],RH):- Value>Head,splice_list(Value,Tail,TailL,RH).
splice_list(Value,[Head|Tail],LH,[Head|TailR]):- Value=<Head,splice_list(Value,Tail,LH,TailR).


/* Exercise 2.2 - example code */
% middle(X,Xs)
% X is the middle element in the list Xs
middle(X, [X]).
middle(X, [_First|Xs]) :-
append(Middle, [_Last], Xs),
middle(X, Middle).


/* 2.3 */

while(IsTrue,Do):- IsTrue, Do, while(IsTrue,Do).

id(I).
num(N).

execute(X,skip,X).
execute(X,seq(C,C),Y).
execute(X,if(B,id(C1),id(C2)),Y):- Z is (C1>C2), B = Z.
execute(X,while(B,C),Y).
execute(X,set(id(I),E),Y):- eval(X,E,Sum), update(X,I,Sum,Y).

eval(X,id(I),CV):- member([I,CV],X).
eval(X,A+B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV+BV.
eval(X,A-B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV-BV.
eval(X,A*B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV*BV.
eval(X,num(A),A).

update([[H1,H2]|_Tail],Identifier,Replacement,Result):- update(_Tail,Identifier,Replacement,Temp),
	append([[H1,H2]], Temp, Result).

update([[Identifier,H2]|_Tail],Identifier,Replacement,Result):- append([[Identifier,Replacement]], _Tail, Result).

/* 2.4 */
union(SetA,SetB,Result):- append(SetA,SetB,Sorted), sort(Sorted,Result).
powerset(Set,Result):- sort(Set,Sorted),setof(X, subseq0(Sorted,X), Result).
