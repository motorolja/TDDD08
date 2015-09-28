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

execute(X,skip,X).
%execute(X,id(I),Out):- member([I|T],X), Out is T.
%execute(X,num(N),Out):- member([H|N],X), Out is H.
execute(X,seq(C,C),Y).
execute(X,if(B,id(C1),id(C2)),Y):- Z is (C1>C2), B = Z.
execute(X,while(B,C),Y).
execute(X,set(id(I),E),Y):- nonmember([I,E],X), append(X,[[I,E]],Y).

id(I).
num(N).
eval(A+B,CV):- eval(A,AV), eval(B,BV), CV is AV+BV.
eval(A-B,CV):- eval(A,AV), eval(B,BV), CV is AV-BV.
eval(A*B,CV):- eval(A,AV), eval(B,BV), CV is AV*BV.
eval(Num,Num):-num(Num).

/* 2.4 */
union(SetA,SetB,Result):- append(SetA,SetB,Sorted), sort(Sorted,Result).
powerset(Set,Result):- sort(Set,Sorted),setof(X, subseq0(Sorted,X), Result).
