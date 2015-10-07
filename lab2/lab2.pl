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

id(I).
num(N):- number(N).

execute(X,skip,X).
execute(X,seq(C1,C2),Y):- execute(X,C1,Res1), execute(Res1,C2,Y).
execute(X,if(B,C1,C2),Y):- bool_eval(X,B),  execute(X,C1,Y)
execute(X,if(B,C1,C2),Y):- bool_eval_false(X,B),  execute(X,C2,Y)

% not allowed ->
execute(X,while(B,C),Y):- (bool_eval(X,B) -> execute(X,C,Out), execute(Out,while(B,C),Y); Y=X).
execute(X,set(id(I),E),Y):- eval(X,E,Sum), update(X,I,Sum,Y).

bool_eval(X,A<B):- eval(X,A,LH), eval(X,B,RH), LH<RH.
bool_eval(X,(A=<B)):- eval(X,A,LH), eval(X,B,RH), LH=<RH.
bool_eval(X,A>B):- eval(X,A,LH), eval(X,B,RH), LH>RH.
bool_eval(X,(A>=B)):- eval(X,A,LH), eval(X,B,RH), LH>=RH.
bool_eval(X,A==B):- eval(X,A,LH), eval(X,B,RH), LH==RH.

bool_eval_false(X,A>=B):- eval(X,A,LH), eval(X,B,RH), LH<RH.

eval(X,id(I),CV):- member([I,CV],X).
eval(X,A+B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV+BV.
eval(X,A-B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV-BV.
eval(X,A*B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV*BV.
eval(X,A,A):-num(A).

update([[H1,H2]|Tail],Identifier,Replacement,[[H1,H2]|Temp]):- update(Tail,Identifier,Replacement,Temp).
	%append([[H1,H2]], Temp, Result).

% same as above
update([[Identifier,H2]|_Tail],Identifier,Replacement,Result):- append([[Identifier,Replacement]], _Tail, Result).

/* 2.4 */
union(SetA,SetB,Result):- append(SetA,SetB,Sorted), sort(Sorted,Result).
powerset(Set,Result):- sort(Set,Sorted),setof(X, subseq0(Sorted,X), Result).

% Missing intersect with two lists