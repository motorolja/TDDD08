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

id(a):- a is 1.
id(b):- b is 2.
id(c):- c is 3.
id(d):- d is 4.
id(e):- e is 5.
id(f):- f is 6.
skip().

num(1).
num(2).
num(3).
num(4).
num(5).
num(6).

set(I,E):- I is E.

seq(Command1,Command2):- Command1, Command2.

if(LH,>,RH):- (LH>RH).
if(LH,<,RH):- (LH<RH).
if(LH,>=,RH):- (LH>=RH).
if(LH,>=,RH):- LH>=RH.
if(LH,==,RH):- =(LH,RH).

while(IsTrue,Do):- IsTrue, Do, while(IsTrue,Do).

execute(X,Y,_Result):- seq(set(id(Y),num(1)),
while(id(X) > num(1),
seq(set(id(Y), id(Y) * id(X)),
set(id(X), id(X) - num(1))))).


/* 2.4 */
union(SetA,SetB,Result):- append(SetA,SetB,Sorted), sort(Sorted,Result).
powerset(Set,Result):- sort(Set,Sorted),setof(X, subseq0(Sorted,X), Result).
