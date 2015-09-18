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




