/* Lab 2 TDDD08 - Logic Programming */

%numbers([6,2,3,1,9,2,5,7]).

%isort(Unsorted,Sorted):- insert_sort([6,2,3,1,9,2,5,7],[],Sorted). 
%insert_sort([],Acc,Acc).

%insert_sort([Head|Tail],Acc,Sorted):- insert_element(Head,Acc,NAcc), insert_sort(Tail,NAcc,Sorted).

%insert_element(X,[Y|Tail],[Y|NT]):- X>Y, insert_element(X,Tail,NT).
%insert_element(X,[Y|Tail],[X,Y|Tail]):- X=<Y.
%insert_element(X,[],[X]).

% qsort(X,[]):- 2.

isort([Head|Tail],Sorted):- insert_element(Head,Tail,Sorted).

insert_element(X,[Y|Tail],[Y|NotTail]):- X>Y, insert_element(X,Tail,NotTail).
insert_element(X,[Y|Tail],[X,Y|Tail]):- X=<Y.
insert_element(X,[],[X]).