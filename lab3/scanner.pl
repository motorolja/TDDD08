/* Lab3 - Execise 3.1 */

run(In, String, Out) :-
	scan(String, Tokens),
	parse(Tokens, AbstStx).%,
	%execute(In, AbstStx, Out).

%---------------------------------------------------------------------
parse(Tokens,AbstStx):- pgm(AbstStx,Tokens, []).

%Uppdaterad version
pgm(N)--> cmd(N).
pgm(seq(X,Y))-->  cmd(X),[;],pgm(Y).

cmd(skip)--> [skip].
cmd(if(Bool,Pgm1,Pgm2))--> [if],bool(Bool),[then], pgm(Pgm1),[else], pgm(Pgm2),[fi].
cmd(set(X,Expr))--> identifier(X),[:=], expr(Expr).
cmd(while(Bool,Pgm))--> [while],bool(Bool),[do],pgm(Pgm),[od].

cmd(X)--> bool(X).

bool(V >= H)--> expr(V),[>=],expr(H).
bool(V > H)--> expr(V),[>],expr(H).
bool(V =< H)--> expr(V),[=<],expr(H).
bool(V < H)--> expr(V),[<],expr(H).
bool(V = H)--> expr(V),[=],expr(H).
bool(X)-->expr(X).

expr(V * H)--> factor(V),[*],expr(H).
expr(X)--> factor(X).

factor(V + H)--> term(V),[+],factor(H).
factor(V - H)--> term(V),[-],factor(H).
factor(X)--> term(X).

term(X)--> numb(X)|identifier(X).

numb(num(X))--> [num(X)],{number(X)}.
identifier(id(X))--> [id(X)],{atom(X)}.

%parse(Tokens,AbstStx):- pgm(Tokens,AbstStx).
%----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	
/* Execise 2.3 */

id(I):- atomic(I),\+(number(I)).
num(N):- number(N).

execute(X,skip,X).
execute(X,seq(C1,C2),Y):- execute(X,C1,Res1), execute(Res1,C2,Y).
execute(X,if(B,C1,C2),Y):- (bool_eval(X,B) -> execute(X,C1,Y); execute(X,C2,Y)).
execute(X,while(B,C),Y):- (bool_eval(X,B) -> execute(X,C,Out), execute(Out,while(B,C),Y); Y=X).
execute(X,set(id(I),E),Y):- eval(X,E,Sum), update(X,I,Sum,Y).

bool_eval(X,A<B):- eval(X,A,LH), eval(X,B,RH), LH<RH.
bool_eval(X,(A=<B)):- eval(X,A,LH), eval(X,B,RH), LH=<RH.
bool_eval(X,A>B):- eval(X,A,LH), eval(X,B,RH), LH>RH.
bool_eval(X,(A>=B)):- eval(X,A,LH), eval(X,B,RH), LH>=RH.

eval(X,id(I),CV):- member([I,CV],X).
eval(X,A+B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV+BV.
eval(X,A-B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV-BV.
eval(X,A*B,CV):- eval(X,A,AV), eval(X,B,BV), CV is AV*BV.
eval(X,A,A):-num(A).

update([[H1,H2]|_Tail],Identifier,Replacement,Result):- update(_Tail,Identifier,Replacement,Temp),
	append([[H1,H2]], Temp, Result).

update([[Identifier,H2]|_Tail],Identifier,Replacement,Result):- append([[Identifier,Replacement]], _Tail, Result).



% Scanner for assignment 3
% TDDD08 Logic Programming
%
% top predicate:
% scan(+String, -Tokens) 
%
% try: scan("x:=3; y:=1; while x>1 do y := y*x; x := x-1 od",Tokens).
%
% NOTE: strings are lists of ASCII codes, i.e.
% "Prolog" = [80,114,111,108,111,103]

scan([],[]).
scan([C|Cs],[';'|Ts]) :-
	semicolon(C),!,
	scan(Cs,Ts).
scan([C|Cs],Ts) :-
	space(C),!,
	scan(Cs,Ts).
scan([C|Cs],[num(T)|Ts]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CNum),
	name(T,[C|CNum]),
	scan(Cs1,Ts).
scan([C1,C2|Cs],[T|Ts]) :-
	name(T,[C1,C2]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]):-
	name(T,[C]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]) :-
	letter(C),
	scan_key_or_id(Cs,Cs1,CWord),
	name(Word,[C|CWord]),
	classify(Word,T),
	scan(Cs1,Ts).

% scaning a number
% scan_number(+In, -Out, -Num)
% Num is a string of digits from front of In,
% Out is the remaining string

scan_number([C|Cs],Cs1,[C|CN]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CN).
scan_number(Cs,Cs,[]).

% scaning a keyword or an identifier
% scan_key_or_id(+In, -Out, -Word)
% Word is a string from front of In,
% Out is the remaining string

scan_key_or_id([C|Cs],Cs1,[C|CW]) :-
	(letter(C)
	 ;
	 digit(C)
	),!,
	scan_key_or_id(Cs,Cs1,CW).
scan_key_or_id(Cs,Cs,[]).

% distinguishing keywords from identifiers

classify(W,T) :-
	keyword(W),!,
	T = W.
classify(W,id(W)).


digit(C) :-
	C >= "0", C =< "9".


letter(C) :-
	C >= "a", C =< "z"
	;
	C >= "A", C =< "Z".


semicolon(59).


operator('*').
operator('+').
operator('/').
operator('-').
operator('>').
operator('<').
operator('=').
operator('=<').
operator('>=').
operator(':=').


space(32).


keyword(skip).
keyword(if).
keyword(then).
keyword(else).
keyword(fi).
keyword(while).
keyword(do).
keyword(od).
keyword(true).
keyword(false).
