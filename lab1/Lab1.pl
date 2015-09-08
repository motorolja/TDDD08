% Written by Henrik Phung (henph669).

male(nisse).
male(peter).
male(bosse).
female(ulrika).
female(bettan).

beautiful(ulrika).
beautiful(nisse).
beautiful(peter).

rich(bettan).
rich(nisse).
strong(bettan).
strong(peter).
strong(bosse).
kind(bosse).

like(X,Y):- male(X), female(Y),	beautiful(Y).
like(X,Y):- =(nisse,X), female(Y), like(Y,X).

like(X,Y):- =(ulrika,X),like(Y,X), male(Y), rich(Y), kind(Y).
like(X,Y):- =(ulrika,X),like(Y,X) ,male(Y), beautiful(Y), strong(Y).

happy(X):- rich(X).
happy(X):- male(X), female(Y), like(X,Y), like(Y,X).
happy(X):- female(X), male(Y), like(X,Y), like(Y,X).



