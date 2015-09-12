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

like(X,Y):- =(ulrika,X), male(Y), rich(Y), kind(Y), like(Y,X).
like(X,Y):- =(ulrika,X), male(Y), beautiful(Y), strong(Y), like(Y,X).

happy(X):- rich(X).
happy(X):- male(X), female(Y), like(X,Y), like(Y,X).
happy(X):- female(X), male(Y), like(X,Y), like(Y,X).

edge(a,b).
edge(a,c).
edge(b,c).
edge(c,d).
edge(c,e).
edge(e,f).
edge(e,g).
edge(f,g).
edge(d,h).
edge(d,f).

path(X,Y):- edge(X,Y).
path(X,Y):- edge(X,Z), path(Z,Y). 
path(X,Y,[X,Y]):- edge(X,Y).
path(X,Y,[X,Z|XS]):- edge(X,Z), path(Z,Y,XS).
npath(X,Y,[Length|XS]):- path(X,Y,XS),length(XS,Len),Length is Len/2.
