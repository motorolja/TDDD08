
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
like(nisse,Y):- female(Y), like(Y,nisse).

like(ulrika,Y):- male(Y), rich(Y), kind(Y), like(Y,ulrika).
like(ulrika,Y):- male(Y), beautiful(Y), strong(Y), like(Y,ulrika).

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
path(X,Y,[[X,Y]]):- edge(X,Y).
path(X,Y,[[X,Z]|XS]):- edge(X,Z), path(Z,Y,XS).
npath(X,Y,[Len|[XS]]):- path(X,Y,XS),length(XS,Len).
