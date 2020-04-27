% course 1
after(dis2, dis1).
after(va, la).
after(oop, up).
% course 2.1
after(dupril, dis2).
after(dupril, la).
after(eai, ds).
after(sdp, oop).
after(sdp, ds).
% course 2.2
after(chm, dis1).
after(chm, la).
after(daa, ds).
after(daa, sdp).
after(lp, ds).
after(lp, eai).

uses1(X, Y) :- after(X, Y).
uses2(X, Y) :- after(Z, Y), after(X, Z).
uses3(X, Y) :- after(Z, Y), uses2(X ,Z).

uses_n(X, Y) :- uses1(X, Y).
uses_n(X, Y) :- uses2(X, Y).
uses_n(X, Y) :- uses3(X, Y).

uses(X, Y) :- after(X, Y).
uses(X, Y) :- after(Z, Y), uses(X, Z).