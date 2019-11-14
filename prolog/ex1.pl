female(mary).
female(sandra).
female(juliet).
female(lisa).

male(peter).
male(paul).
male(joe).
male(bob).
male(harry).

parent(bob, lisa).
parent(bob, paul).
parent(bob, mary).
parent(juliet, lisa).
parent(juliet, paul).
parent(juliet, mary).
parent(peter, harry).
parent(lisa, harry).
parent(mary, joe).
parent(mary, sandra).

mother(X, Y) :- female(X), parent(X, Y).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
