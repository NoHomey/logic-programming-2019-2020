elem(a).
elem(b).
elem(c).

abcList(null).
abcList(list(Head, Tail)) :- abcList(Tail), elem(Head).

member(X, [ X | _    ]).
member(X, [ _ | Tail ]) :- member(X, Tail).
