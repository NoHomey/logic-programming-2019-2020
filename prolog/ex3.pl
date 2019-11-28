nat(zero).
nat(s(X)) :- nat(X).

add(zero, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

pairs_of_nats(X, Y) :- nat(Z), add(X, Y, Z).

append([], List, List).
append([Head|Tail], List, [Head|Result]) :- append(Tail, List, Result).

wrong_member_do_not_use(X, List) :- append(_, [X], Prefix), append(Prefix, _, List).

first(Head, [Head | _]).

last_with_append(X, List) :- append(_, [X], List).

last_with_rec(Head, [Head]).
last_with_rec(Last, [_ | Tail]) :- last_with_rec(Last, Tail).

prefix(Prefix, List) :- append(Prefix, _, List).

sufix(Sufix, List) :- append(_, Sufix, List).

member(X, List) :- prefix(Prefix, List), last_with_append(X, Prefix).

member_with_append(X, List) :- append(_, [X | _], List).

reverse([], []).
reverse([Head | Tail], Result) :- reverse(Tail, ReverseTail), append(ReverseTail, [Head], Result).

sublist(SubList, List) :- prefix(Prefix, List), sufix(SubList, Prefix).

subsequence([], []).
subsequence(SubSequence, [_ | Tail]) :- subsequence(SubSequence, Tail).
subsequence([Head | SubSequenceTail], [Head | Tail]) :- subsequence(SubSequenceTail, Tail).

distinct(Distinct, List) :- distinct_elements(Distinct, List, []).

distinct_elements([], [], _).
distinct_elements(Distinct, [Head | Tail], Seen) :-
       member(Head, Seen),  distinct_elements(Distinct, Tail, Seen).
distinct_elements([Head | Distinct], [Head | Tail], Seen) :-
       not(member(Head, Seen)),  distinct_elements(Distinct, Tail, [Head | Seen]).
