% gen_bin_func(+A, +B, +C, >F)
gen_bin_func([], _, _, []).
gen_bin_func([H | T], B, C, [[H, FH] | FT]) :-
    gen_bin_func(T, B, C, FT),
    gen_func(B, C, FH).

% gen_func(+A, +B, >F)
gen_func([], _, []).
gen_func([H | T], B, [[H, S] | FT]) :-
    gen_func(T, B, FT),
    member(S, B).

% sends_to_bin(+F, +X, +Y, >Z)
sends_to_bin(F, X, Y, Z) :-
    member([X, G], F),
    sends_to_un(G, Y, Z).

% sends_to_un(+F, +X, >Y)
sends_to_un(F, X, Y) :-
    member([X, Y], F), !.

% gen_group(+N, >[G, E, Op, Inv])
gen_group(N, [G, 1, Op, Inv]) :-
    range(1, N, G), !,
    gen_bin_func(G, G, G, Op),
    is_neutral(E, G, Op),
    is_associative(Op, G),
    gen_func(G, G, Inv),
    is_inv(Inv, G, E, Op).

% range(+A, +B, >R)
range(A, B, []) :- A > B.
range(A, B, [A | R]) :-
    A =< B,
    A1 is A + 1,
    range(A1, B, R).

% is_neutral(+E, +G, +Op)
is_neutral(E, G, Op) :-
    not((
        member(A, G),
        not((
            sends_to_bin(Op, A, E, A),
            sends_to_bin(Op, E, A, A)
        ))
    )).

% is_associative(+Op, +G)
is_associative(Op, G) :-
    not((
        member(A, G),
        member(B, G),
        member(C, G),
        not((
            sends_to_bin(Op, A, B, Z),
            sends_to_bin(Op, Z, C, R),
            sends_to_bin(Op, B, C, T),
            sends_to_bin(Op, A, T, R)
        )) 
    )).

% is_inv(Inv, G, E, Op)
is_inv(Inv, G, E, Op) :-
    not((
        member(A, G),
        not((
            sends_to_un(Inv, A, I),
            sends_to_bin(Op, A, I, E),
            sends_to_bin(Op, I, A, E)    
        ))
    )).