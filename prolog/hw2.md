For this homework use only `append` and `distinct` from (ex3.pl) , helper predicates and recursion.

The definition of `append` is:

```prolog
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
```

# Task 1 (removing first occurence of an element from a list)

Implement predicate `remove(X, L, R)` with the following semantic: If `X` is member of `L` then `R` is `L` from which the first occurece of `X` is removed.

Do not use `member` and do not implement it!

# Task 2 (inserting element in a random position in a list)

Implement predicate `insert(X, L, R)` with the following semantic: `R` is `L` in which `X` is inserted (in a random position).
Here random position means when the  query `?- insert(d, [a, b, c], R).` is given to Prolog.
Prolog can answer with all possible insertions of `d` in `[a, b, c]`. Those insertions are 4 their order does not matter.

Example (Query: `?- insert(d, [a, b, c], R).`):

answers:

- `X = [d, a, b, c]` 
- `X = [a, d, b, c]`
- `X = [a, b, d, c]`
- `X = [a, b, c, d]`
- `false`

# Task 3 (List permutations)

For this taks use `insert` from last one.

Implement predicate `permutate(L, P)` with the following semantic: `P` is permutation of the elements of `L`.

When used as decider `permutate(L, P)` should correctly check if `P` is permutation of `L`.

When used as generator `permutate(L, P)` should generate in `P` one by one all permutations of `L`.

Examples:

`?- permutate([a, b, c], [c, a, b]).` answer: `true`.

`?- permutate([a, b, c], P).`

answers:

- `X = [a, b, c]`
- `X = [b, a, c]`
- `X = [b, c, a]`
- `X = [a, c, b]`
- `X = [c, a, b]`
- `X = [c, b, a]`
- `false`

# Task 4 (Power set)

For this task use only `append` and `distinct` and implement helper predicate `add_first_to_each_list`.

Implement predicate `power_set(P, S)` with the following semantic: when given a list `S` of distinct elements, `P` is the power set of `S`.

`power_set(P, S)` should generate in `P` the power set of `S` when all elements of `S` are distinct the order does not matter (we don't care about it).

Hint: Use `distinct` to check if elments of `S` are all distinct.

Example: `?- power_set(X, [a, b]).` answer: `X = [[a, b], [a], [b], []]`.

`add_first_to_each_list(X, Ls, Rs)` should be recursive predicate which adds `X` as first element to each list in `Ls` and the result is `Rs`.

It should be possible to be used as decider and as well as 1-generator (generator with one answer) on each argument.

Examles:

`?- add_first_to_each_list(b, [[a]], [[b, a]]).` aswer: `true`.

`?- add_first_to_each_list(X, [[a]], [[b, a]]).` anser: `X = b`.

`?- add_first_to_each_list(b, X, [[b, a]]).` aswer: `X = [[a]]`.

`?- add_first_to_each_list(b, [[a]], X).` aswer: `X = [[b, a]]`.
