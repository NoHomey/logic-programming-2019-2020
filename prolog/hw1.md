# Task 1

Implement predicate `nat(X)` which decides "nats" when used as a decider (yields `true` if and only if `X` is one of "nats")
and can generate all of them when used as generator.

"nats" are: `zero`, `s(zero)`, `s(s(zero))` and so on ...


# Task 2

Implement predicate `add(X, Y, Z)` which acts as addition of "nats":

Example:

`?- add(s(s(zero)), s(zero), R).`
answer: `R = s(s(s(zero)))`.

Run the following queries:

`?- add(s(s(zero)), Y, Z).`

`?- add(X, s(s(zero)), Z).`

For which Prolog has only one answer and for which has infinitely many answers (can generate more then one answer) ?

How do you interpretate the result of each query ?

If there was a query with only one answer try to reimplement `add(X, Y, Z)` in such a way that both of the above queries have infinitely many answers. 

# Task 3

Implement predicate `append(X, Y, Z)` semantic of which is: `Z` is the concatenation of `X` and `Y`.

Example:

`?- append([], [a, b, c], [a, b, c])` answer: `true`.

`?- append([a, b, c], [], [a, b, c])` answer: `true`.

`?- append([s, t], [a, b, c], X)` answer: `X = [s, t, a, b, c]`.

# Task 4

Implemnt `member(X, L)` using `append`.

# Task 5

Implemnet `reverse(X, Y)` with the following semantic `Y` is the reverse of `X`.

Examples:

`?- reverse([a, b, c], [c, b, a]).` answer: `true`.

`?- reverse([a, b, c], X).` answer: `X = [c, b, a]`.

`?- reverse(X, [c, b, a]).` answer: `X = [a, b, c]`.
