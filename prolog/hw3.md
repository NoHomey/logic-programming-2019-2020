# Task 1

Define predicate `gcd(D, A, B)` which is true when A is not zero or B is not zero
and D is the greatest common devisitor of A and B.

# Task 2

For this task we will use terms to denote rational numbers. The term `ratio(P, Q)` where `P` is integer,
`Q` is nonzero natural number and it is true `gcd(1, P, Q)` will encode the rational number `P / Q`.

Define predicate `is_ratio(X)` which is true if `X` encodes a rational number.

Define predicate `min_ratio(X, A, B)` which is true
when `is_ratio(A)` and `is_ratio(B)` are true and `X` encodes the minimum of `A` and `B`.

Define predicate `sum_ratios(S, A, B)` which is true
when `is_ratio(A)` and `is_ratio(B)` are true and `X` encodes the sum of `A` and `B`.

# Task 3

Make `mod(R, N, K)` work when it is true `N < 0` (N is negative integer).

# Task 4

Define predicate `char_vects(S, N)` which given natural number `N` generates in `S` the set of
all lists of length `N` of `0`s and `1`s.

Define predicate `char_vect(V, N)` which given natural number `N` generates in `V` one by one each element of `S` where for `S` is true `char_vects(S, N)`.

# Task 5

Define predicate `arithmetic_progression(X)` which generates in `X` all finite arithmetic progression.
Finite arithmetic progression is any subsequnce of an arithmetic progression
which is finite sequence and it is also arithmetic progression.

Examples for finite arithmetic progressions: `[0]`, `[1, 1]`, `[-1, 2]`, `[2, 5, 8]`.

# Task 6

Define predicate `fin_subset_Nat(X)` which generates in `X` all finite subsets of natural numbers.
Represent each finite subset as list which has no repetive elements
and ensure that once `X` is generated no permutation of it is generated as well.

This means that `[1, 1]` should not be generated and
if `[1, 2]` is generated than `[2, 1]` is not
and if `[2, 1]` is generated than `[1, 2]` is not.

# Task 7

Define predicate `binary_tree(T)` which generates in `T` all finite binary trees with vertices natural numbers.
Choose representation for finite binary tree.

# Task 8

Define predicate `prime_binary_tree(T)` which generates in `T` all finite binary trees with vertices prime numbers.

# Task 9

Define predicate `hamiltonian_path(P, G)` which generates in `P` each hamiltonian path in `G`.
`G` is representation of undirected graph as follows `G = [V, E]`
where `V` is list of vertices and `E` is list of edges. If there is edge from `X` to `Y` then either `[X, Y]` is member of `E` or `[Y, X]` but not the two.

# Task 10

Define predicate `is_hamiltonian(G)` which checks if `G` is hamiltonian graph (`G` represents graph which has hamiltonian cycle).

# Task 11

Define predicate `gen_hamiltonian(V, G)` which generates in `G` all graphs that are hamiltonian and their vertices are subsequnce of list `V`.

# Task 12

Define predicate `gen_hamiltonian_Nat(G)` which generates in `G` all graphs that are hamiltonian and their vertices are natural numbers.
