prox_diff(List, N) :-
    nth0(N, List, X),
    Nb is N - 1,
    Nn is N + 1,
    nth0(Nb, List, Y),
    nth0(Nn, List, Z),
    X \== Y,
    X \== Z.

left(List, N, X) :-
    X is nth0(N - 1, List, X).

main :-
    use_module(library(clpfd)),
    left([1, 2, 3], 1, X),
    write(X),
    halt.
