prox_diff(List, N) :-
    nth0(N, List, X),
    Nb is N - 1,
    Nn is N + 1,
    nth0(Nb, List, Y),
    nth0(Nn, List, Z),
    X \== Y,
    X \== Z.

main :-
    use_module(library(clpfd)),
    prox_diff([1, 2, 3, 4, 5], N),
    write(N),
    halt.
