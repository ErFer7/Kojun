basic_rule(X, Y) :-
    between(0, Y, X).

% rule(List, N) :-
%     nth0(N, List, X),
%     Nb is N - 1,
%     Nn is N + 1,
%     nth0(Nb, List, Y),
%     nth0(Nn, List, Z),
%     basic_rule(X, Y, Z).

maplist(Predicate, [A]).
maplist(Predicate, [H|T]) :-
    nth0(0, T, M),
    call(Predicate, H, M),
    maplist(Predicate, T).

correct_list(CorrectList, Guess) :-
    maplist(basic_rule, CorrectList),
    Guess = CorrectList.

% prox_diff(List, N) :-
%     nth0(N, List, X),
%     Nb is N - 1,
%     Nn is N + 1,
%     nth0(Nb, List, Y),
%     nth0(Nn, List, Z),
%     X \== Y,
%     X \== Z.

main :-
    use_module(library(clpfd)),
    correct_list([A, 2, 3, 4, 5], Guess),
    write(Guess),
    halt.
