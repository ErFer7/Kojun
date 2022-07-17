test_list([_, 1, 2, _]).

unique(List) :-
    all_distinct(List).

main :-
    use_module(library(clpfd)),
    write('TESTE\n\n'),

    unique([_, 1, 2, _]),

    halt.
