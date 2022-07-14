/*  Trabalho 3 de Paradigmas de Programação
    Grupo: Eric e Otávio
*/

% Carrega todos os arquivos com o código fonte
load_modules :-
    use_module('Source/Parser'),
    use_module('Source/Printer'),
    use_module('Source/Solver').

main :-
    load_modules,
    write('----------------\nTrabalho 3\nEric e Otavio\n----------------\n\n'),

    read_file('Puzzles/Kojun_12.txt', List),
    print_puzzle(List),

    write('\n\nResolvendo puzzle...\n\n'),

    solved_puzzle(List),
    print_puzzle(List),
    halt.
