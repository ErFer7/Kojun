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

    read_file('Puzzles/Kojun_1.txt', Puzzle),  % Lê o puzzle
    print_puzzle(Puzzle),

    write('\nResolvendo puzzle...\n\n'),

    solve(Puzzle, 0, 0),  % Resolve o puzzle
    print_puzzle(Puzzle),
    halt.
