/*  Trabalho 3 de Paradigmas de Programação
    Grupo: Eric e Otávio
*/

% Carrega todos os arquivos com o código fonte
load_modules :-
    use_module('Source/Parser'),
    use_module('Source/Solver').

main :-
    load_modules,
    write('----------------\nTrabalho 3\nEric e Otavio\n----------------\n\n'),
    read_file('Puzzles/Kojun_12.txt', List),
    % Testes %
    write(List),
    write('\n\n'),
    solved_puzzle(List),
    write(List),
    write('\n\n'),
    nth0(0, List, Sublist),
    write('\n\n'),
    write(Sublist),
    nth0(0, Sublist, Elem),
    write('\n\n'),
    write(Elem),
    write('\n\n'),
    halt.
