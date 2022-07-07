/*  Trabalho 3 de Paradigmas de Programação
    Grupo: Eric e Otávio
*/

% Carrega todos os arquivos com o código fonte
load_modules :-
    use_module('Source/Parser').

main :-
    load_modules,
    write('----------------\nTrabalho 3\nEric e Otavio\n----------------'),
    read_file('Puzzles/Kojun_12.txt', List),
    % Testes
    nth0(5, List, Sublist),
    write('\n'),
    write(Sublist),
    nth0(5, Sublist, Elem),
    write('\n'),
    write(Elem),
    write('\n'),
    halt.
