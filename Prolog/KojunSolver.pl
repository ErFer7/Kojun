/*  Trabalho 3 de Paradigmas de Programação
    Grupo: Eric e Otávio
*/

% Carrega todos os arquivos com o código fonte
load_files :- ['Source/Structure'],
              ['Source/Parser'],
              ['Source/Printer'],
              ['Source/Solver'].

main :- load_files,
        write('----------------\nTrabalho 3\nEric e Otavio\n----------------'),
        halt.
