% Printer
% Exibição de dados

:- module(printer, [print_puzzle/1]).

% Auxiliares ------------------------------------------------------------------
% Exibe a linha
print_line([_]).
print_line([]).
print_line([H|T]) :-
    nth0(0, H, V),  % Obtém o valor a ser exibido
    write(V),       % Exibe uma célula no console
    write(' '),     % Espaço entre as células
    print_line(T).

% Exibição de dados -----------------------------------------------------------
% Exibe o puzzle
print_puzzle([_]).
print_puzzle([]).
print_puzzle([H|T]) :-
    print_line(H),  % Exibe uma linha
    write('\n'),    % Newline para a formatação
    print_puzzle(T).
