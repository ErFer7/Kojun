% Parser
% Leitura e processamento de dados

:- module(parser, [read_file/2]).

% Auxiliares ------------------------------------------------------------------
% Lê uma stream de caracteres e constrói a estrutura
read_stream(In, L) :-
    % Lê uma parte do arquivo
    read_term(In, H, []),
    % Verifica se o arquivo acabou
    (   H == end_of_file
    ->  L = []
    ;   L = [H|T],
        read_stream(In, T)
    ).

% Leitura da estrutura --------------------------------------------------------
% Lê um arquivo
read_file(File, L) :-
    setup_call_cleanup(
        open(File, read, In),
        read_stream(In, L),  % Faz a leitura
        close(In)
    ).
