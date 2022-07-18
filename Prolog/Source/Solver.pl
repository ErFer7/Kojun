% Solver
% Resolve o puzzle

:- module(solver, [solve/1]).

:- use_module(library(clpfd)).

% Solução ---------------------------------------------------------------------

% auxiliares de pegar valores
get_cell_value(Puzzle,X,Y,V):-
    get_cell(Puzzle,X,Y,[V,_]).

get_cell_region(Puzzle,X,Y,R):-
    get_cell(Puzzle,X,Y,[_,R]).

get_cell(Puzzle,X,Y,[V,R]):-
    nth0(Y, Puzzle, Line),
    nth0(X, Line, [V, R]).

% verifica se dentre uma regiao, uma celula eh maior que a inferior
vertical_greatness(Puzzle, X, Y) :-
    %
    length(Puzzle, Size),
    % Obtém a célula na posição (x, y)
    get_cell(Puzzle,X,Y,[V,R]),
    % Obtém a célula acima
    (   Y > 0
        ->  Yup is Y - 1,  % Yup
        get_cell_region(Puzzle,X,Yup,Rup)
    ;   Rup = -1
    ),
    % Obtém a célula abaixo
    (   Y < (Size - 1)
    ->  Ydown is Y + 1,
        get_cell_region(Puzzle,X,Ydown,Rdown)
    ;   Rdown = -1
    ),
    % Regras de verificação
    (   R == Rup
    ->  V < Vup
    ;   true
    ),
    (   R == Rdown
    ->  V > Vdown
    ;   true
    ).

% Em construção
solve(Puzzle) :-
    vertical_greatness(Puzzle, 5, 0),
    true.

% COMENTADO -------------------------------------------------------------------

% funcoes para evitar repeticoes [valor,regiao]

% transforma matriz em lista unidimensional

% oneDimentional([H],H):-!.
% oneDimentional([H|T],Line_matrix):-
%     line_matrix1 = oneDimentional(T,Line_matrix1),
%     line_matrix = concatenate(H,Line_matrix1,Line_matrix).

% % encontra duplicatas em lista
% findDupes([H|T]):- member(H,T);
%                    findDupes(T).

% % verifica se cada elemento eh unico
% allUnique(M):- Line_matrix = oneDimentional(M,Line_matrix),
%                #\ findDupes(Line_matrix).


% % funcoes para manter valores entre 1 e N


below_N(Puzzle,X,Y):- 
    get_cell(Puzzle,X,Y,[V,R]),
    get_region(Puzzle,R). % continua...
    

% verifica diferenca entre casas adjacentes
orthogonal_difference(Puzzle,X,Y) :-
    length(Puzzle,Size),
    % Pega celula na posicao (X,Y)
    get_cell_value(Puzzle,X,Y,V)
    % Acima
    ( Y > 0
    -> Yup = Y - 1,
    get_cell_value(Puzzle,X,Yup,Vup)
    ; Vup = -1
    ),
    % Abaixo
    ( Y < (Size - 1)
    -> Ydown = Y + 1,
    get_cell_value(Puzzle,X,Ydown,Vdown)
    ; Vdown = -1
    ),
    % Esquerda
    ( X > 0
    -> XLeft = X - 1,
    get_cell_value(Puzzle,XLeft,Y,Vleft)
    ; Vleft = -1
    ),
    % Direita
    ( X < (Size - 1),
    XRight = X + 1,
    get_cell_value(Puzzle,XRight,Y,Vright)
    ; Vright = -1
    ),
    % Regras
    V \= Vup,
    V \= Vdown,
    V \= Vleft,
    V \= Vright.

% funcao teste para o tabuleiro inteiro



orthogonal_loop_y(Puzzle,X,Y):-
    length(Puzzle,Size),
    orthogonal_loop_x(Puzzle,X,Y),
    (Y < (Size - 1)
    -> Y1 is Y + 1,
    orthogonal_loop_y(Puzzle,X,Y1)
    ; true
    ).

orthogonal_loop_x(Puzzle,X,Y):-
    length(Puzzle,Size),
    orthogonal_difference(Puzzle,X,Y),
    (X < (Size - 1)
    -> X1 is X + 1,
    orthogonal_loop_x(Puzzle,X1,Y),
    ; true
    ).

orthogonalDifference(Puzzle):- 
    orthogonal_loop_y(Puzzle,0,0).


% % acha solucao com todas as condicoes acima validas

% solved_puzzle(P):-allUnique(P),
%                   verticalGreatness(P),
%                   allBelowN(P),
%                   orthogonalDifference(P).

% (i (am (starting((to miss) this) now))
% msg: .asciiz "I prefer MIPS assembly over Prolog"
