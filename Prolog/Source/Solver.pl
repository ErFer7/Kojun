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
% Planifica uma lista em apenas um nível
flat([], []).
flat([H|T], Out) :-
    is_list(H),
    flat(T, T1),
    !,
    append(H, T1, Out). 
    flat(H, H).

% Remove uma célula da lista
remove(_, [], []).
remove(Cell, [Cell|T], T).
remove(Cell, [H|T], [H,R]) :-
    remove(Cell, T, R).

% Pega lista de celulas na regiao R
get_region_list(Puzzle, R, RegionCells):-
    findall([V, R], (get_cell(Puzzle, _, _, [V, R])), RegionCells).

% Regra de intervalo: valor deve estar entre 1 e N em regiao com N 
interval(Puzzle, X, Y):-
    get_cell(Puzzle,X,Y,[V,R]),
    get_region_list(Puzzle,R,RegionCells),
    length(RegionCells,Size),
    V > 0,
    V =< Size.

% Regra de gradeza vertical
vertical_greatness(Puzzle, X, Y) :-
    % Obtém o tamanho
    length(Puzzle, Size),
    % Obtém a célula na posição (x, y)
    get_cell(Puzzle,X,Y,[V, R]),
    % Obtém a célula acima
    (   Y > 0
        ->  Yup is Y - 1,  % Yup
        get_cell_value(Puzzle, X, Yup, Vup),
        get_cell_region(Puzzle, X, Yup, Rup)
    ;   Rup = -1
    ),
    % Obtém a célula abaixo
    (   Y < (Size - 1)
    ->  Ydown is Y + 1,
        get_cell_value(Puzzle, X, Ydown, Vdown),
        get_cell_region(Puzzle, X, Ydown, Rdown)
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

% Regra de valor único na região.
unique(Puzzle, X, Y) :-
    get_cell(Puzzle, X, Y, Cell),
    % Planifica o puzzle
    flat(Puzzle, FlatPuzzle),
    remove(Cell, FlatPuzzle, CompPuzzle),
    maplist(dif(Cell), CompPuzzle).

% verifica diferenca entre casas adjacentes
orthogonal_difference(Puzzle,X,Y) :-
    length(Puzzle,Size),
    % Pega celula na posicao (X,Y)
    get_cell_value(Puzzle,X,Y,V),
    % Acima
    ( Y > 0
    -> Yup is Y - 1,
    get_cell_value(Puzzle,X,Yup,Vup)
    ; Vup = -1
    ),
    % Abaixo
    ( Y < (Size - 1)
    -> Ydown is Y + 1,
    get_cell_value(Puzzle,X,Ydown,Vdown)
    ; Vdown = -1
    ),
    % Esquerda
    ( X > 0
    -> XLeft is X - 1,
    get_cell_value(Puzzle,XLeft,Y,Vleft)
    ; Vleft = -1
    ),
    % Direita
    ( X < (Size - 1),
    XRight is X + 1,
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
    (   Y < (Size - 1)
        -> Y1 is Y + 1,
        orthogonal_loop_y(Puzzle,X,Y1)
    ;   true
    ).

orthogonal_loop_x(Puzzle,X,Y):-
    length(Puzzle,Size),
    orthogonal_difference(Puzzle,X,Y),
    (   X < (Size - 1)
    ->  X1 is X + 1,
        orthogonal_loop_x(Puzzle,X1,Y)
    ;   true
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

% Em construção
solve(Puzzle) :-
    orthogonal_difference(Puzzle, 1, 3).
