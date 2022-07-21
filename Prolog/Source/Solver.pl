% Solver
% Resolve o puzzle

:- module(solver, [solve/3]).

:- use_module(library(clpfd)).

% Auxiliares ------------------------------------------------------------------
% Obtenção de célula
get_cell(Puzzle,X,Y,[V,R]):-
    nth0(Y, Puzzle, Line),
    nth0(X, Line, [V, R]).

% Obtenção de valor
get_cell_value(Puzzle,X,Y,V):-
    get_cell(Puzzle,X,Y,[V,_]).

% Obtenção de região
get_cell_region(Puzzle,X,Y,R):-
    get_cell(Puzzle,X,Y,[_,R]).

% Planifica uma lista em apenas um nível
flat([], []).
flat([H|T], Out) :-
    is_list(H),   % Verifica se a cabeça da lista é uma lista
    flat(T, T1),  % Planifica recusrivamente
    !,
    append(H, T1, Out).  % Adiciona a nova lista na lista atual
    flat(H, H).

% Remove uma célula da lista
remove(_, [], []).          % Caso base
remove(Cell, [Cell|T], T).  % Remove uma célula na lista que bate com a célula informada
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
    % Regras
    V #> 0,
    V #=< Size.

% verifica se dentre uma região, uma célula é maior que a inferior
vertical_greatness(Puzzle, X, Y) :-
    % Obtém o tamanho
    length(Puzzle, Size),
    % Obtém a célula na posição (x, y)
    get_cell(Puzzle,X,Y,[V, R]),
    % Obtém a célula acima
    (   Y #> 0
        ->  Yup #= Y - 1,  % Yup
        get_cell_value(Puzzle, X, Yup, Vup),
        get_cell_region(Puzzle, X, Yup, Rup)
    ;   Rup #= -1
    ),
    % Obtém a célula abaixo
    (   Y #< (Size - 1)
    ->  Ydown #= Y + 1,
        get_cell_value(Puzzle, X, Ydown, Vdown),
        get_cell_region(Puzzle, X, Ydown, Rdown)
    ;   Rdown #= -1
    ),
    % Regras de verificação
    (   R #= Rup
    ->  V #< Vup
    ;   true
    ),
    (   R #= Rdown
    ->  V #> Vdown
    ;   true
    ).

% Regra de valor único na região.
unique(Puzzle, X, Y) :-
    get_cell(Puzzle, X, Y, Cell),
    flat(Puzzle, FlatPuzzle),          % Planifica o puzzle
remove(Cell, FlatPuzzle, CompPuzzle),  % Remove a célula (x, y)
    maplist(dif(Cell), CompPuzzle).    % Verifica se a célula ainda pode ser encontrada

% verifica diferenca entre casas adjacentes
orthogonal_difference(Puzzle,X,Y) :-
    length(Puzzle,Size),
    % Pega celula na posicao (X,Y)
    get_cell_value(Puzzle, X, Y, V),
    % Acima
    (   Y #> 0
    ->  Yup #= Y - 1,
        get_cell_value(Puzzle, X, Yup, Vup)
    ;   Vup #= -1
    ),
    % Abaixo
    (   Y #< (Size - 1)
    ->  Ydown #= Y + 1,
        get_cell_value(Puzzle, X, Ydown, Vdown)
    ;   Vdown #= -1
    ),
    % Esquerda
    (   X #> 0
    ->  XLeft #= X - 1,
        get_cell_value(Puzzle, XLeft, Y, Vleft)
    ;   Vleft #= -1
    ),
    % Direita
    (   X #< (Size - 1)
    ->  XRight #= X + 1,
        get_cell_value(Puzzle, XRight, Y, Vright)
    ;   Vright #= -1
    ),
    % Regras
    V #\= Vup,
    V #\= Vdown,
    V #\= Vleft,
    V #\= Vright.

% Regras para que uma célula seja válida
valid_cell(Puzzle, X, Y) :-
    interval(Puzzle, X, Y),
    unique(Puzzle, X, Y),
    vertical_greatness(Puzzle, X, Y),
    orthogonal_difference(Puzzle, X, Y).

% Solução ---------------------------------------------------------------------
% Resolve o puzzle
solve(Puzzle, X, Y) :-
    length(Puzzle, Size),        % Obtém o tamanho
    valid_cell(Puzzle, X1, Y1),  % Define a relação de verificação
    % Incrementação de coordenadas
    (   X #= Size
    ->  X1 #= 0,
        Y1 #= Y + 1
    ;   X1 #= X + 1
    ),
    (   Y1 #< Size
    ->  solve(Puzzle, X1, Y1)
    ).
