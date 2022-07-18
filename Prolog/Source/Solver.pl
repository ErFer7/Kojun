% Solver
% Resolve o puzzle

:- module(solver, [solve/1]).

:- use_module(library(clpfd)).

% Solução ---------------------------------------------------------------------
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

% Regra de intervalo
% interval(Puzzle, X, Y).  % Não implementada.

% Regra de gradeza vertical
vertical_greatness(Puzzle, X, Y) :-
    % Obtém o tamanho
    length(Puzzle, Size),
    % Obtém a célula na posição (x, y)
    nth0(Y, Puzzle, Line),
    nth0(X, Line, [V, R]),
    % Obtém a célula acima
    (   Y > 0
        ->  Yup is Y - 1,  % Yup
        nth0(Yup, Puzzle, LineAbove),
        nth0(X, LineAbove, [Vup, Rup])
    ;   Rup = -1
    ),
    % Obtém a célula abaixo
    (   Y < (Size - 1)
    ->  Ydown is Y + 1,
        nth0(Ydown, Puzzle, LineUnder),
        nth0(X, LineUnder, [Vdown, Rdown])
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
    % Obtém a célula na posição (x, y)
    nth0(Y, Puzzle, Line),
    nth0(X, Line, Cell),
    % Planifica o puzzle
    flat(Puzzle, FlatPuzzle),
    remove(Cell, FlatPuzzle, CompPuzzle),
    maplist(dif(Cell), CompPuzzle).

% Em construção
solve(Puzzle) :-
    unique(Puzzle, 0, 0).

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

% % conta elementos em regiao
% elemsInRegion(_,[],C):- C = 0.
% elemsInRegion(R,[[_,R]|T],C):- elemsInRegion(R,T,C1), C = C1+1.
% elemsInRegion(R,[_|T],C):- elemsInRegion(R,T,C).

% % verifica se todos estao em 1:N em uma regiao
% allOneThroughN([],_).
% allOneThroughN([[V1,R1]|T],L):- elemsInRegion(R1,L,C),
%                                 V1 #=< C,
%                                 V1 #>= 1,
%                                 allOneThroughN(T,L).

% % aplica funcao acima a partir de matriz original
% allBelowN(P):- allOneThroughN(flatten(P),flatten(P)).


% % funcoes para evitar valor maior que a posicao inferior

% % checa se valor de um elemento eh maior q o proximo se for mesma regiao
% checkColumn([_]).
% checkColumn([[V1,R],[V2,R]|T]):- V1 #> V2, checkColumn([[V2,R]|T]).
% checkColumn([_|T]):- checkColumn([T]).

% % aplica funcao acima a cada coluna
% vgAux([C]):- checkColumn(C).
% vgAux([C|T]):- checkColumn(C), vgAux(T).

% % aplica checagem para a transposta, checando coluna e nao linha
% verticalGreatness(M):- vgAux(transpose(M)).


orthogonal_difference(Puzzle,X,Y) :-
    length(Puzzle,Size),
    % Pega celula na posicao (X,Y)
    nth0(Y,Puzzle,Line),
    nth0(X,Puzzle,Cell),
    % Acima
    ( Y > 0
    -> Yup = Y - 1,
    nth0(Yup,Puzzle,LineAbove),
    nth0(X,LineAbove,[Vup,Rup])
    ; Vup = -1
    ),
    % Abaixo
    ( Y < (Size - 1)
    -> Ydown = Y + 1,
    nth0(Ydown,Puzzle,LineBelow),
    nth0(X,LineBelow,[Vdown,Rdown])
    ; Vdown = -1
    ),
    % Esquerda
    ( X > 0
    -> XLeft = X - 1,
    nth0(XLeft,Line,[Vleft,Rleft])
    ; Vleft = -1
    ),
    % Direita
    ( X < (Size - 1),
    XRight = X + 1,
    nth0(XRight,Line,[Vright,Rright])
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
