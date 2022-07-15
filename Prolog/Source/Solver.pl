% Solver
% Resolve o puzzle

:- module(solver, [solved_puzzle/1]).

:- use_module(library(clpfd)).
% funcoes para evitar repeticoes [valor,regiao]

% transforma matriz em lista unidimensional

oneDimentional([H],H):-!.
oneDimentional([H|T],Line_matrix):-
    line_matrix1 = oneDimentional(T,Line_matrix1),
    line_matrix = concatenate(H,Line_matrix1,Line_matrix).

% encontra duplicatas em lista
findDupes([H|T]):- member(H,T);
                   findDupes(T).

% verifica se cada elemento eh unico
allUnique(M):- Line_matrix = oneDimentional(M,Line_matrix),
               #\ findDupes(Line_matrix).


% funcoes para manter valores entre 1 e N

% conta elementos em regiao
elemsInRegion(_,[],C):- C = 0,!.
elemsInRegion(R,[[_,R]|T],C):- C1 = elemsInRegion(R,T,C1), C = C1+1,!.
elemsInRegion(R,[_|T],C):- C = elemsInRegion(R,T,C).

% verifica se todos estao em 1:N em uma regiao
allOneThroughN([],_).
allOneThroughN([[V1,R1]|T],L):- C = elemsInRegion(R1,L,C),
                                V1 #=< C,
                                V1 #>= 1,
                                allOneThroughN(T,L).

% aplica funcao acima a partir de matriz original
allBelowN(P):- allOneThroughN(flatten(P),flatten(P)).


% funcoes para evitar valor maior que a posicao inferior

% checa se valor de um elemento eh maior q o proximo se for mesma regiao
checkColumn([_]):-!.
checkColumn([[V1,R],[V2,R]|T]):- V1 #> V2, checkColumn([[V2,R]|T]),!.
checkColumn([_|T]):- checkColumn([T]).

% aplica funcao acima a cada coluna
vgAux([C]):- checkColumn(C),!.
vgAux([C|T]):- checkColumn(C), vgAux(T).

% aplica checagem para a transposta, checando coluna e nao linha
verticalGreatness(M):- vgAux(transpose(M)).


% funcoes para checar diferenca entre valores adjacentes

% verifica se valores em uma linha sao diferentes dos vizinhos
checkLine([_]):-!.
checkLine([[V1,_],[V2,R]|T]):- V1 #\= V2,checkLine([[V2,R]|T]).

% aplica funcao acima a cada linha
directionDif([H]):- checkLine(H),!.
directionDif([H|T]):- checkLine(H),
                      directionDif(T).

% aplica checagem a original e transposta para checar duas direcoes
orthogonalDifference(M):- directionDif(M),
                          directionDif(transpose(M)).


% acha solucao com todas as condicoes acima validas

solved_puzzle(P):-allUnique(P),
                  verticalGreatness(P),
                  allBelowN(P),
                  orthogonalDifference(P).

% (i (am (starting((to miss) this) now))
