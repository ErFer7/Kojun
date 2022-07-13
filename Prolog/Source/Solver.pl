% Solver
% Resolve o puzzle


% funcoes para evitar repeticoes [valor,regiao]

% encontra duplicatas em lista
not findDupes([_]):-!.
findDupes([H|T]):- member(H,T);
                   findDupes(T).

% verifica se cada elemento eh unico
allUnique(M):- line_matrix is flatten(M,line_matrix),
               not findDupes(line_matrix).


% funcoes para manter valores entre 1 e N

% conta elementos em regiao
elemsInRegion(R,[],C):- C is 0,!.
elemsInRegion(R,[[V,R]|T],C):- C1 is elemsInRegion(R,T,C1), C is C1+1.
elemsInRegion(R,[H|T],C):- C is elemsInRegion(R1,T,C).

% verifica se todos estao em 1:N em uma regiao
allOneThroughN([],L).
allOneThroughN([[V1,R1]|T],L):- C is elemsInRegion(R1,L,C),
                                V1 <= C,
                                V1 >= 1,
                                allOneThroughN(T,L).

% aplica funcao acima a partir de matriz original
allBelowN(P):- allOneThroughN(flatten(P),flatten(P)).


% funcoes para evitar valor maior que a posicao inferior

% checa se valor de um elemento eh maior q o proximo se for mesma regiao
checkColumn([_]):-!.
checkColumn([[V1,R],[V2,R]|T]):- V1>V2, checkColumn([[V2,R]|T]),!.
checkColumn([_|T]):- checkColumn([T]).

% aplica funcao acima a cada coluna
vgAux([C]):- checkColumn(C),!.
vgAux([C|T]):- checkColumn(C), vgAux(T).

% aplica checagem para a transposta, checando coluna e nao linha
verticalGreatness(M):- vgAux(transpose(M)).


% funcoes para checar diferenca entre valores adjacentes

% verifica se valores em uma linha sao diferentes dos vizinhos
checkLine([_]):-!.
checkLine([[V1,_],[V2,R]|T]):- V1=\=V2,checkLine([[V2,R]|T]).

% aplica funcao acima a cada linha
directionDif([H]):- checkLine(H),!.
directionDif([H|T]):- checkLine(H),
                      directionDif(T).

% aplica checagem a original e transposta para checar duas direcoes
orthogonalDifference(M):- directionDif(M),
                          directionDif(transpose(M)).


% acha solucao com todas as condicoes acima validas

solvedPuzzle(P):- allUnique(P),
                  verticalGreatness(P),
                  allBelowN(P),
                  orthogonalDifference(P).
