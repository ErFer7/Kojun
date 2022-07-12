% Solver
% Resolve o puzzle


% funcoes para evitar repeticoes [valor,regiao]

% concatena lista de listas
concatAll([L1,L2],R):- concatenate(L1,L2,R)!.
concatAll([L|T],R):- R1 is concatAll(T,R1), concatenate(T,R1,R).

% encontra duplicatas em lista
not findDupes(_,[])!.
findDupes(X,[H|T]):- contains(H,T); findDupes(T).

% encontra se elemento esta na lista
not contains(_,[])!.
contains(X,[H|T]):- X==H; contains(X,T).

% verifica se cada elemento eh unico
allUnique(M):- line_matrix is concatAll(M,line_matrix), not findDupes(line_matrix)


% funcoes para evitar valor maior que a posicao inferior

% checa se valor de um elemento eh maior q o proximo se for mesma regiao
checkColumn([_])!.
checkColumn([[V1,R],[V2,R]|T]):- V1>V2, checkColumn([[V2,R]|T])!.
checkColumn([_|T]):- checkColumn([T]).

% aplica funcao acima a cada coluna
vgAux([C]):- checkColumn(C)!.
vgAux([C|T]):- checkColumn(C), vgAux(T).

% aplica checagem para a transposta, checando coluna e nao linha
verticalGreatness(M):- vgAux(transpose(M)).


% funcoes para checar diferenca entre valores adjacentes

% verifica se valores em uma linha sao diferentes dos vizinhos
checkLine([_])!.
checkLine([[V1,_],[V2,R]|T]):- V1=\=V2,checkLine([[V2,R]|T]).

% aplica funcao acima a cada linha
directionDif([H]):- checkLine(H)!.
directionDif([H|T]):- checkLine(H), directionDif(T).

% aplica checagem a original e transposta para checar duas direcoes
orthogonalDifference(M):- directionDif(M), directionDif(transpose(M)).


% acha solucao com todas as condicoes acima validas

solvedPuzzle(P):- allUnique(P),
                  verticalGreatness(P),
                  orthogonalDifference(P).

