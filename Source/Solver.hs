-- Solver
-- Resolve o puzzle.

{-
Algoritmo:

1) Repetir ate uma iteracao nao causar mudanca alguma
    a) Celulas com apenas um valor possivel sao fixos nesta
    b) Regioes com uma celula compativel com um valor a fixam neste
    c) Regioes com uma coluna livre a preenchem com valores de baixo pra cima
    d) Recalcular todos os valores possiveis para cada celula nao fixa
Isso melhora a quantidade de solucoes possiveis

2) Testar possiveis solucoes com algoritmo de backtracking
-}

module Solver(checkCell, getPossibleValues) where

import Structure
import Data.List (permutations)

-- Auxiliares ------------------------------------------------------------------
-- Conta quantas vezes um valor aparece na lista
count :: Int -> [Int] -> Int
count v [] = 0
count v (a:b)
    | a == v = 1 + count v b
    | otherwise = count v b

-- Checa se um valor não se repete na região (Exceto 0)
checkRegionRepetition :: [Int] -> Bool
checkRegionRepetition [] = True
checkRegionRepetition (a:b) =
    if a /= 0 then
        if count a (a:b) == 1 then
            checkRegionRepetition b
        else
            False
    else
        checkRegionRepetition b

-- Checa se todas as células adjacentes são diferentes
checkOrthogonalDifference :: Int -> Int -> Puzzle -> Bool
checkOrthogonalDifference x y puzzle
    | (y + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y + 1) puzzle)) &&
      (y - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y - 1) puzzle)) &&
      (x + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x + 1) y puzzle)) &&
      (x - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x - 1) y puzzle)) = True
    | otherwise = False

-- Checa se a célula superior é maior que a atual
checkVerticalGreatness :: Int -> Int -> Puzzle -> Bool
checkVerticalGreatness x y puzzle
    | y - 1 < 0 ||
      getCellValue (getCell2D x y puzzle) < getCellValue (getCell2D x (y - 1) puzzle) ||
      getCellRegion (getCell2D x y puzzle) /= getCellRegion (getCell2D x (y - 1) puzzle) = True
    | otherwise = False

-- Checa todas as regras para uma célula
checkCell :: Int -> Int -> Region -> Puzzle -> Bool
checkCell x y region puzzle
    | checkRegionRepetition (getValuesInRegion region puzzle) &&
      checkOrthogonalDifference x y puzzle &&
      checkVerticalGreatness x y puzzle = True
    | otherwise = False

-- Obtém os valores possíveis para uma região com base nos valores dela
getPossibleValues :: Int -> [Int] -> [Int]
getPossibleValues i values
    | i - 1 >= length values = []
    | count i values == 0 = [i] ++ getPossibleValues (i + 1) values
    | otherwise = getPossibleValues (i + 1) values

-- Cria lista de permutacoes para os valores possiveis de uma regiao
getPossibleValuesPermutation ::  Region -> Puzzle -> [[Int]]
getPossibleValuesPermutation (n,[]) _ = []
getPossibleValuesPermutation r puzzle =
    permutations values where
        values = getPossibleValues 1 (getValuesInRegion r puzzle)

-- Testa se todos os valores, apos insercao, sao validos
testingRegion :: Region -> Puzzle -> Int -> Bool
testingRegion (regSize,[a:b]) (puzSize,cellList) iter
    | (iter == regSize) = True
    | (checkCell (mod a puzSize) (div a puzSize) == False) = False
    | otherwise = testingRegion (a,c) puzzle iter

-- coloca n valores em n celulas
fillRegionWithValues :: [Int] -> [Int] -> Puzzle -> Bool
fillRegionWithValues [] [] _ = False
fillRegionWithValues [a,values] [b,coords] puzzle = do
    setCellValue b a puzzle
    fillRegionWithValues values coords puzzle
    return True

{-
    metodo backtracking, recebe regiao e puzzle, retorna bool
    passo 1) preencher uma regiao com valores validos
    passo 2) checar se valores sao validos
    passo 3)
      se valido, backtracking da regiao seguinte
        se nao houver regiao reguinte, retorna verdadeiro
      se nao valido, retorna falso
    passo 4) se retornou falso,
      se ha mais permutacoes possiveis, volta pro comeco e tenta proxima permutacao
      de valores validos
      se nao, retorna falso
-}
backtracking :: Region -> Puzzle -> Puzzle
backtracking _ p = p




-- TODO: Função para escolher um valor aleatório em uma lista e inserir no puzzle
-- TODO: Função monad que chama a função de inserção e checa a lista, fazendo o backtracking

--------------------------------------------------------------------------------

{-
getPossibleAt :: Int -> [Int]
getPossibleAt i =
    let cell = getCell i
    if getCellValue cell /= 0 then
        []
    else
        let region = getCellRegion cell
        removeSubList [1..length region] iterRegionCells regions

-- Retorna lista de listas de valores possiveis para cada celula
getPossibleValues :: Puzzle -> [[Int]]
getPossibleValues (_,[]) = []
getPossibleValues (a,b:c) = [getPossibleAt b] ++ getPossibleValues (a,c)
-}
