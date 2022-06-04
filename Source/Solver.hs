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

module Solver(checkCell, getPossibleValues, cellBacktracking, insertValues, testingRegion, backtracking, checkRegionRepetition) where

import Structure
import Data.List (permutations)
import System.Random

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
checkOrthogonalDifference x y puzzle =
    (y + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y + 1) puzzle)) &&
    (y - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y - 1) puzzle)) &&
    (x + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x + 1) y puzzle)) &&
    (x - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x - 1) y puzzle))

-- Checa se a célula superior é maior que a atual
checkVerticalGreatness :: Int -> Int -> Puzzle -> Bool
checkVerticalGreatness x y puzzle =
    y - 1 < 0 ||
    getCellValue (getCell2D x y puzzle) < getCellValue (getCell2D x (y - 1) puzzle) ||
    getCellRegion (getCell2D x y puzzle) /= getCellRegion (getCell2D x (y - 1) puzzle)

-- Checa todas as regras para uma célula
checkCell :: Int -> Int -> Region -> Puzzle -> Bool
checkCell x y region puzzle =
    checkRegionRepetition (getValuesInRegion region puzzle) &&
    checkOrthogonalDifference x y puzzle &&
    checkVerticalGreatness x y puzzle &&
    getCellValue (getCell2D x y puzzle) /= 0

-- Obtém os valores possíveis para uma região com base nos valores dela
getPossibleValues :: Int -> [Int] -> [Int]
getPossibleValues i values
    | i - 1 >= length values = []
    | count i values == 0 = [i] ++ getPossibleValues (i + 1) values
    | otherwise = getPossibleValues (i + 1) values

-- Escolhe um valor aleatório na lista
choice :: [Int] -> Int
choice l = l !! rand where
  n = length l
  (rand, _) = randomR (0, (n - 1)) (mkStdGen 0)

-- Insere vários valores na célula até que a inserção deixe a célula válida
insertValues :: Int -> [Int] -> Region -> Puzzle -> (Puzzle, Bool)
insertValues _ [] _ puzzle = (puzzle, False)
insertValues i l region (size, cells) = do

    let newPuzzle = setCellValue i (choice l) (size, cells)

    if checkCell (i `mod` size) (i `div` size) region newPuzzle then
        (newPuzzle, True)
    else
        insertValues i l region newPuzzle

-- Faz o backtracking em células
cellBacktrackingAux :: Int -> [Int] -> [Region] -> Puzzle -> Puzzle
cellBacktrackingAux i freeCells regions (size, cells) = do

    let cI = (freeCells!!i)
    let region = regions!!((getCellRegion (getCell cI (size, cells))) - 1)
    let insertion = insertValues cI (getPossibleValues 0 (getValuesInRegion region (size, cells))) region (size, cells)
    let newPuzzle = fst insertion
    let validInsertion = snd insertion

    if i < size^2 then

        if validInsertion then
            cellBacktrackingAux (i + 1) freeCells regions newPuzzle
        else
            cellBacktrackingAux (i - 1) freeCells regions newPuzzle
    else
        (size, cells)


-- -- Cria lista de permutacoes para os valores possiveis de uma regiao
-- getPossibleValuesPermutation :: Region -> Puzzle -> [[Int]]
-- getPossibleValuesPermutation (n, []) _ = []
-- getPossibleValuesPermutation r puzzle =
--     permutations values where
--         values = getPossibleValues 1 (getValuesInRegion r puzzle)
--
-- getWorkingPermutation :: Int -> Puzzle -> Int -> [[Int]] -> [Int]
-- getWorkingPermutation region puzzle iter perms =
--     fillRegionWithValues (perms!!iter) (getFreeCellsInRegion (getRegion region puzzle) puzzle)
--
--
-- -- Testa se todos os valores, apos insercao, sao validos
-- testingRegion :: Region -> Puzzle -> Int -> Bool
-- testingRegion (regSize,[a:b]) (puzSize,cellList) iter
--     | (iter == regSize) = True
--     | (checkCell (mod a puzSize) (div a puzSize) == False) = False
--     | otherwise = testingRegion (a,c) puzzle iter
--
-- -- coloca n valores em n celulas
-- fillRegionWithValues :: [Int] -> [Int] -> Puzzle -> Bool
-- fillRegionWithValues [] [] _ = False
-- fillRegionWithValues [a,values] [b,coords] puzzle = do
--     setCellValue b a puzzle
--     fillRegionWithValues values coords puzzle
--     return True

-- Insere um valor na célula
-- insertValue :: Int -> [Int] -> Puzzle -> Puzzle
-- insertValue _ [] puzzle = puzzle
-- insertValue i l = (\l g -> l !! fst (randomR (0, length l) g))

-- Cria lista de permutações para os valores possíveis de uma região
getPossibleValuesPermutation :: Region -> Puzzle -> [[Int]]
getPossibleValuesPermutation (n, []) _ = []
getPossibleValuesPermutation r puzzle =
    permutations values where
        values = getPossibleValues 1 (getValuesInRegion r puzzle)

-- Testa se todos os valores, após inserção, são validos
testingRegion :: Region -> Puzzle -> Int -> Bool
testingRegion (n,coords) (puzSize,cellList) iter
    | (iter == length coords) = True
    | ((checkCell (mod
        (coords!!iter) puzSize)
        (div (coords!!iter) puzSize)
        (n,coords)
        (puzSize,cellList)) == False) = False
    | otherwise = (testingRegion (n,coords) (puzSize,cellList) (iter+1))

-- coloca n valores em n celulas
fillRegionWithValues :: [Int] -> [Int] -> Puzzle -> Puzzle
fillRegionWithValues [] [] p = p
fillRegionWithValues (a:values) (b:coords) puzzle =
    fillRegionWithValues values coords (setCellValue b a puzzle)

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
-- backtracking :: Int -> Puzzle -> Puzzle
-- backtracking _ p = do

-- atualmente, testa uma regiao apenas
backtracking :: Int -> Puzzle -> Puzzle
backtracking regIndex p =
    fillRegionWithValues (
        getPossibleValues
            1
            (getValuesInRegion (getRegion regIndex p) p))
        (getFreeCellsInRegion (getRegion regIndex p) p)
        p


--
-- insertRandomValue :: Int -> [Int] -> Puzzle -> Puzzle
-- insertRandomValue _ [] puzzle = puzzle
-- insertRandomValue i l = (\l g -> l !! fst (randomR (0, length l) g))

-- TODO: Função para escolher um valor aleatório em uma lista e inserir no puzzle
-- TODO: Função monad que chama a função de inserção e checa a lista, fazendo o backtracking

-- Resolve ---------------------------------------------------------------------
-- Resolve o puzzle com backtracking sobre células
cellBacktracking :: Puzzle -> Puzzle
cellBacktracking puzzle = cellBacktrackingAux 0 (getFreeCells puzzle) (getRegions puzzle) puzzle

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
