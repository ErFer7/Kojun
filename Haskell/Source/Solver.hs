-- Solver
-- Resolve o puzzle.

{-
Algoritmo original planejado (Não implementado):

1) Repetir até uma iteração não causar mudança alguma
    a) Células com apenas um valor possível são fixos nesta
    b) Regiões com uma célula compatível com um valor a fixam neste
    c) Regiões com uma coluna livre a preenchem com valores de baixo pra cima
    d) Recalcular todos os valores possíveis para cada célula não fixa
Isso melhora a quantidade de soluções possíveis

2) Testar possíveis soluções com algoritmo de backtracking
-}

module Solver(cellBacktracking, getNextRegionIter) where

import Data.List (permutations)
import System.Random

import Structure

-- Auxiliares ------------------------------------------------------------------
-- Conta quantas vezes um valor aparece na lista
count :: Int -> [Int] -> Int
count v [] = 0
count v (a:b)
    | a == v = 1 + count v b
    | otherwise = count v b

-- Remove um elemento da lista
removeN :: Int -> [Int] -> [Int]
removeN _ [] = []
removeN n (a:b)
    | n /= a = [a] ++ removeN n b
    | otherwise = removeN n b

-- Checa se todas as células adjacentes são diferentes
checkOrthogonalDifference :: Int -> Int -> Puzzle -> Bool
checkOrthogonalDifference x y puzzle =
    -- Verifica se a célula inferior existe e é diferente
    (y + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y + 1) puzzle)) &&
    -- Verifica se a célula superior existe e é diferente
    (y - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D x (y - 1) puzzle)) &&
    -- Verifica se a célula direita existe e é diferente
    (x + 1 >= fst puzzle || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x + 1) y puzzle)) &&
    -- Verifica se a célula esquerda existe e é diferente
    (x - 1 < 0 || getCellValue (getCell2D x y puzzle) /= getCellValue (getCell2D (x - 1) y puzzle))

-- Checa se a célula é menor que a superior e maior que a inferior, caso a outra seja 0 a verificação é válida
-- A verificação só considera células na mesma região
checkVerticalGreatness :: Int -> Int -> Puzzle -> Bool
checkVerticalGreatness x y puzzle =
    -- Verifica se a célula inferior existe
    (y + 1 >= fst puzzle ||
    -- Verifica se a célula tem valor 0
    getCellValue (getCell2D x (y + 1) puzzle) == 0 ||
    -- Verifica se a célula inferior é menor
    getCellValue (getCell2D x y puzzle) > getCellValue (getCell2D x (y + 1) puzzle) ||
    -- Verifica se a célula inferior está em regiões diferentes
    getCellRegion (getCell2D x y puzzle) /= getCellRegion (getCell2D x (y + 1) puzzle)) &&
    -- Verifica se a célula superior existe
    (y - 1 < 0 ||
    -- Verifica se a célula tem valor 0
    getCellValue (getCell2D x (y - 1) puzzle) == 0 ||
    -- Verifica se a célula superior é maior
    getCellValue (getCell2D x y puzzle) < getCellValue (getCell2D x (y - 1) puzzle) ||
    -- Verifica se a célula superior está em regiões diferentes
    getCellRegion (getCell2D x y puzzle) /= getCellRegion (getCell2D x (y - 1) puzzle))

-- Checa todas as regras para uma célula
checkCell :: Int -> Int -> Region -> Puzzle -> Bool
checkCell x y region puzzle = checkOrthogonalDifference x y puzzle && checkVerticalGreatness x y puzzle

-- Obtém os valores possíveis para uma região com base nos valores dela, os valores devem ser de 1 a N
-- sendo que N é o tamanho da região
getPossibleValues :: Int -> [Int] -> [Int]
getPossibleValues i values
    | i - 1 >= length values = []
    | count i values == 0 = [i] ++ getPossibleValues (i + 1) values
    | otherwise = getPossibleValues (i + 1) values

-- Insere vários valores aleatoriamente na célula até que a inserção deixe a célula válida
insertValues :: Int -> [Int] -> StdGen -> Region -> Puzzle -> (Puzzle, Bool, StdGen)
insertValues _ [] rng _ puzzle = (puzzle, False, rng)
insertValues i l rng region (size, cells) = do

    let (randomIndex, usedRng) = randomR (0, (length l) - 1) rng  -- Índice aleatório
    let value = l!!randomIndex                                    -- Valor aleatório
    let newPuzzle = setCellValue i value (size, cells)            -- Insere o valor

    -- Checa se a inserção é valida e retorna o tabuleiro novo com um booleano e o gerador caso seja
    if checkCell (i `mod` size) (i `div` size) region newPuzzle then
        (newPuzzle, True, usedRng)
    else
        insertValues i (removeN value l) usedRng region newPuzzle  -- Tenta inserir outro valor caso não seja válida

-- Redefine o tabuleiro para 0 do fim até N
resetToN :: Int -> Int -> [Int] -> Puzzle -> Puzzle
resetToN n i freeCells puzzle = do

    -- i é o índice da que é usado na lista de células livres para a redefinição
    if i > n then
        resetToN n (i - 1) freeCells (setCellValue (freeCells!!i) 0 puzzle)
    else
        setCellValue (freeCells!!i) 0 puzzle

-- Função auxiliar que faz o backtracking em células
cellBacktrackingAux :: Int -> StdGen -> [Int] -> [Region] -> Puzzle -> Puzzle -> Puzzle
cellBacktrackingAux i rng freeCells regions (size, cells) originalPuzzle = do

    -- Região atual
    let region = regions!!((getCellRegion (getCell (freeCells!!i) (size, cells))) - 1)
    -- Valores possíveis
    let possibleValues = getPossibleValues 0 (getValuesInRegion region (size, cells))
    -- Nova puzzle e validade da inserção
    let (newPuzzle, isValid, usedRng) = insertValues (freeCells!!i) possibleValues rng region (size, cells)
    -- Puzzle redefinida até N
    let resetedToN = resetToN (i - size * 3) ((length freeCells) - 1) freeCells newPuzzle

    if isValid then

        -- Verifica se esse é o último índice
        if i + 1 < length freeCells then
            cellBacktrackingAux (i + 1) usedRng freeCells regions newPuzzle originalPuzzle  -- Avança
        else
            newPuzzle  -- Retorna o tabuleiro resolvido
    else
        -- Verifica se é possível pular até 'i - size * 3'
        if i - size * 3 >= 0 then
            cellBacktrackingAux (i - size * 3) usedRng freeCells regions resetedToN originalPuzzle  -- Volta
        else
            cellBacktrackingAux 0 usedRng freeCells regions originalPuzzle originalPuzzle  -- Volta até 0

-- Resolve ---------------------------------------------------------------------
-- Resolve o puzzle com backtracking sobre células
cellBacktracking :: StdGen -> Puzzle -> Puzzle
cellBacktracking rng puzzle = cellBacktrackingAux 0 rng (getFreeCells puzzle) (getRegions puzzle) puzzle puzzle

-- Solução 2 ----------------------------------------------------------------
-- Solução não aleatoria, baseada em iterações sobre permutações

-- Cria lista de permutações para os valores possíveis de uma região
getPossibleValuesPermutation :: Region -> Puzzle -> [[Int]]
getPossibleValuesPermutation (n, []) _ = []
getPossibleValuesPermutation r puzzle =
    permutations values where
        values = getPossibleValues 1 (getValuesInRegion r puzzle)

-- Testa se todos os valores, apos inserção, sao válidos
testingRegion :: Region -> Puzzle -> Int -> Bool
testingRegion (regSize,(a:b)) (puzSize,cellList) iter
    | (iter == regSize) = True
    | not (checkCell (mod a puzSize) (div a puzSize) (regSize,(a:b)) (puzSize,cellList)) = False
    | otherwise = testingRegion (regSize,(b)) (puzSize,cellList) (iter+1)

-- coloca n valores em n células vazias
fillRegionWithValues :: [Int] -> [Int] -> Puzzle -> Puzzle
fillRegionWithValues [] [] puzzle = puzzle
fillRegionWithValues (a:values) (b:coords) puzzle = do
    let inserted = setCellValue b a puzzle
    fillRegionWithValues values coords inserted

-- descobre se uma dada iteração é válida
getWorkingPermutation :: Int -> Puzzle -> Int -> [[Int]] -> Puzzle
getWorkingPermutation regionIndex puzzle iter perms = do
    let tested = fillRegionWithValues (perms!!iter) (getFreeCellsInRegion (getRegion regionIndex puzzle) puzzle) puzzle
    if (testingRegion (getRegion regionIndex puzzle) tested 0) then
        tested
    else
        (-1,[]) -- (-1) = valores inválidos

-- tenta encontrar permutacao valida para uma regiao so que nao funciona
getNextRegionIterAux :: Int -> Puzzle -> Int -> [[Int]] -> Puzzle
getNextRegionIterAux regiaoIndex puzzle iter values =
    if (iter < length values) then do
        let regiao = getRegion regiaoIndex puzzle
        let naught = getFreeCellsInRegion regiao puzzle
        let (a,b) = getWorkingPermutation regiaoIndex puzzle iter values
        if (a == -1) then
            getNextRegionIterAux regiaoIndex puzzle (iter+1) values
        else
            (a,b)
    else
        (-2,[]) -- (-2) = sem iterações disponiveis

-- encontra permutação válida
getNextRegionIter :: Int -> Puzzle -> Puzzle
getNextRegionIter regiaoIndex puzzle =
    getNextRegionIterAux regiaoIndex puzzle 0 (getPossibleValuesPermutation (getRegion regiaoIndex puzzle) puzzle)
