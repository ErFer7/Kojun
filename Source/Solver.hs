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

module Solver(cellBacktracking) where

-- import Data.List (permutations)
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

-- Checa se a célula inferior é menor que a atual
checkVerticalGreatness :: Int -> Int -> Puzzle -> Bool
checkVerticalGreatness x y puzzle =
    y + 1 >= fst puzzle ||
    getCellValue (getCell2D x y puzzle) > getCellValue (getCell2D x (y + 1) puzzle) ||
    getCellRegion (getCell2D x y puzzle) /= getCellRegion (getCell2D x (y + 1) puzzle)

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

-- Insere vários valores aleatoriamente na célula até que a inserção deixe a célula válida
insertValues :: Int -> [Int] -> StdGen -> Region -> Puzzle -> (Puzzle, Bool, StdGen)
insertValues _ [] rng _ puzzle = (puzzle, False, rng)
insertValues i l rng region (size, cells) = do

    let (randomIndex, usedRng) = randomR (0, ((length l) - 1)) rng
    let value = l!!randomIndex
    let newList = removeN value l
    let newPuzzle = setCellValue i value (size, cells)

    if checkCell (i `mod` size) (i `div` size) region newPuzzle then
        (newPuzzle, True, usedRng)
    else
        insertValues i newList usedRng region newPuzzle

-- Faz o backtracking em células
cellBacktrackingAux :: Int -> StdGen -> [Int] -> [Region] -> Puzzle -> Puzzle -> Puzzle
cellBacktrackingAux i rng freeCells regions (size, cells) originalPuzzle = do

    let region = regions!!((getCellRegion (getCell (freeCells!!i) (size, cells))) - 1)
    let possibleValues = getPossibleValues 0 (getValuesInRegion region (size, cells))
    let (newPuzzle, isValid, usedRng) = insertValues (freeCells!!i) possibleValues rng region (size, cells)

    if isValid then

        if (i + 1) < (length freeCells) then
            cellBacktrackingAux (i + 1) usedRng freeCells regions newPuzzle originalPuzzle
        else
            newPuzzle
    else
        cellBacktrackingAux 0 usedRng freeCells regions originalPuzzle originalPuzzle

-- Resolve ---------------------------------------------------------------------
-- Resolve o puzzle com backtracking sobre células
cellBacktracking :: StdGen -> Puzzle -> Puzzle
cellBacktracking rng puzzle = cellBacktrackingAux 0 rng (getFreeCells puzzle) (getRegions puzzle) puzzle puzzle
