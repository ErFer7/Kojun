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

module Solver where

import Structure

-- Auxiliares ------------------------------------------------------------------
-- count :: Int -> [Int] -> Int
-- count [] v = 0
-- count (a:b) v
--     | a == v = 1 + count b v
--     | otherwise = count b v

-- checkRegionValues :: [Int] -> Bool
-- checkRegionValues [] = True
-- checkRegionValues (a:b)
--     | count ((length (a:b)) (a:b)) == 1 = checkRegion b
--     | otherwise = False

{-
removeElement :: Int -> [Int] -> [Int]
removeElement n [] = []
removeElement n (a:b) =
    if a == n then
          removeElement n b
    else
          [a] ++ removeElement n b

removeSubList :: [Int] -> [Int] -> [Int]
removeSubList [] a = a
removeSubList (a:b) c =
    removeSubList b (removeElement a c)

iterRegionCells :: Region -> [Int]
iterRegionCells [] = []
iterRegionCells (a:b) =
    let present = iterRegionCells b
        val = getCellValue (getCell a) in iterRegionCells
    if val /= 0:
        present ++ [val]
    present

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
