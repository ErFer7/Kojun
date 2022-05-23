-- Printer
-- Exibe os arquivos.

module Printer (getSizeStr, buildPuzzleStr) where

import Structure

-- Auxiliares
cellToStr :: Cell -> String
cellToStr (r, v)
    | (r <= 9)  = "(0" ++ show r ++ ", " ++ show v ++ ")"
    | otherwise = "(" ++ show r ++ ", " ++ show v ++ ")"

buildPuzzleLines :: Int -> Int -> Puzzle -> String
buildPuzzleLines x y (size, cells)
    | (x < size && y < size) = cellToStr (getCell x y (size, cells)) ++ " " ++ buildPuzzleLines (x + 1) y (size, cells)
    | (x == size && y < size) = "\n" ++ buildPuzzleLines 0 (y + 1) (size, cells)
    | otherwise  = "\n"

-- Exibição de dados
getSizeStr :: Puzzle -> String
getSizeStr (size, _) = show size

buildPuzzleStr :: Puzzle -> String
buildPuzzleStr puzzle = buildPuzzleLines 0 0 puzzle
