-- Printer
-- Exibe os arquivos.

module Printer (getSizeStr, buildPuzzleStr) where

import Structure

-- Auxiliares -----------------------------------------------------------------
-- Converte uma célula para um string
cellToStr :: Cell -> String
cellToStr (r, v)
    | r <= 9 = "(0" ++ show r ++ ", " ++ show v ++ ")"
    | otherwise = "(" ++ show r ++ ", " ++ show v ++ ")"

-- Constrói um string que representa a puzzle
buildPuzzleStrAux :: Int -> Int -> Puzzle -> String
buildPuzzleStrAux x y (size, cells)
    | x < size && y < size = cellToStr (getCell2D x y (size, cells)) ++ " " ++ buildPuzzleStrAux (x + 1) y (size, cells)
    | x == size && y < size = "\n" ++ buildPuzzleStrAux 0 (y + 1) (size, cells)
    | otherwise  = "\n"

-- Exibição de dados ----------------------------------------------------------
-- Obtém o tamanho convertido para string
getSizeStr :: Puzzle -> String
getSizeStr (size, _) = show size

-- Função que encapsula a função de construção de strings do puzzle
buildPuzzleStr :: Puzzle -> String
buildPuzzleStr puzzle = buildPuzzleStrAux 0 0 puzzle
