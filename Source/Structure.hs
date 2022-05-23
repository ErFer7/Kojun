-- Structure
-- Define a estrutura.

module Structure (Cell, Puzzle, buildPuzzle, getCell) where

-- Tipos
type Cell = (Int, Int)
type Puzzle = (Int, [Cell])

-- Auxiliares
buildCellArray :: [Int] -> [Int] -> [Cell]
buildCellArray [] [] = []
buildCellArray (a:b) (c:d) = [(a, c)] ++ buildCellArray b d

-- Construção e acesso
buildPuzzle :: Int -> [Int] -> [Int] -> Puzzle
buildPuzzle size regions values = (size, buildCellArray regions values)

getCell :: Int -> Int -> Puzzle -> Cell
getCell x y (size, cells) = cells!!(y * size + x)
