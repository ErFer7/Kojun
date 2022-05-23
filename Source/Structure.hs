-- Structure
-- Define a estrutura.

module Structure (Cell, Puzzle, buildPuzzle, getCell) where

-- Tipos ----------------------------------------------------------------------
type Cell = (Int, Int)
type Puzzle = (Int, [Cell])

-- Auxiliares -----------------------------------------------------------------
-- Constrói uma lista de células
buildCellList :: [Int] -> [Int] -> [Cell]
buildCellList [] [] = []
buildCellList (a:b) (c:d) = [(a, c)] ++ buildCellList b d

-- Construção e acesso --------------------------------------------------------
-- Constrói o puzzle com base no tipo
buildPuzzle :: Int -> [Int] -> [Int] -> Puzzle
buildPuzzle size regions values = (size, buildCellList regions values)

-- Obtém a célula na posição (x, y)
getCell :: Int -> Int -> Puzzle -> Cell
getCell x y (size, cells) = cells!!(y * size + x)
