-- Structure
-- Define a estrutura.

module Structure (Cell, Puzzle, buildPuzzle, getCell2D, getCell, getCellRegion, getCellValue) where

-- Tipos ----------------------------------------------------------------------
type Cell = (Int, Int)
type Puzzle = (Int, [Cell])
type Region = [Int]

-- Auxiliares -----------------------------------------------------------------
-- Constrói uma lista de células
buildCellList :: [Int] -> [Int] -> [Cell]
buildCellList [] [] = []
buildCellList (a:b) (c:d) = [(a, c)] ++ buildCellList b d

-- Constrói uma região
buildRegion :: Int -> Int -> Puzzle -> Region
buildRegion r i (size, cells) =
    if (i < size^2) then
        if (getCellRegion (getCell i (size, cells)) == r) then
            [i] ++ buildRegion r (i + 1) (size, cells)
        else
            buildRegion r (i + 1) (size, cells)
    else
        []

-- Construção e acesso --------------------------------------------------------
-- Constrói o puzzle com base no tipo
buildPuzzle :: Int -> [Int] -> [Int] -> Puzzle
buildPuzzle size regions values = (size, buildCellList regions values)

-- Obtém a célula na posição (x, y) (Bidimensionalmente)
getCell2D :: Int -> Int -> Puzzle -> Cell
getCell2D x y (size, cells) = cells!!(y * size + x)

-- Obtém a célula no índice i (Unidimensionalmente)
getCell :: Int -> Puzzle -> Cell
getCell i (size, cells) = cells!!i

-- Obtém a região da célula
getCellRegion :: Cell -> Int
getCellRegion (region, _) = region

-- Obtém o valor da célula
getCellValue :: Cell -> Int
getCellValue (_, value) = value
