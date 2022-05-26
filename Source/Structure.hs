-- Structure
-- Define a estrutura.

module Structure (Cell, Puzzle, buildPuzzle, getCell2D, getCell, getCellRegion, getCellValue, getRegions) where

-- Tipos ----------------------------------------------------------------------
type Cell = (Int, Int)      -- celula = (regiao,valor)
type Puzzle = (Int, [Cell]) -- tabuleiro = (tamanho,[celulas])
type Region = [Int]         -- regiao = [coords]

-- Auxiliares -----------------------------------------------------------------
-- Constrói uma lista de células
buildCellList :: [Int] -> [Int] -> [Cell]
buildCellList [] [] = []
buildCellList (a:b) (c:d) = [(a, c)] ++ buildCellList b d

-- Constrói uma região
buildRegion :: Int -> Int -> Puzzle -> Region
buildRegion r i (size, cells) =
    if i < size^2 then
        if getCellRegion (getCell i (size, cells)) == r then
            [i] ++ buildRegion r (i + 1) (size, cells)
        else
            buildRegion r (i + 1) (size, cells)
    else
        []

-- Checa se um valor está na lista
contains :: Int -> [Int] -> Bool
contains _ [] = False
contains v (a:b)
    | v == a = True
    | otherwise = contains v b

-- Obtém os índices de cada região a ser procurada posteriormente
getRegionIndexes :: [Int] -> [Cell] -> [Int]
getRegionIndexes _ [] = []
getRegionIndexes regionIndexes (a:b)
    | contains (getCellRegion a) regionIndexes = getRegionIndexes regionIndexes b
    | otherwise = [getCellRegion a] ++ getRegionIndexes (regionIndexes ++ [getCellRegion a]) b

-- Função auxiliar para a obtenção de regiões
getRegionsAux :: [Int] -> Puzzle -> [Region]
getRegionsAux [] _ = []
getRegionsAux (a:b) puzzle = [buildRegion a 0 puzzle] ++ getRegionsAux b puzzle

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

-- Obtém uma lista de regiões
getRegions :: Puzzle -> [Region]
getRegions (size, cells) = getRegionsAux (getRegionIndexes [] cells) (size, cells)
