-- Structure
-- Define a estrutura.

module Structure (Cell,
                  Puzzle,
                  Region,
                  buildPuzzle,
                  getCell2D,
                  getCell,
                  getCellRegion,
                  getCellValue,
                  setCellValue,
                  getRegion,
                  getRegions,
                  getRegionIndexes,
                  getValuesInRegion,
                  getFreeCells,
                  getFreeCellsInRegion) where

-- Tipos ----------------------------------------------------------------------
type Cell = (Int, Int)      -- celula = (região, valor)
type Puzzle = (Int, [Cell]) -- tabuleiro = (tamanho, [celulas])
type Region = (Int, [Int])  -- regiao = (Índice, [coords])

-- Auxiliares -----------------------------------------------------------------
-- Constrói uma lista de células
buildCellList :: [Int] -> [Int] -> [Cell]
buildCellList [] [] = []
buildCellList (a:b) (c:d) = [(a, c)] ++ buildCellList b d

-- Checa se um valor está na lista
contains :: Int -> [Int] -> Bool
contains _ [] = False
contains v (a:b)
    | v == a = True
    | otherwise = contains v b

-- Função auxiliar para a obtenção dos índices das regiões
getRegionIndexesAux :: [Int] -> [Cell] -> [Int]
getRegionIndexesAux _ [] = []
getRegionIndexesAux regionIndexes (a:b)
    -- Se a célula tem uma região já adicionada na lista, ignora
    | contains (getCellRegion a) regionIndexes = getRegionIndexesAux regionIndexes b
    -- Adiciona a região na lista
    | otherwise = [getCellRegion a] ++ getRegionIndexesAux (regionIndexes ++ [getCellRegion a]) b

-- Função auxiliar para a obtenção de uma lista de coordenadas em uma região
getRegionCellIndexesAux :: Int -> Int -> Puzzle -> [Int]
getRegionCellIndexesAux r i (size, cells) =

    -- Verifica se o índice já passou do limite
    if i < size^2 then

        -- Se a célula faz parte da região 'r', ela é adicionada na lista
        if getCellRegion (getCell i (size, cells)) == r then
            [i] ++ getRegionCellIndexesAux r (i + 1) (size, cells)
        else
            getRegionCellIndexesAux r (i + 1) (size, cells)
    else
        []

-- Função auxiliar para a obtenção de regiões
getRegionsAux :: [Int] -> Puzzle -> [Region]
getRegionsAux [] _ = []
getRegionsAux (a:b) puzzle = [getRegion a puzzle] ++ getRegionsAux b puzzle

-- Função auxiliar para encontrar células vazias no puzzle
getFreeCellsAux :: Int -> Puzzle -> [Int]
getFreeCellsAux i (size, cells) =

    -- Verifica se o índice já passou do limite
    if i < size^2 then

        -- Se a célula é 0, ela é adicionada na lista
        if getCellValue (getCell i (size, cells)) == 0 then
            [i] ++ getFreeCellsAux (i + 1) (size, cells)
        else
            getFreeCellsAux (i + 1) (size, cells)
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
getCell i (_, cells) = cells!!i

-- Obtém a região da célula
getCellRegion :: Cell -> Int
getCellRegion (region, _) = region

-- Obtém o valor da célula
getCellValue :: Cell -> Int
getCellValue (_, value) = value

-- Define a célula no índice i (Unidimensionalmente)
setCellValue :: Int -> Int -> Puzzle -> Puzzle
setCellValue i v (size, cells) = do

    let splitList = splitAt i cells  -- Tupla com a lista divida
    let (a:b) = snd splitList        --

    (size, (fst splitList) ++ [((getCellRegion (getCell i (size, cells))), v)] ++ b)

-- Obtém uma região
getRegion :: Int -> Puzzle -> Region
getRegion r puzzle = (r, getRegionCellIndexesAux r 0 puzzle)

-- Obtém uma lista de regiões
getRegions :: Puzzle -> [Region]
getRegions puzzle = getRegionsAux (getRegionIndexes puzzle) puzzle

-- Obtém os índices de cada região a ser procurada posteriormente
getRegionIndexes :: Puzzle -> [Int]
getRegionIndexes (_, cells) = getRegionIndexesAux [] cells

-- Obtém os valores em uma região
getValuesInRegion :: Region -> Puzzle -> [Int]
getValuesInRegion (_, []) _ = []
getValuesInRegion (r, (a:b)) puzzle = [getCellValue (getCell a puzzle)] ++ getValuesInRegion (r, b) puzzle

-- Obtém uma lista de células livres no tabuleiro inteiro
getFreeCells :: Puzzle -> [Int]
getFreeCells puzzle = getFreeCellsAux 0 puzzle

-- funcao auxiliar de obter celulas livres de uma regiao
getFreeCellsInRegionAux :: [Int] -> Puzzle -> [Int]
getFreeCellsInRegionAux [] _ = []
getFreeCellsInRegionAux (a:b) p
    | (getCellValue (getCell a p) == 0) = [a] ++ getFreeCellsInRegionAux b p
    | otherwise = getFreeCellsInRegionAux b p

-- Obtem lista de celulas livres na regiao escolhida
getFreeCellsInRegion :: Region -> Puzzle -> [Int]
getFreeCellsInRegion (i, coords) p = getFreeCellsInRegionAux coords p
