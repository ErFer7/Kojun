-- Parser
-- Lê os arquivos.

module Parser (inputStrToInt, getSize, getRegionList, getValueList) where

-- Auxiliares -----------------------------------------------------------------
-- Seleciona os n primeiros elemetos de uma lista
selectN :: Int -> [t] -> [t]
selectN n (a:b)
    | (n >= 0)  = [a] ++ selectN (n - 1) b
    | otherwise = []

-- Deleta os n primeiros elementos de uma lista
deleteN :: Int -> [t] -> [t]
deleteN n (a:b)
    | (n <= length (a:b)) = deleteN n b
    | otherwise           = (a:b)

-- Tratamento de dados --------------------------------------------------------
-- Mapeia os dados de entrada para uma lista de inteiros
inputStrToInt :: String -> [Int]
inputStrToInt inputStr = map read (words inputStr) :: [Int]

-- Obtém o tamanho do puzzle (primeiro elemento)
getSize :: [Int] -> Int
getSize inputInt = inputInt!!0

-- Obtém uma lista com as regiões
getRegionList :: [Int] -> [Int]
getRegionList inputInt = tail (selectN ((getSize inputInt)^2) inputInt)

-- Obtém uma lista com os valores
getValueList :: [Int] -> [Int]
getValueList inputInt = deleteN ((getSize inputInt)^2 + 1) inputInt
