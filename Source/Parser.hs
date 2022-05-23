-- Parser
-- Lê os arquivos.

module Parser (inputStrToInt, getSize, getRegionArray, getValueArray) where

-- Auxiliares
selectN :: Int -> [t] -> [t]
selectN x (a:b)
    | (x >= 0)  = [a] ++ selectN (x - 1) b
    | otherwise = []

deleteN :: Int -> [t] -> [t]
deleteN x (a:b)
    | (x <= length (a:b)) = deleteN x b
    | otherwise          = (a:b)

-- Tratamento de dados
inputStrToInt :: String -> [Int]
inputStrToInt inputStr = map read (words inputStr) :: [Int]

getSize :: [Int] -> Int
getSize inputInt = inputInt!!0

getRegionArray :: [Int] -> [Int]
getRegionArray inputInt = tail (selectN ((getSize inputInt) ^ 2) inputInt)

getValueArray :: [Int] -> [Int]
getValueArray inputInt = deleteN ((getSize inputInt) ^ 2 + 1) inputInt
