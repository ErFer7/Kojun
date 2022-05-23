-- Structure
-- Define a estrutura.

module Structures (Cell, Puzzle) where

type Cell = (Int, Int)
type Puzzle = [[Cell]]

-- buildMatrix :: String -> Int -> Int -> [[Int]] -> String
-- buildMatrix inputStr charIndex line matrix =
--     if inputStr!!charIndex \= '\n' then
--         buildMatrix (inputStr (charIndex + 1) line (matrix!!line ++ [inputStr!!charIndex]))
--     else
--         buildMatrix (inputStr (charIndex + 1) (line + 1) matrix)

-- buildRegionMatrix :: String -> String
-- buildRegionMatrix inputStr = inputStr
-- buildRegionMatrix inputStr = buildMatrix (inputStr 0 0 [])
