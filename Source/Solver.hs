-- Solver
-- Resolve o puzzle.

module Solver() where

import Structure

-- Auxiliares ------------------------------------------------------------------
-- count :: Int -> [Int] -> Int
-- count [] v = 0
-- count (a:b) v
--     | a == v = 1 + count b v
--     | otherwise = count b v

-- checkRegionValues :: [Int] -> Bool
-- checkRegionValues [] = True
-- checkRegionValues (a:b)
--     | count ((length (a:b)) (a:b)) == 1 = checkRegion b
--     | otherwise = False
