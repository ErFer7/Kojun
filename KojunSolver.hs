{-  Trabalho 1 de Paradigmas de Programação
    Grupo: Eric e Otávio
    v1.0
-}

import System.Random

import Structure
import Parser
import Printer
import Solver

main = do

    putStr ("----------------\nTrabalho 1\nEric e Otávio\n----------------\n\n")

    testStr <- readFile "Puzzles/Kojun_4.txt"  -- Lê um dos tabuleiros

    let inputInt = inputStrToInt testStr                                                          -- Arquivo convertido
    let puzzle = buildPuzzle (getSize inputInt) (getRegionList inputInt) (getValueList inputInt)  -- Tabuleiro
    let sizeStr = getSizeStr puzzle                                                               -- Tamanho

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)

    putStr ("Resolvendo o puzzle...\n\n")

    let rng = mkStdGen 0  -- Gerador de números pseudo-aleatórios

    let solvedPuzzle = cellBacktracking rng puzzle -- Tabuleiro resolvido
    putStr (buildPuzzleStr solvedPuzzle)           -- Exibe o tabuleiro
