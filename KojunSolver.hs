{-  Trabalho 1 de Paradigmas de Programação
    Grupo: Eric e Otávio
    v0.8.1

    Funcionamento:
    Leitura e montagem do tabuleiro
    Resolução
    Exibição do resultado

    TODO:
    [X] Parser e Structure
    [X] Printer
    [X] Solver
    [ ] Arrumar tudo
    [ ] Relatório
-}

import System.Random

import Structure
import Parser
import Printer
import Solver

main = do

    putStr ("----------------\nTrabalho 1\nEric e Otávio\n----------------\n\n")

    testStr <- readFile "Puzzles/Kojun_12.txt"

    let inputInt = inputStrToInt testStr
    let puzzle = buildPuzzle (getSize inputInt) (getRegionList inputInt) (getValueList inputInt)
    let sizeStr = getSizeStr puzzle

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)

    putStr ("Resolvendo o puzzle...\n\n")

    let rng = mkStdGen 0

    let solvedPuzzle = cellBacktracking rng puzzle
    putStr (buildPuzzleStr solvedPuzzle)
