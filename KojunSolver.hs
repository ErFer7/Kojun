{-  Trabalho 1 de Paradigmas de Programação
    Grupo: Eric e Otávio
    v0.6.1

    Funcionamento:
    Leitura e montagem do tabuleiro
    Resolução
    Exibição do resultado

    TODO:
    [X] Parser e Structure
    [X] Printer
    [ ] Solver                (2022-06-01)
    [ ] Aprimorar o algoritmo (2022-06-01)
    [ ] Arrumar tudo          (2022-06-01)
    [ ] Relatório             (2022-06-01)
-}

import Structure
import Parser
import Solver
import Printer
import Solver

main = do

    putStr ("----------------\nTrabalho 1\nEric e Otávio\n----------------\n\n")

    testStr <- readFile "Puzzles/Kojun_12_TEST.txt"

    let inputInt = inputStrToInt testStr
    let size = getSize inputInt
    let regions = getRegionList inputInt
    let values = getValueList inputInt
    let puzzle = buildPuzzle size regions values
    let sizeStr = getSizeStr puzzle
    let regions = getRegions puzzle
    --let possible = getPossibleValues puzzle

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)
    print (show (regions!!0))
    print (show (getValuesInRegion (regions!!0) puzzle))
    print (show (checkCell 0 0 (regions!!0) puzzle))
