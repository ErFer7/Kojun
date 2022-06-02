{-  Trabalho 1 de Paradigmas de Programação
    Grupo: Eric e Otávio
    v0.7

    Funcionamento:
    Leitura e montagem do tabuleiro
    Resolução
    Exibição do resultado

    TODO:
    [X] Parser e Structure
    [X] Printer
    [ ] Solver                (2022-06-02)
    [ ] Aprimorar o algoritmo (2022-06-03)
    [ ] Arrumar tudo          (2022-06-04)
    [ ] Relatório             (2022-06-04)
-}

import Structure
import Parser
import Printer
import Solver

main = do

    putStr ("----------------\nTrabalho 1\nEric e Otávio\n----------------\n\n")

    testStr <- readFile "Puzzles/Kojun_12.txt"

    let inputInt = inputStrToInt testStr
    let size = getSize inputInt
    let regions = getRegionList inputInt
    let values = getValueList inputInt
    let puzzle = buildPuzzle size regions values
    let sizeStr = getSizeStr puzzle
    let regions = getRegions puzzle
    --let possible = getPossibleValues puzzle

    let testRegion = 2 -- Modificar aqui para testar valores diferentes

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)
    print (show (regions!!2))
    print (show (getValuesInRegion (regions!!testRegion) puzzle))
    print (show (getPossibleValues 1 (getValuesInRegion (regions!!testRegion) puzzle)))
    print (show (getFreeCellsInRegion (regions!!testRegion) puzzle))
    print (show (getFreeCells puzzle))
    let region = regions!!((getCellRegion (getCell 4 puzzle)) - 1)
    print (show (insertValues 4 (getPossibleValues (fst region) (getValuesInRegion region puzzle)) region puzzle))
    let solvedPuzzle = cellBacktracking 0 (getFreeCells puzzle) regions puzzle
    putStr (buildPuzzleStr solvedPuzzle)
