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

    let testRegion = 7 -- Modificar aqui para testar valores diferentes

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)

    -- TESTES -----------------------------------------------------------------
    print (show (regions!!testRegion))
    print (show (getValuesInRegion (regions!!testRegion) puzzle))
    print (show (getPossibleValues 0 (getValuesInRegion (regions!!testRegion) puzzle)))
    print (show (getFreeCellsInRegion (regions!!testRegion) puzzle))
    print (show (getFreeCells puzzle))
    let region = regions!!((getCellRegion (getCell 21 puzzle)) - 1)
    let notnewPuzzle = setCellValue 20 4 puzzle
    let newPuzzle = setCellValue 21 3 notnewPuzzle
    putStr (buildPuzzleStr newPuzzle)
    print (show (checkCell (23 `mod` 10) (23 `div` 10) region newPuzzle))
    print (show (insertValues 23 (getPossibleValues (fst region) (getValuesInRegion region puzzle)) region puzzle))
    ---------------------------------------------------------------------------

    putStr ("\n\nResolvendo o puzzle.\n\n")

    -- CUIDADO, O BACKTRAKING FUNCIONA, PORÉM USA 16 GB DE RAM
    -- Ainda não investiguei o motivo do memory leak
    -- let solvedPuzzle = cellBacktracking puzzle
    -- putStr (buildPuzzleStr solvedPuzzle)
