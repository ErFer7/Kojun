-- Trabalho 1 de Paradigmas de Programação
-- Grupo: Eric e Otávio
-- v0.3

-- Funcionamento:
-- Leitura e montagem do tabuleiro
-- Resolução
-- Exibição do resultado

-- TODO:
-- [X] v0.2: Parser e Structure
-- [X] v0.3: Printer
-- [ ] v0.4: Solver                (2022-05-25)
-- [ ] v0.5: Aprimorar o algoritmo (2022-05-26)
-- [ ] v1.0: Arrumar tudo          (2022-05-27)
-- [ ] Relatório                   (2022-05-28)

import Structure
import Parser
import Printer

main = do

    putStr ("----------------\nTrabalho 1\nEric e Otávio\n----------------\n\n")

    testStr <- readFile "Puzzles/Kojun_12.txt"

    let inputInt = inputStrToInt testStr
    let size = getSize inputInt
    let regions = getRegionList inputInt
    let values = getValueList inputInt
    let puzzle = buildPuzzle size regions values
    let sizeStr = getSizeStr puzzle

    putStr ("Tamanho: " ++ sizeStr ++ "x" ++ sizeStr ++ "\n\n")
    putStr (buildPuzzleStr puzzle)
