-- Trabalho 1 de Paradigmas de Programação
-- Grupo: Eric e Otávio
-- v0.2

-- Funcionamento:
-- Leitura e montagem do tabuleiro
-- Resolução
-- Exibição do resultado

-- TODO:
-- [X] v0.2: Parser e Structure
-- [ ] v0.3: Printer               (2022-05-23)
-- [ ] v0.4: Solver                (2022-05-25)
-- [ ] v0.5: Aprimorar o algoritmo (2022-05-26)
-- [ ] v1.0: Arrumar tudo          (2022-05-27)
-- [ ] Relatório                   (2022-05-28)

import Parser
import Structure

main = do

    print ("Trabalho 1")

    testStr <- readFile "Puzzles/Kojun_12.txt"

    let inputInt = inputStrToInt testStr
    let size = getSize inputInt
    let regions = getRegionArray inputInt
    let values = getValueArray inputInt
    let puzzle = buildPuzzle size regions values

    print (show puzzle)                -- Puzzle construída
    print (show (getCell 0 0 puzzle))  -- Exemplo de acesso a uma célula
