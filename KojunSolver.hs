-- Trabalho 1 de Paradigmas de Programação
-- Grupo: Eric e Otávio
-- v0.1.1

-- Funcionamento:
-- Leitura e montagem do tabuleiro
-- Resolução
-- Exibição do resultado

-- TODO:
-- v0.2: Parser e Structure    (2022-05-23)
-- v0.3: Printer               (2022-05-25)
-- v0.4: Solver                (2022-05-25)
-- v0.5: Aprimorar o algoritmo (2022-05-26)
-- v1.0: Arrumar tudo          (2022-05-27)
-- Relatório                   (2022-05-28)

import Structures


main = do

    print ("Trabalho 1")

    testStr <- readFile "Puzzles/tabuleiro_teste.txt"

    print (testStr)
