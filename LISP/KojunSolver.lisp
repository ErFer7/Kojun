#|  Trabalho 2 de Paradigmas de Programação
    Grupo: Eric e Otávio
    v0.1
|#

(require "Structure" "./Source/Structure.lisp")
(require "Parser" "./Source/Parser.lisp")
(require "Printer" "./Source/Printer.lisp")
(require "Solver" "./Source/Solver.lisp")

(defun main()

    (write-line "----------------")
    (write-line "Trabalho 2")
    (write-line "Eric e Otávio")
    (write-line "----------------")
    (terpri)

    (write (write-to-string (Structure:buildPuzzle 10 '(1 2 3) '(1 2 3))))

    (write (Parser:readFile "Puzzles/Kojun_1.txt"))
)

(main)
