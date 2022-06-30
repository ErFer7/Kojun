#|  Trabalho 2 de Paradigmas de Programação
    Grupo: Eric e Otávio

    TODO:
    [X] Parser
    [ ] Structure 2022-06-30
    [ ] Printer   2022-07-01
    [ ] Solver    2022-07-02
|#

;; (require "Structure" "./Source/Structure.lisp")
(require "Parser" "./Source/Parser.lisp")
;; (require "Printer" "./Source/Printer.lisp")
;; (require "Solver" "./Source/Solver.lisp")

(defun main()

    (write-line "----------------")
    (write-line "Trabalho 2")
    (write-line "Eric e Otávio")
    (write-line "----------------")
    (terpri)

    ;; (write (write-to-string (Structure:buildPuzzle 10 '(1 2 3) '(1 2 3))))

    (let (float-list)

        (setq float-list
            (Parser:input-str-to-float (Parser:read-file "Puzzles/Kojun_1.txt"))
        )

        (write-line (write-to-string (Parser:get-size float-list)))
        (write-line (write-to-string (Parser:get-region-list float-list)))
        (write-line (write-to-string (Parser:get-value-list float-list)))
    )
)

(main)
