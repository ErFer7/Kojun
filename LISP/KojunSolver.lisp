#|  Trabalho 2 de Paradigmas de Programação
    Grupo: Eric e Otávio

    TODO:
    [X] Parser
    [X] Printer   2022-07-30
    [ ] Structure 2022-06-01
    [ ] Solver    2022-07-02
|#

(require "Structure" "./Source/Structure.lisp")
(require "Parser" "./Source/Parser.lisp")
(require "Printer" "./Source/Printer.lisp")
;; (require "Solver" "./Source/Solver.lisp")

(defun main()

    (write-line "----------------")
    (write-line "Trabalho 2")
    (write-line "Eric e Otávio")
    (write-line "----------------")
    (terpri)

    (let (puzzle)
        (let (float-list)
            (setq float-list
                (Parser:input-str-to-float (Parser:read-file "Puzzles/Kojun_1.txt"))
            )

            (setq puzzle
                (Structure:build-puzzle (Parser:get-size float-list)
                                        (Parser:get-region-list float-list)
                                        (Parser:get-value-list float-list)
                )
            )
        )
            (write-line (concatenate 'string
                            "Tamanho: "
                            (write-to-string (Structure:puzzle-size puzzle))
                            "x"
                            (write-to-string (Structure:puzzle-size puzzle))
                        )
            )

            (write-line (Printer:print-puzzle puzzle))

            (terpri)
            (write-line "Resolvendo o puzzle...")

            ; (A resolução é chamada aqui)
    )
)

(main)
