#|  Trabalho 2 de Paradigmas de Programação
    Grupo: Eric e Otávio
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

    (let (puzzle)
        (let (float-list)
            ; Lê o arquivo
            (setq float-list
                (Parser:input-str-to-float (Parser:read-file "Puzzles/Kojun_12.txt"))
            )

            ; Constrói o puzzle
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

            ; Resolve o puzzle
            (Solver:region-backtracking puzzle)
            (write-line (Printer:print-puzzle puzzle))

    )
)

(main)
