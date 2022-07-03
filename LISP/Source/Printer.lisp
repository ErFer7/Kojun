; Printer
; Exibe o resultado

(defpackage :Printer
    (:use :common-lisp)
    (:export :print-puzzle))

(in-package :Printer)

; Exibição de dados -----------------------------------------------------------
; Exibe o puzzle no console
(defun print-puzzle (puzzle)
    (let ((i 0) (ret-string ""))
        (loop
            ; Condição de saída em que i chegou na última posição do puzzle
            (when
                (= i (* (Structure:puzzle-size puzzle)
                        (Structure:puzzle-size puzzle)
                     )
                )
                (return ret-string)
            )
            ; Verifica se a posição mudou de linha
            (if (= 0 (mod i (Structure:puzzle-size puzzle)))
                ; Quando há uma nova linha o caractere '~%' é adicionado junto ao valor para gerar uma nova linha
                (setq ret-string (format t
                                        (concatenate 'string
                                            ret-string "~%" (write-to-string
                                                                (Structure:cell-value
                                                                    (nth i (Structure:puzzle-cells puzzle))
                                                                )
                                                            )
                                        )
                                    )
                )
                ; Adiciona o valor no string
                (setq ret-string (concatenate 'string
                                        ret-string " " (write-to-string
                                                            (Structure:cell-value
                                                                (nth i (Structure:puzzle-cells puzzle))
                                                            )
                                                        )
                                    )
                )
            )
            (setq i (+ i 1))
        )
    )
)
