; Printer
; Exibe o resultado

(defpackage :Printer
    (:use :common-lisp)
    (:export :print-puzzle))

(in-package :Printer)

; Exibição de dados -----------------------------------------------------------
; Exibe o puzzle no console
(defun print-puzzle (puzzle)
    (let (i)
        (setq i 0)
        (let (ret-string)
            (setq ret-string "")
            (loop
                (when
                    (= i (* (Structure:puzzle-size puzzle)
                            (Structure:puzzle-size puzzle)
                         )
                    )
                    (return ret-string)
                )
                (if (= 0 (mod i (Structure:puzzle-size puzzle)))
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
)
