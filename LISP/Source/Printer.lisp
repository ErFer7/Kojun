; Printer
; Exibe o resultado

; (require "Structure" "./Structure.lisp")

(defpackage :Printer
    (:use :common-lisp)
    (:export :printPuzzle))

(in-package :Printer)

; Auxiliares

; Exibição de dados
; TODO: Representar em matriz
(defun printPuzzle (puzzle)
    (concatenate (write-to-string (Structure:puzzle-size puzzle)) (write-to-string (Structure:puzzle-cells puzzle)))
)
