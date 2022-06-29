; Parser

(defpackage :Parser
    (:use :common-lisp)
    (:export :read-file
             :input-str-to-float
             :get-size
             :get-region-list
             :get-value-list))

(in-package :Parser)

; A indentação foi modificada de 2 espaços para 4 espaços por motivos ｅｓｔｅｔｉｃｏｓ

; Auxiliares ------------------------------------------------------------------

; Seleciona um intervalo (de s a e-1) de números em uma lista.
(defun select-range (s e float-list)
    (let (i)
        (setq i s)
        (let (ret-list)
            (setq ret-list '())
            (loop
                (when (= i e) (return ret-list))
                (setq ret-list (append ret-list (list (nth i float-list))))
                (setq i (+ i 1))
            )
        )
    )
)

; Converte um string de floats em uma lista de floats
(defun parse-string-to-float-list (line)
    (with-input-from-string (s line)
        (loop
        :for num := (read s nil nil)
        :while num
        :collect num)))

; Tratamento de dados ---------------------------------------------------------

; Gera uma lista de floats a partir de uma lista de strings
(defun input-str-to-float (str-list)
    (when str-list
        (concatenate 'list
            (parse-string-to-float-list (car str-list))
            (input-str-to-float (cdr str-list))
        )
    )
)

; Obtém o tamanho do puzzle
(defun get-size (puzzle-list)
    (car (select-range 0 1 puzzle-list))
)

; Obtém a lista de indices das regiões do puzzle
(defun get-region-list (puzzle-list)
    (let (size)
        (setq size (get-size puzzle-list))
        (select-range 1 (+ (* size size) 1) puzzle-list)
    )
)

; Obtém a lista de valores do puzzle
(defun get-value-list (puzzle-list)
    (let (size)
        (setq size (get-size puzzle-list))
        (select-range (+ (* size size) 1) (+ (* 2 (* size size)) 1) puzzle-list)
    )
)

; Lê um arquivo
(defun read-file (filename)  ; thanks Rainer Joswig and Frank Shearar
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))
