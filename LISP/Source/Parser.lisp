; Parser

(defpackage :Parser
    (:use :common-lisp)
    (:export :read-file
             :input-str-to-int
             :get-size
             :get-region-list
             :get-value-list
             :parse-string-to-float))

(in-package :Parser)

; A indentação foi modificada de 2 espaços para 4 espaços por motivos ｅｓｔｅｔｉｃｏｓ

; Auxiliares

(defun select-n (n lista)
    (let (lista_r)
        (setq lista_r '())
        (loop
            (when (= n 0) (return lista_r))
            (setq n (- n 1))
            (cons (car lista) lista_r)
            (setq lista (cdr lista))
        )
    )
)

(defun delete-n (n lista)
    (loop
        (when (= n 0) (return lista))
        (setq n (- n 1))
        (setq lista (cdr lista))
    )
)

(defun parse-string-to-float (line)
    (with-input-from-string (s line)
        (loop
        :for num := (read s nil nil)
        :while num
        :collect num)))

; Tratamento de dados

(defun input-str-to-int (str-list)
    (when str-list
        (concatenate
            (parse-string-to-float (car str-list))
            (input-str-to-int (cdr str-list))
        )
    )
)

(defun get-size (puzzle_list)
    (car puzzle_list)
)

(defun get-region-list (puzzle_list)
    (select-n (expt (get-size puzzle_list) 2) (delete-n puzzle_list 1))
)

(defun get-value-list (puzzle_list)
    (delete-n (+ (expt (get-size puzzle_list) 2) 1) puzzle_list)
)

(defun read-file (filename)  ; thanks Rainer Joswig and Frank Shearar
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

;; (defun main ()
;;     (write-line (input-str-to-int '("1 2 3" "4 5 6" "7 8 9")))
;; )

;; (main)
