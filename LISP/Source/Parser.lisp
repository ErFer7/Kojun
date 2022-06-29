; Parser

; A indentação foi modificada de 2 espaços para 4 espaços por motivos ｅｓｔｅｔｉｃｏｓ

;Auxiliares

(defun selectN (n lista)
(setq lista_r '())
    (loop
        (when (= n 0) (return lista_r))
        (setq n (- n 1))
        (cons (car lista) lista_r)
        (setq lista (cdr lista))
    )
)

(defun deleteN (n lista)
    (loop
        (when (= n 0) (return lista))
        (setq n (- n 1))
        (setq lista (cdr lista))
    )
)

;Tratamento de dados

(defun parse-string-to-float (line)
    (with-input-from-string (s line)
        (loop
        :for num := (read s nil nil)
        :while num
        :collect num)))

(defun inputStrToInt (str_list)
    (if (null str_list) ()
        (concatenate
        (parse-string-to-float (car str_list))
        (inputStrToInt (cdr str_list))
        )
    )
)

(defun getSize (puzzle_list)
    (car puzzle_list)
)

(defun getRegionList (puzzle_list)
    (selectN (expt (getSize puzzle_list) 2) (deleteN puzzle_list 1))
)

(defun getValueList (puzzle_list)
    (deleteN (+ (expt (getSize puzzle_list) 2) 1) puzzle_list)
)

(defun readFile (filename) ;thanks Rainer Joswig and Frank Shearar
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun main ()
    (write-line (inputStrToInt '("1 2 3" "4 5 6" "7 8 9")))
)

(main)
