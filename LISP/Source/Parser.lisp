; Parser

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

(defun getSize (puzzle)
  (car puzzle)
)

(defun getRegionList (puzzle)
  (selectN (expt (getSize puzzle) 2) (deleteN puzzle 1))
)

(defun getValueList (puzzle)
  (deleteN puzzle (+ (expt (getSize puzzle) 2) 1))
)

(defun main ()
  (write-line (inputStrToInt '("1 2 3" "4 5 6" "7 8 9")))
)

(main)
