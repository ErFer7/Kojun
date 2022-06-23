; Parser

;Auxiliares

(defun selectN (n lista)
  (setq lista_r '())
  (loop
    (when (= n 0) (return lista_r))
    (setq n (- n 1))
    (cons (car lista) lista_r)
    (setq lista (cdr lista)
  )
)

(defun deleteN (n lista)
  (loop
    (when (= n 0) (return lista))
    (setq n (- n 1))
    (setq lista (cdr lista)
  )
)

;Tratamento de dados

(defun inputStrToInt (str)
  (dolist (i str)
    ((loop :for (integer position) := (multiple-value-list
                                    (parse-integer string
                                                   :start (or position 0)
                                                   :junk-allowed t))
        :while integer
        :collect integer))
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
