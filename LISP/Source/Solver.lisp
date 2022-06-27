; solver

; conta quantas vezes n aparece na lista
(defun count (n lista)
  (if (null lista)
    0
    (+
      (if (= n (car lista))
        1
        0
      )
      (count n (cdr lista))
    )
  )
)

; retira n da lista
(defun removeN (n lista)
  (if (null lista)
    ()
    (cons
      (if (= n (car lista))
        ()
        (n)
      )
      (removeN n (cdr lista))
    )
  )
)

(defun checkOrthogonalDifference (pos puzzle)
  (and
    (and
      ;superior
      (or
        (= (floor pos puzzle-size) 0)
        (not (=
          (getCellValue (getCell (- pos puzzle-size) puzzle))
        ))
      )
      ;inferior
      (or
        (= (floor pos puzzle-size) (- puzzle-size 1))
        (not (getCellValue (getCell (+ pos puzzle-size) puzzle)))
      )
    )
    (and
      ;esquerda
      (or
        (= (mod pos puzzle-size) 0)
        (not (getCellValue (getCell (- pos 1) puzzle)))
      )
      ;direita
      (or
        (= (mod pos puzzle-size) (- puzzle-size 1))
        (not (getCellValue (getCell (+ pos 1) puzzle)))
      )
    )
  )
)

(defun checkVerticalGreatness (pos puzzle)
  (or
    (or
      (= (floor pos puzzle-size) (- puzzle-size 1))
      ()
    )
    ()
  )
)

(defun checkCell (pos puzzle)
  (and
    (checkOrthogonalDifference pos puzzle)
    (checkVerticalGreatness pos puzzle)
  )
)

(defun getPossibleValues (i values)
  (if (>= (- i 1) (length values))
    ('())
    (if (= 0 (count i values))
      (cons (car values) (getPossibleValues (+ i 1) values))
      (getPossibleValues (+ i 1) values)
    )
  )
)

; Solucao 1 - Preenchimento aleatorio
(defun insertValues (puzzle cell_pos)
  (setq 
    (cell-value 
      (nth cell_pos 
        (puzzle-cells puzzle)))
    (random
      (max
        (getPossibleValues 0
          (getValuesInRegion 
            (cell-region 
              (nth cell_pos 
                (puzzle-cells puzzle))))))))  ; OMG CLOWN
  (if (checkCell cell_pos puzzle)
    (T)
    (insertValues puzzle cell_pos)
  )
)

(defun resetToN (n i puzzle)
  (if (= n i)
    (T)
    (
      (setq 
        (cell-value 
          (nth cell_pos 
            (puzzle-cells puzzle)))
      )
      (resetToN n (- i 1) puzzle)
    )
  )
)

(defun cellBacktrackingAux)

(defun cellBacktracking)
; Solver
; Resolve o puzzle

(defpackage :Solver
    (:use :common-lisp)
    (:export :blankFunction))

(in-package :Solver)

#|  Placeholder temporário, esta função é inútil e pode ser deletada assim que
    necessário
|#
(defun blankFunction ()
    0
)
