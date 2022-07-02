; Solver
; Resolve o puzzle

(defpackage :Solver
    (:use :common-lisp)
    (:export :cell-backtracking))

(in-package :Solver)

; Auxiliares ------------------------------------------------------------------
; conta quantas vezes n aparece na lista
;; (defun count (n lista)
;;     (if (null lista)
;;         0
;;         (+
;;           (if (= n (car lista))
;;             1
;;             0
;;           )
;;           (count n (cdr lista))
;;         )
;;     )
;; )

;; ; retira n da lista
;; (defun removeN (n lista)
;;   (if (null lista)
;;     ()
;;     (cons
;;       (if (= n (car lista))
;;         ()
;;         (n)
;;       )
;;       (removeN n (cdr lista))
;;     )
;;   )
;; )

(defun check-orthogona-difference (pos puzzle)
    (and
        (and
            ;superior
            (or
                (= (floor pos (Structure:puzzle-size puzzle)) 0)
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ;inferior
            (or
                (= (floor pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
        )
        (and
            ;esquerda
            (or
                (= (mod pos (Structure:puzzle-size puzzle)) 0)
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos 1) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ;direita
            (or
                (= (mod pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (+ pos 1) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
        )
    )
)

;; (defun check-vertical-greatness (pos puzzle)
;;     (or
;;         (or
;;             (= (floor pos puzzle-size) (- puzzle-size 1))
;;             ()
;;             )
;;         ()
;;     )
;; )

;; (defun checkCell (pos puzzle)
;;   (and
;;     (checkOrthogonalDifference pos puzzle)
;;     (checkVerticalGreatness pos puzzle)
;;   )
;; )

(defun get-possible-values (values)
    (let ((possible-values '()))
        (dotimes (i (length values))
            (if (member (+ i 1) values)
                ()
                (setq possible-values (append possible-values (list (+ i 1))))
            )
        )
        possible-values
    )
)

;; ; Solucao 1 - Preenchimento aleatorio
;; (defun insertValues (puzzle cell_pos)
;;   (setq 
;;     (cell-value 
;;       (nth cell_pos 
;;         (puzzle-cells puzzle)))
;;     (random
;;       (max
;;         (getPossibleValues 0
;;           (getValuesInRegion 
;;             (cell-region 
;;               (nth cell_pos 
;;                 (puzzle-cells puzzle))))))))  ; OMG CLOWN
;;   (if (checkCell cell_pos puzzle)
;;     (T)
;;     (insertValues puzzle cell_pos)
;;   )
;; )

;; (defun resetToN (n i puzzle)
;;   (if (= n i)
;;     (T)
;;     (
;;       (setq 
;;         (cell-value 
;;           (nth cell_pos 
;;             (puzzle-cells puzzle)))
;;       )
;;       (resetToN n (- i 1) puzzle)
;;     )
;;   )
;; )

; Resolução do puzzle ---------------------------------------------------------
; Resolve o puzzle com backtracking sobre células
(defun cell-backtracking (puzzle)
    (let ((i 0) (region) (region-list (Structure:get-region-list puzzle)))

        ; Obtém a região atual
        (setq region (nth (- (Structure:cell-region (nth i (Structure:puzzle-cells puzzle))) 1) region-list))

        ; Obter uma lista de valores possíveis para a célula
        (get-possible-values (Structure:get-values-in-region region puzzle))

        ;; (write-line (write-to-string (get-possible-values (Structure:get-values-in-region region puzzle))))
        ;; (write-line (write-to-string (check-orthogona-difference 0 puzzle)))
        ; Inserir o valor e testar até que seja válido, se for, ret. T, se não, ret. NIL
    )
)
