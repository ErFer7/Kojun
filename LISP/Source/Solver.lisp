; Solver
; Resolve o puzzle

(defpackage :Solver
    (:use :common-lisp)
    (:export :cell-backtracking))

(in-package :Solver)

; Auxiliares ------------------------------------------------------------------
(defun check-orthogonal-difference (pos puzzle)
    (and
        (and
            ; Superior
            (or
                (= (floor pos (Structure:puzzle-size puzzle)) 0)
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ; Inferior
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
            ; Esquerda
            (or
                (= (mod pos (Structure:puzzle-size puzzle)) 0)
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos 1) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ; Direita
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

(defun check-vertical-greatness (pos puzzle)
    (and
        (or
            (or (= (floor pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                (= (Structure:cell-value
                        (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   0
                )
            )
            (or (< (Structure:cell-value
                        (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                )
                (not (= (Structure:cell-region
                            (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                        (Structure:cell-region (nth pos (Structure:puzzle-cells puzzle)))
                     )
                )
            )
        )
        (or
            (or (= (floor pos (Structure:puzzle-size puzzle)) 0)
                (= (Structure:cell-value
                        (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   0
                )
            )
            (or (> (Structure:cell-value
                        (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                )
                (not (= (Structure:cell-region
                            (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                        (Structure:cell-region (nth pos (Structure:puzzle-cells puzzle)))
                     )
                )
            )
        )
    )
)

(defun check-cell (pos puzzle)
    (and
        (check-orthogonal-difference pos puzzle)
        (check-vertical-greatness pos puzzle)
    )
)

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

(defun insert-values (pos values puzzle)
    (let ((is-valid NIL) (random-value))
        (loop
            (when (or (null values) is-valid) (return (cons is-valid puzzle)))
            (setq random-value (nth (random (length values)) values))
            (setq values (remove random-value values))
            (setf (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle))) random-value)  ; OMG CLOWN (not anymore -_-)
            (if (check-cell pos puzzle)
                (setq is-valid T)
                ()
            )
        )
    )
)

(defun reset-to-n (n i puzzle)
    (loop
        (when (= i n) (return puzzle))
        (if (Structure:cell-is-fixed (nth i (Structure:puzzle-cells puzzle)))
            ()
            (setf (Structure:cell-value (nth i (Structure:puzzle-cells puzzle))) 0)
        )
        (setq i (- i 1))
    )
)

; Resolução do puzzle ---------------------------------------------------------
; Resolve o puzzle com backtracking sobre células
(defun cell-backtracking (puzzle)
    (let ((i 0) (region) (region-list (Structure:get-region-list puzzle)) (insertion '()))
        (loop
            (when (= i (* (Structure:puzzle-size puzzle) (Structure:puzzle-size puzzle))) (return puzzle))

            ; Obtém a região atual
            (setq region (nth (- (Structure:cell-region (nth i (Structure:puzzle-cells puzzle))) 1) region-list))

            (if (Structure:cell-is-fixed (nth i (Structure:puzzle-cells puzzle)))
                (setq i (+ i 1))
                (progn
                    (setq insertion
                        (insert-values i (get-possible-values (Structure:get-values-in-region region puzzle)) puzzle)
                    )
                    (if (car insertion)
                        (progn
                            (setq puzzle (cdr insertion))
                            (setq i (+ i 1))
                        )
                        (progn
                            (if (>= (- i (* (Structure:puzzle-size puzzle) 3)) 0)
                                (progn
                                    (setq puzzle (reset-to-n (- i (* (Structure:puzzle-size puzzle) 3)) i puzzle))
                                    (setq i (- i (* (Structure:puzzle-size puzzle) 3)))
                                )
                                (progn
                                    (setq puzzle (reset-to-n 0 i puzzle))
                                    (setq i 0)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
