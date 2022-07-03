; Solver
; Resolve o puzzle

(defpackage :Solver
    (:use :common-lisp)
    (:export :region-backtracking))

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

(defun all-permutations (l)
(cond ((null l) nil)
        ((null (cdr l)) (list l))
        (t (loop for element in l
            append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element l)
                           )
                   )
           )
        )
    )
)

(defun insert-value-list (coords values puzzle)
    (let ((is-valid NIL) (random-permutation))
        (loop
            (when (or (null values) is-valid) (return (cons is-valid puzzle)))
            (setq random-permutation (nth (random (length values)) values))
            (setq values (remove random-permutation values))
            (let ((i 0))
                (loop
                    (when (>= i (length coords)) (return coords))
                    (if (Structure:cell-is-fixed (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                        (setq coords (remove (nth i coords) coords))
                        (setq i (+ i 1))
                    )
                )
            )
            (dotimes (i (length coords))
                (if (Structure:cell-is-fixed (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                    ()
                    (progn
                        (setf (Structure:cell-value (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                              (nth i random-permutation)
                        )
                        (if (check-cell (nth i coords) puzzle)
                            (setq is-valid T)
                            (setq is-valid NIL)
                        )
                        (when
                            (null is-valid)
                            (return
                                (setq puzzle (reset-at-positions coords puzzle))
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun reset-at-positions (coords puzzle)
    (loop for pos in coords
        do (if (Structure:cell-is-fixed (nth pos (Structure:puzzle-cells puzzle)))
                ()
                (setf (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle))) 0)
           )
    )
    puzzle
)

; Resolução do puzzle ---------------------------------------------------------
; Resolve o puzzle com backtracking sobre regiões
(defun region-backtracking (puzzle)
    (let ((r 0)
          (region)
          (region-list (Structure:get-region-list puzzle))
          (insertion '())
          (jump-offset 1)
          (jump-region 1)
          (error-count 0)
         )
        (loop
            (when (> r (- (Structure:region-index (nth (- (length region-list) 1) region-list)) 1)) (return puzzle))

            ; Obtém a região atual
            (setq region (nth r region-list))

            (setq insertion
                (insert-value-list
                    (Structure:region-coords region)
                    (all-permutations (get-possible-values (Structure:get-values-in-region region puzzle)))
                    puzzle
                )
            )

            (if (car insertion)
                (progn
                    (setq puzzle (cdr insertion))
                    (setq r (+ r 1))
                )
                (progn
                    (setq error-count (+ error-count 1))
                    (if (> error-count 4)
                        (progn
                            (setq jump-offset (+ jump-offset 1))
                            (setq error-count 0)
                        )
                        ()
                    )
                    (if (>= (- r jump-offset) 0)
                        (progn
                            (setq jump-region (- r jump-offset))
                            (loop
                                (setq r (- r 1))
                                (if (>= r jump-region)
                                    (progn
                                        (setq region (nth r region-list))
                                        (setq puzzle (reset-at-positions (Structure:region-coords region) puzzle))
                                    )
                                    (progn
                                        (setq r jump-region)
                                        (return puzzle)
                                    )
                                )
                            )
                        )
                        (progn
                            (loop
                                (setq r (- r 1))
                                (if (>= r 0)
                                    (progn
                                        (setq region (nth r region-list))
                                        (setq puzzle (reset-at-positions (Structure:region-coords region) puzzle))
                                    )
                                    (progn
                                        (setq r 0)
                                        (return puzzle)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
