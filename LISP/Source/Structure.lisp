; Structure

(defpackage :Structure
    (:use :common-lisp)
    (:export :region
             :region-index
             :region-coords
             :cell
             :cell-region
             :cell-value
             :cell-is-fixed
             :puzzle
             :puzzle-size
             :puzzle-cells
             :copy-puzzle
             :build-puzzle
             :get-region-list
             :get-values-in-region))

(in-package :Structure)

; Estruturas ------------------------------------------------------------------
; Região (int, list)
(defstruct region
    index
    coords
)

; Célula (int, int, bool)
(defstruct cell
    region
    value
    is-fixed
)

; Puzzle (int, list)
(defstruct puzzle
    size
    cells
)

; Construção e acesso ---------------------------------------------------------
; Obtém uma lista de regiões
(defun get-region-list (puzzle)
    (let ((region-index) (region-index-list '()) (region-list '()) (coords '()))
        (dotimes (i (* (Structure:puzzle-size puzzle)
                       (Structure:puzzle-size puzzle)
                    )
                 )
            (if (member (Structure:cell-region (nth i (Structure:puzzle-cells puzzle))) region-index-list)
                ()
                (progn
                    (setq region-index-list
                        (append region-index-list (list (Structure:cell-region (nth i (Structure:puzzle-cells puzzle)))))
                    )
                    (setq region-index (Structure:cell-region (nth i (Structure:puzzle-cells puzzle))))
                    (dotimes (j (* (Structure:puzzle-size puzzle)
                                (Structure:puzzle-size puzzle)
                                )
                            )
                        (if (= region-index (Structure:cell-region (nth j (Structure:puzzle-cells puzzle))))
                            (setq coords (append coords (list j)))
                            ()
                        )
                    )
                    (setq region-list (append region-list (list (make-region
                                                                    :index region-index
                                                                    :coords coords
                                                                )
                                                          )
                                      )
                    )
                    (setq coords '())
                )
            )
        )
        region-list
    )
)

(defun get-values-in-region (region puzzle)
    (loop for pos in (region-coords region)
        collect (cell-value (nth pos (puzzle-cells puzzle)))
    )
)

; Constrói um puzzle com o tamanho, uma lista de regiões e uma lista de valores
(defun build-puzzle (size region-list value-list)
    (make-puzzle
        :size size
        :cells (let ((i 0) (cell-list '()))
                    (loop
                        (when (= i (* size size)) (return cell-list))
                        (setq cell-list
                            (append cell-list
                                (list (make-cell :region (nth i region-list)
                                                    :value (nth i value-list)
                                                    :is-fixed (/= 0 (nth i value-list))
                                        )
                                )
                            )
                        )
                        (setq i (+ i 1))
                    )
                )
    )
)
