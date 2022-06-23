; Structure

(defpackage :Structure
    (:use :common-lisp)
    (:export :region
             :cell
             :puzzle
             :puzzle-size
             :puzzle-cells
             :buildPuzzle))

(in-package :Structure)

(defstruct region
    index
    coords
)

(defstruct cell
    region
    value
    isFixed
)

(defstruct puzzle
    size
    cells
)

; Constrói um puzzle com o tamanho, uma lista de regiões e uma lista de valores
(defun buildPuzzle (size regionList valueList)
    (make-puzzle
        :size size
        :cells (dotimes (i size)
                    collect (make-cell
                                :region (nth i regionList)
                                :value (nth i valueList)
                                :isFixed (= 0 (nth i valueList))))
    )
)
