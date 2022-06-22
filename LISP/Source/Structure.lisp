; Structure

(defpackage :Structure
    (:use :common-lisp)
    (:export :Region
             :Cell
             :Puzzle))

(in-package :Structure)

(defstruct Region
    index
    coords
)

(defstruct Cell
    region
    value
    isFixed
)

(defstruct Puzzle
    size
    cells
)
