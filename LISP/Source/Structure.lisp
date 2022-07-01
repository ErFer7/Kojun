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
             :build-puzzle
             :get-regions))

(in-package :Structure)

(defstruct region
    index
    coords
)

(defstruct cell
    region
    value
    is-fixed
)

(defstruct puzzle
    size
    cells
)

; bool - n presente na lista
(defun contains (n lista)
    (if (null lista)
        NIL
        (if (= n (car lista))
            (T)
            (contains n (cdr lista))
        )
    )
)

(defun getRegion (index pos regionList)
    (if (null regionList)
        '()
        (if (= index (nth pos regionList))
            (cons pos (getRegion index (+ pos 1) regionList))
            (getRegion index (+ pos 1) regionList)
        )
    )
)

(defun getRegionsAux (index pos regionList)
    (if (>= pos (length regionList))
        '()
        (if (> (nth pos regionList) index)
            (cons
                (make-region
                    :index index
                    :coords (getRegion index pos regionList)
                )
                (getRegionsAux (+ index 1) (+ pos 1) regionList))
            (getRegionsAux index (+ pos 1) regionList)
        )
    )
)

(defun get-regions (regionList)
    (getRegionsAux 0 0 regionList)
)

; Constrói um puzzle com o tamanho, uma lista de regiões e uma lista de valores
(defun build-puzzle (size region-list value-list)
    (make-puzzle
        :size size
        :cells (let (i)
                    (setq i 0)
                    (let (cell-list)
                        (setq cell-list '())
                        (loop
                            (when (= i (* size size)) (return cell-list))
                            (setq cell-list
                                (append cell-list
                                    (list (make-cell :region (nth i region-list)
                                                     :value (nth i value-list)
                                                     :is-fixed (= 0 (nth i value-list))
                                          )
                                    )
                                )
                            )
                            (setq i (+ i 1))
                        )
                    )
                )
    )
)
