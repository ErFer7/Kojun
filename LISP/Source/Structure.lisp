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

; Funcao auxiliar de pegar regioes
(defun getRegion (index regionsList)
    (if null regionsList
        '()
		(if (= index (region-index (car regionList)))
			(car regionList)
			(getRegion index (cdr regionsList))
		)
    )
)

; Retorna lista de todas as regioes no puzzle, sla se funciona
(defun getRegions (size regionMap)
	(setq regionList '())
	(dotimes i size
		(setq region = (getRegion (nth i regionMap) regionList))
		(if (null region)
			(setq regionList
				(cons (make-region :index (nth i regionMap) :coords '(i)) regionList))
			(setq region
				(cons i (region-coords region)))
		)
		(setq (nth (nth i regionMap) regionList) region)
	)
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
