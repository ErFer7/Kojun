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
             :build-puzzle))

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

; Funcao auxiliar de pegar regioes
; (defun getRegion (index regionsList)
;     (if null regionsList
;         '()
; 		(if (= index (region-index (car regionList)))
; 			(car regionList)
; 			(getRegion index (cdr regionsList))
; 		)
;     )
; )
;
; ; Retorna lista de todas as regioes no puzzle, sla se funciona
; (defun getRegions (size regionMap)
; 	(setq regionList '())
; 	(dotimes i size
; 		(setq region = (getRegion (nth i regionMap) regionList))
; 		(if (null region)
; 			(setq regionList
; 				(cons (make-region :index (nth i regionMap) :coords '(i)) regionList))
; 			(setq region
; 				(cons i (region-coords region)))
; 		)
; 		(setq (nth (nth i regionMap) regionList) region)
; 	)
; )

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
