; solver

(defun count (n lista))

(defun removeN (n lista))

(defun checkOrthogonalDifference (pos puzzle))

(defun checkVerticalGreatness (pos puzzle))

(defun checkCell (pos puzzle)
  (and
    (checkOrthogonalDifference pos puzzle)
    (checkVerticalGreatness pos puzzle)
  )
)

(defun getPossibleValues)

(defun insertValues)

(defun resetToN)

(defun cellBacktrackingAux)

(defun cellBacktracking)
