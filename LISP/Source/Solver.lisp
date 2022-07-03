; Solver
; Resolve o puzzle

(defpackage :Solver
    (:use :common-lisp)
    (:export :region-backtracking))

(in-package :Solver)

; Auxiliares ------------------------------------------------------------------
; Verifica se todos os valores adjacentes à célula são diferentes
(defun check-orthogonal-difference (pos puzzle)
    ; O acesso às coordenadas usa cálculos que convertem a posição para valores bidimensionais
    (and
        (and
            ; Superior
            (or
                ; Verifica se a posição está no canto superior
                (= (floor pos (Structure:puzzle-size puzzle)) 0)
                ; Verifica se as células são diferentes
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ; Inferior
            (or
                ; Verifica se a posição está no canto inferior
                (= (floor pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                ; Verifica se as células são diferentes
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
                ; Verifica se a posição está no canto esquerdo
                (= (mod pos (Structure:puzzle-size puzzle)) 0)
                ; Verifica se as células são diferentes
                (not (= (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                        (Structure:cell-value
                            (nth (- pos 1) (Structure:puzzle-cells puzzle))
                        )
                     )
                )
            )
            ; Direita
            (or
                ; Verifica se a posição está no canto direito
                (= (mod pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                ; Verifica se as células são diferentes
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

; Verifica se a célula é menor que a superior e maior que a inferior
(defun check-vertical-greatness (pos puzzle)
    (and
        (or
            (or
                ; Verifica se a posição está no canto inferior
                (= (floor pos (Structure:puzzle-size puzzle)) (- (Structure:puzzle-size puzzle) 1))
                ; Verifica se a célula inferior é 0
                (= (Structure:cell-value
                        (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   0
                )
            )
            (or
                ; Verifica se a célula inferior é menor
                (< (Structure:cell-value
                        (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                )
                ; Verifica se a célula inferior está em uma região diferente
                (not (= (Structure:cell-region
                            (nth (+ pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                        )
                        (Structure:cell-region (nth pos (Structure:puzzle-cells puzzle)))
                     )
                )
            )
        )
        (or
            (or
                ; Verifica se a posição está no canto superior
                (= (floor pos (Structure:puzzle-size puzzle)) 0)
                ; Verifica se a célula superior é 0
                (= (Structure:cell-value
                        (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   0
                )
            )
            (or
                ; Verifica se a célula superior é menor
                (> (Structure:cell-value
                        (nth (- pos (Structure:puzzle-size puzzle)) (Structure:puzzle-cells puzzle))
                   )
                   (Structure:cell-value (nth pos (Structure:puzzle-cells puzzle)))
                )
                ; Verifica se a célula superior está em uma região diferente
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

; Verifica se a célula está de acordo com as regras
(defun check-cell (pos puzzle)
    (and
        (check-orthogonal-difference pos puzzle)
        (check-vertical-greatness pos puzzle)
    )
)

; Obtém os valores possíveis para uma região
(defun get-possible-values (values)
    (let ((possible-values '()))
        ; Itera pelos valores atuais e adiciona os valores que não foram encontrados em uma lista
        (dotimes (i (length values))
            (if (member (+ i 1) values)
                ()
                (setq possible-values (append possible-values (list (+ i 1))))
            )
        )
        possible-values
    )
)

; Obtém uma lista com todas as permutações possíveis de uma lista
(defun get-permutations (l)
    ; Condições de recursão
    (cond
        ((null l) nil)             ; Lista vazia
        ((null (cdr l)) (list l))  ; Lista com um só elemento
        (t (loop for element in l
                ; Adiciona a permutação na lista e obtém a próxima permutação recursivamente
                append (mapcar (lambda (l) (cons element l)) (get-permutations (remove element l)))
           )
        )
    )
)

; Insere uma permutação de valores na região e verifica se existe uma combinação válida ou não
(defun insert-value-list (coords values puzzle)
    (let ((is-valid NIL) (random-permutation))
        ; Itera sobre as permutações possíveis
        (loop
            ; Condições de saída: Acabaram as permutações ou uma permutação válida foi encontrada
            (when (or (null values) is-valid) (return (cons is-valid puzzle)))
            ; Obtém uma permutação da lista e forma aleatória 
            (setq random-permutation (nth (random (length values)) values))
            ; Apaga a permutação a lista
            (setq values (remove random-permutation values))
            ; Remove as coordenadas de valores fixos da lista de coordenadas
            (let ((i 0))
                (loop
                    (when (>= i (length coords)) (return coords))
                    (if (Structure:cell-is-fixed (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                        (setq coords (remove (nth i coords) coords))
                        (setq i (+ i 1))
                    )
                )
            )
            ; Insere os valores da permutação na região e verifica a cada inserção se o valor é valido, caso não seja
            ; a inserção para e essa permutação é dada como inválida.
            (dotimes (i (length coords))
                (if (Structure:cell-is-fixed (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                    ()
                    (progn
                        ; Inserção de um valor
                        (setf (Structure:cell-value (nth (nth i coords) (Structure:puzzle-cells puzzle)))
                              (nth i random-permutation)
                        )
                        ; Checagem do valor
                        (if (check-cell (nth i coords) puzzle)
                            (setq is-valid T)
                            (setq is-valid NIL)
                        )
                        ; Verificação da validade da permutação
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

; Reseta as células com as coordenadas na lista
(defun reset-at-positions (coords puzzle)
    ; Itera por cada coordenada e define o valor da célula como 0
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
            ; Encerra o loop quando todo o puzzle for resolvido
            (when (> r (- (Structure:region-index (nth (- (length region-list) 1) region-list)) 1)) (return puzzle))

            ; Obtém a região atual
            (setq region (nth r region-list))

            ; Insere os permutados em uma região
            (setq insertion
                (insert-value-list
                    (Structure:region-coords region)
                    (get-permutations (get-possible-values (Structure:get-values-in-region region puzzle)))
                    puzzle
                )
            )

            ; Se a inserção for válida
            (if (car insertion)
                (progn
                    (setq puzzle (cdr insertion))  ; Atualiza o puzzle
                    (setq r (+ r 1))               ; Avança uma região
                )
                (progn
                    (setq error-count (+ error-count 1))  ; Aumenta a contagem de erros
                    ; Se a contagem de erros for maior que 4 o offset de backtracking aumenta
                    (if (> error-count 4)
                        (progn
                            (setq jump-offset (+ jump-offset 1))
                            (setq error-count 0)
                        )
                        ()
                    )
                    ; Verifica se a região menos o offset de backtracking é um valor maior ou igual a 0
                    (progn
                        ; Define a região alvo para o backtracking evitando que ela seja negativa
                        (if (>= (- r jump-offset) 0)
                            (setq jump-region (- r jump-offset))
                            (setq jump-region 0)
                        )
                        ; Retorna até a região alvo apagando todas as outras no caminho
                        (loop
                            (setq r (- r 1))
                            (if (>= r jump-region)
                                (progn
                                    (setq region (nth r region-list))
                                    (setq puzzle (reset-at-positions (Structure:region-coords region) puzzle))
                                )
                                (progn
                                    (setq r jump-region)  ; Corrige r
                                    (return puzzle)       ; Encerra o loop
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
