; Parser
; Lê o arquivo e obtém os dados não processados

(defpackage :Parser
    (:use :common-lisp)
    (:export :read-file
             :input-str-to-float
             :get-size
             :get-region-list
             :get-value-list))

(in-package :Parser)

; Auxiliares ------------------------------------------------------------------
; Seleciona um intervalo (de s a e-1) de números em uma lista.
(defun select-range (s e float-list)
    (let ((i s) (ret-list '()))  ; Inicializa o i na posição s
        (loop
            (when (= i e) (return ret-list))                             ; Condição de saída do i igual a e
            (setq ret-list (append ret-list (list (nth i float-list))))  ; Adiciona o valor na lista de retorno
            (setq i (+ i 1))
        )
    )
)

; Converte um string de floats em uma lista de floats
(defun parse-string-to-float-list (line)
    (with-input-from-string (s line)
        ; Itera sobre os números e os converte usando 'read'
        (loop
        :for num := (read s NIL NIL)
        :while num
        :collect num
        )
    )
)

; Tratamento de dados ---------------------------------------------------------
; Gera uma lista de floats a partir de uma lista de strings
(defun input-str-to-float (str-list)
    ; Converte cada string com vários números recursivamente e os concatena em uma lista
    (when str-list
        (concatenate 'list
            (parse-string-to-float-list (car str-list))
            (input-str-to-float (cdr str-list))
        )
    )
)

; Obtém o tamanho do puzzle
(defun get-size (puzzle-list)
    (car (select-range 0 1 puzzle-list))  ; Seleciona o primeiro elemento da lista
)

; Obtém a lista de indices das regiões do puzzle
(defun get-region-list (puzzle-list)
    (let ((size (get-size puzzle-list)))
        (select-range 1 (+ (* size size) 1) puzzle-list)  ; Seleciona os índices das regiões
    )
)

; Obtém a lista de valores do puzzle
(defun get-value-list (puzzle-list)
    (let ((size (get-size puzzle-list)))
        (select-range (+ (* size size) 1) (+ (* 2 (* size size)) 1) puzzle-list)  ; Seleciona os valores
    )
)

; Lê um arquivo
(defun read-file (filename)  ; Thanks Rainer Joswig and Frank Shearar
    ; Itera sobre um arquivo e lê linha por linha gerando uma lista de linhas
    (with-open-file (stream filename)
        (loop for line = (read-line stream NIL)
            while line
            collect line
        )
    )
)
