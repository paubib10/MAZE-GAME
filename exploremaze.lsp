(defun imprimir-laberinto (nombre-fichero)
  ; "Lee el laberinto desde un archivo y lo imprime en la consola."
  (let ((laberinto (cargar-laberinto nombre-fichero)))
    (labels ((imprimir-fila (fila)
               (cond
                ((null fila) (terpri)) ; Salto de línea al final de la fila
                (t (princ (simbolo-a-caracter (car fila)))
                   (imprimir-fila (cdr fila)))))
             (imprimir-laberinto-rec (laberinto)
               (cond
                ((null laberinto) nil)
                (t (imprimir-fila (car laberinto))
                   (imprimir-laberinto-rec (cdr laberinto))))))
      (imprimir-laberinto-rec laberinto))))

(defun simbolo-a-caracter (simbolo)
  ; "Convierte un símbolo del laberinto a su representación de carácter."
  (cond
   ((eql simbolo 'paret) #\#)
   ((eql simbolo 'cami) #\.)
   ((eql simbolo 'entrada) #\e)
   ((eql simbolo 'sortida) #\s)
   (t #\?))) ; Carácter desconocido

; (imprimir-laberinto "laberinto.txt")