(load "dibuixar.lsp")

; ---------------------------- EXPLORA -------------------------------
(defun explora (nombre-fichero)
  ; "Carga el laberinto desde un archivo y lo dibuja en la ventana gráfica."
  (let* ((laberinto (cargar-laberinto nombre-fichero))
         (posicion-jugador (buscar-posicion laberinto 'entrada))
         (posicion-meta (buscar-posicion laberinto 'sortida)))
    (cond
     ((and posicion-jugador posicion-meta)
      (dibujar-laberinto laberinto posicion-jugador posicion-meta))
     (t (format t "El laberinto no tiene entrada o salida validas.~%")))))

; -------------------------- CARGAR_LABERINTO ---------------------------
(defun cargar-laberinto (nombre-fichero)
  ; "Carga el laberinto desde un archivo de texto y lo convierte en una lista de listas."
  (let ((fp (open nombre-fichero :direction :input)))
    (let ((laberinto (cargar-lineas-rec fp)))
      (close fp)
      laberinto)))

(defun cargar-lineas-rec (fp)
  ; "Lee las líneas del archivo y las convierte en listas de símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) nil) ; Si es el final del archivo, termina
     ((eq caracter #\Newline) ; Si es un salto de línea, procesa la siguiente línea
      (cons nil (cargar-lineas-rec fp)))
     (t ; Acumula caracteres en la línea actual
      (let ((linea (cargar-linea-rec fp (list (caracter-a-simbolo caracter)))))
        (cons linea (cargar-lineas-rec fp)))))))

(defun cargar-linea-rec (fp acumulador)
  ; "Lee caracteres de una línea y los convierte en símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) (reverse acumulador)) ; Fin del archivo
     ((eq caracter #\Newline) (reverse acumulador)) ; Fin de la línea
     (t (cargar-linea-rec fp (cons (caracter-a-simbolo caracter) acumulador))))))

; -------------------------- BUSCAR_POSICION -----------------------------
(defun buscar-posicion (laberinto simbolo)
  ; "Busca la posición de un símbolo en el laberinto."
  (buscar-en-laberinto laberinto simbolo 0))

(defun buscar-en-laberinto (laberinto simbolo fila-index)
  ; "Busca el símbolo en el laberinto fila por fila."
  (cond
   ((null laberinto) nil)
   (t (let ((pos (buscar-en-fila (car laberinto) simbolo fila-index 0)))
        (cond
         (pos pos)
         (t (buscar-en-laberinto (cdr laberinto) simbolo (+ fila-index 1))))))))

(defun buscar-en-fila (fila simbolo fila-index col-index)
  ; "Busca el símbolo en una fila específica."
  (cond
   ((null fila) nil)
   ((eq (car fila) simbolo) (list fila-index col-index))
   (t (buscar-en-fila (cdr fila) simbolo fila-index (+ col-index 1)))))

; -------------------------- FUNCIONES AUXILIARES ------------------------
(defun caracter-a-simbolo (caracter)
  ; "Convierte un carácter del archivo a su representación simbólica."
  (cond
   ((eq caracter #\#) 'paret)
   ((eq caracter #\.) 'cami)
   ((eq caracter #\e) 'entrada)
   ((eq caracter #\s) 'sortida)
   (t 'desconocido)))