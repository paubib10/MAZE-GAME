(load "drawmaze.lsp") ; Cargar las funciones de dibujo

(defun explora (nombre-fichero)
  ; "Carga el laberinto desde un archivo y lo dibuja en la ventana gráfica."
  (let* ((laberinto (cargar-laberinto nombre-fichero))
         (posicion-jugador (buscar-posicion laberinto 'entrada))
         (posicion-meta (buscar-posicion laberinto 'sortida)))
    (if (and posicion-jugador posicion-meta)
        (dibujar-laberinto laberinto posicion-jugador posicion-meta)
        (format t "El laberinto no tiene entrada o salida válidas.~%"))))

(defun cargar-laberinto (nombre-fichero)
  ; "Carga el laberinto desde un archivo de texto y lo convierte en una lista de listas."
  (let ((fp (open nombre-fichero :direction :input)))
    (labels ((leer-lineas ()
               (let ((linea (read-line fp nil)))
                 (if linea
                     (cons (mapcar #'caracter-a-simbolo (coerce linea 'list))
                           (leer-lineas))
                     nil))))
      (let ((laberinto (leer-lineas)))
        (close fp)
        laberinto))))

(defun caracter-a-simbolo (caracter)
  ; "Convierte un carácter del archivo a su representación simbólica."
  (cond
   ((char= caracter #\#) 'paret)
   ((char= caracter #\.) 'cami)
   ((char= caracter #\e) 'entrada)
   ((char= caracter #\s) 'sortida)
   (t 'desconocido)))

(defun buscar-posicion (laberinto simbolo)
  ; "Busca la posición de un símbolo en el laberinto."
  (labels ((buscar-en-fila (fila simbolo fila-index col-index)
             (cond
              ((null fila) nil)
              ((eql (car fila) simbolo) (list fila-index col-index))
              (t (buscar-en-fila (cdr fila) simbolo fila-index (+ col-index 1)))))
           (buscar-en-laberinto (laberinto simbolo fila-index)
             (cond
              ((null laberinto) nil)
              (t (let ((pos (buscar-en-fila (car laberinto) simbolo fila-index 0)))
                   (if pos
                       pos
                       (buscar-en-laberinto (cdr laberinto) simbolo (+ fila-index 1))))))))
    (buscar-en-laberinto laberinto simbolo 0)))