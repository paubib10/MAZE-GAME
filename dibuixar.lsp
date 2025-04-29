; ------------------- BLOQUE DE COMENTARIO -------------------
; Autores: Pau Toni Bibiloni Martínez & Hugo Guerreiro Paredes
; Fecha: 29 de abril de 2025
; Asignatura: Lenguajes de Programación
; Grupo: 3
; Profesores: Antoni Oliver Tomàs
; Convocatoria: Ordinaria
; ------------------------------------------------------------

(defun dibujar-celda-jugador (posicio laberint ancho-ventana alto-ventana)
  "Dibuja la celda del jugador en su nueva posición."
  (let ((x (first posicio))
        (y (second posicio))
        (celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-celda (* y celda-tam) (* x celda-tam) 'jugador celda-tam)))

(defun actualizar-celda (laberint posicio ancho-ventana alto-ventana)
  "Redibuja la celda en su estado original (sin el jugador)."
  (let* ((x (first posicio))
         (y (second posicio))
         (tipo (get-celda laberint x y)) ; Obtener el tipo original de la celda
         (celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-celda (* y celda-tam) (* x celda-tam) tipo celda-tam)))

(defun dibujar-laberinto (laberint posicio-jugador posicio-meta ancho-ventana alto-ventana)
  "Dibuja todo el laberinto, incluyendo la posición del jugador y la meta."
  (let ((celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-filas laberint 0 celda-tam posicio-jugador posicio-meta)))

(defun dibujar-filas (laberint fila-index celda-tam posicio-jugador posicio-meta)
  "Dibuja todas las filas del laberinto recursivamente."
  (cond
   ((null laberint) nil) ; Si no hay más filas, termina
   (t
    (dibujar-columnas (car laberint) fila-index 0 celda-tam posicio-jugador posicio-meta)
    (dibujar-filas (cdr laberint) (+ fila-index 1) celda-tam posicio-jugador posicio-meta))))

(defun dibujar-columnas (fila fila-index col-index celda-tam posicio-jugador posicio-meta)
  "Dibuja todas las columnas de una fila recursivamente."
  (cond
   ((null fila) nil) ; Si no hay más columnas, termina
   (t
    (let ((tipo (cond
                 ((equal (list fila-index col-index) posicio-jugador) 'jugador)
                 ((equal (list fila-index col-index) posicio-meta) 'sortida)
                 (t (car fila)))))
      (dibujar-celda (* col-index celda-tam) (* fila-index celda-tam) tipo celda-tam))
    (dibujar-columnas (cdr fila) fila-index (+ col-index 1) celda-tam posicio-jugador posicio-meta))))

(defun dibujar-celda (x y tipo celda-tam)
  "Dibuja una celda en la posición (x, y) con el tipo especificado."
  (cond
   ((eq tipo 'paret) (color 0 0 0))      ; Negro para paredes
   ((eq tipo 'cami) (color 255 255 255)) ; Blanco para caminos
   ((eq tipo 'entrada) (color 0 0 255))  ; Azul para la entrada
   ((eq tipo 'sortida) (color 255 0 0))  ; Rojo para la salida
   ((eq tipo 'jugador) (color 0 255 0))) ; Verde para el jugador
  (move x y) ; Mueve el cursor a la posición (x, y)
  (quadrat celda-tam)) ; Dibuja el cuadrado de la celda

(defun calcular-celda-tam (ancho-ventana alto-ventana laberinto)
  "Calcula el tamaño de las celdas dinámicamente para que el laberinto quepa en la ventana."
  (let ((filas (contar-elementos laberinto))
        (columnas (contar-elementos (car laberinto))))
    (floor (min (/ ancho-ventana columnas) (/ alto-ventana filas)))))
    
(defun quadrat (m)
 (drawrel m 0)
 (drawrel 0 m)
 (drawrel (- m) 0)
 (drawrel 0 (- m))
 (cond ((> m 0) (moverel 1 1) (quadrat (- m 1)))))

 ; ------------------ FUNCIONES AUXILIARES -------------------
 (defun contar-elementos (lista)
  "Cuenta los elementos de una lista de forma recursiva."
  (cond
   ((null lista) 0)
   (t (+ 1 (contar-elementos (cdr lista))))))
    