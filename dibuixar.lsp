(defun dibujar-laberinto (laberinto posicion-jugador posicion-meta)
  ; "Dibuja el laberinto en la ventana gráfica usando la función cuadrado."
  (cls) ; Limpia la ventana gráfica
  (let* ((ancho-ventana 640) ; Ancho de la ventana gráfica
         (alto-ventana 375)  ; Alto de la ventana gráfica
         (filas (contar-elementos laberinto)) ; Número de filas del laberinto
         (columnas (contar-elementos (car laberinto))) ; Número de columnas del laberinto
         (celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberinto)))
    (dibujar-filas (reverse laberinto) 0 0 0 celda-tam) ; Dibuja el laberinto
    (dibujar-jugador posicion-jugador celda-tam filas))) ; Dibuja el jugador

(defun calcular-celda-tam (ancho-ventana alto-ventana laberinto)
  ; "Calcula el tamaño de las celdas dinámicamente."
  (let ((filas (contar-elementos laberinto))
        (columnas (contar-elementos (car laberinto))))
    (floor (min (/ ancho-ventana columnas) (/ alto-ventana filas)))))

(defun contar-elementos (lista)
  ; "Cuenta los elementos de una lista de forma recursiva."
  (cond
   ((null lista) 0)
   (t (+ 1 (contar-elementos (cdr lista))))))

(defun dibujar-filas (laberinto fila-actual x y celda-tam)
  ; "Dibuja las filas del laberinto recursivamente."
  (cond
   ((null laberinto) nil)
   (t (dibujar-columnas (car laberinto) x y celda-tam)
      (dibujar-filas (cdr laberinto) (+ fila-actual 1) 0 (+ y celda-tam) celda-tam))))

(defun dibujar-columnas (fila x y celda-tam)
  ; "Dibuja las columnas de una fila recursivamente."
  (cond
   ((null fila) nil)
   (t (dibujar-celda x y (car fila) celda-tam)
      (dibujar-columnas (cdr fila) (+ x celda-tam) y celda-tam))))

(defun dibujar-celda (x y tipo celda-tam)
  ; "Dibuja una celda en la posición (x, y) con el tipo especificado."
  (cond
   ((eq tipo 'paret) (color 0 0 0))       ; Negro para paredes
   ((eq tipo 'cami) (color 255 255 255)) ; Blanco para caminos
   ((eq tipo 'entrada) (color 0 0 255))  ; Azul para la entrada
   ((eq tipo 'sortida) (color 255 0 0))  ; Rojo para la salida
   ((eq tipo 'jugador) (color 0 255 0))) ; Verde para el jugador
  (move x y) ; Mueve el cursor a la posición (x, y)
  (quadrat celda-tam)) ; Dibuja el cuadrado de la celda

(defun dibujar-jugador (posicion-jugador celda-tam filas)
  ; "Dibuja el jugador en la posición especificada, ajustando por el reverse."
  (let ((x (* (second posicion-jugador) celda-tam))
        (y (* (- filas 1 (first posicion-jugador)) celda-tam))) ; Ajusta la fila
    (color 0 255 0) ; Verde para el jugador
    (move x y) ; Mueve el cursor a la posición del jugador
    (quadrat celda-tam))) ; Dibuja el cuadrado del jugador

(defun quadrat (m)
 (drawrel m 0)
 (drawrel 0 m)
 (drawrel (- m) 0)
 (drawrel 0 (- m))
 (cond ((> m 0) (moverel 1 1) (quadrat (- m 1)))))
    