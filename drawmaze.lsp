(defun dibujar-laberinto (laberinto posicion-jugador posicion-meta)
  ; "Dibuja el laberinto en la ventana gráfica usando la función cuadrado."
  (cls) ; Limpia la ventana gráfica
  (let ((celda-tamaño 25) ; Tamaño de cada celda en píxeles
        (x 0)
        (y 0))
    (labels ((dibujar-celda (x y tipo)
               (cond
                ((eql tipo 'paret) (color 0 0 0))      ; Negro para paredes
                ((eql tipo 'cami) (color 255 255 255)) ; Blanco para caminos
                ((eql tipo 'entrada) (color 0 0 255))  ; Azul para la entrada
                ((eql tipo 'sortida) (color 255 0 0))) ; Rojo para la salida
               (move x y) ; Mueve el cursor a la posición (x, y)
               (quadrat celda-tamaño))) ; Dibuja el cuadrado de la celda
      (mapc (lambda (fila)
              (mapc (lambda (celda)
                      (dibujar-celda x y celda)
                      (setf x (+ x celda-tamaño))) ; Avanza en el eje X
                    fila)
              (setf x 0 y (+ y celda-tamaño))) ; Reinicia X y avanza en el eje Y
            laberinto))))

(defun quadrat (m)
 (drawrel m 0)
 (drawrel 0 m)
 (drawrel (- m) 0)
 (drawrel 0 (- m))
 (cond ((> m 0) (moverel 1 1) (quadrat (- m 1)))))
    