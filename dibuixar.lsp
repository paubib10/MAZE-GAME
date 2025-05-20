; ------------------- BLOQUE DE COMENTARIO -------------------
; Autores: Pau Toni Bibiloni Martínez & Hugo Guerreiro Paredes
; Fecha: 29 de abril de 2025
; Asignatura: Lenguajes de Programación
; Grupo: 3
; Profesores: Antoni Oliver Tomàs
; Convocatoria: Ordinaria
; ------------------------------------------------------------
  
; ---------------------- INSTRUCCIONES -----------------------
; Este archivo contiene las funciones necesarias para dibujar el laberinto y sus elementos.
; 
; Nota importante:
; - Esta clase no tiene un método funcional principal.
; - Sus funciones son auxiliares y están diseñadas para ser utilizadas en la clase `exploracio`.
;
; Pasos para usar las funciones:
; 1. Cargar el archivo:
;    - Ejecuta el comando: (load "dibuixar.lsp")
;    - Esto cargará todas las funciones necesarias para dibujar el laberinto.
;
; 2. Dibujar el laberinto completo:
;    - Función: (dibujar-laberinto laberint posicio-jugador posicio-meta ancho-ventana alto-ventana)
;    - Parámetros:
;        * laberint: Estructura del laberinto como lista de listas.
;        * posicio-jugador: Posición inicial del jugador en el laberinto.
;        * posicio-meta: Posición de la meta en el laberinto.
;        * ancho-ventana: Ancho de la ventana gráfica.
;        * alto-ventana: Alto de la ventana gráfica.
;    - Ejemplo: (dibujar-laberinto laberint '(0 0) '(9 9) 500 500)
;      Esto dibujará el laberinto con el jugador en la posición `(0, 0)` y la meta en `(9, 9)`.
;
; 3. Dibujar una celda específica:
;    - Función: (dibujar-celda x y tipo celda-tam)
;    - Parámetros:
;        * x, y: Coordenadas de la celda.
;        * tipo: Tipo de celda ('paret, 'cami, 'entrada, 'sortida, 'jugador).
;        * celda-tam: Tamaño de la celda.
;    - Ejemplo: (dibujar-celda 50 50 'jugador 20)
;      Esto dibujará una celda de tipo jugador en la posición `(50, 50)` con tamaño 20.
;
; ------------------ ASPECTOS OPCIONALES ---------------------
; 1. Ajuste dinámico del tamaño de las celdas:
;    - Implementado en la función (calcular-celda-tam).
;    - Calcula el tamaño de las celdas para que el laberinto se ajuste al tamaño de la ventana gráfica.
;
; 2. Representación gráfica de elementos:
;    - Cada tipo de celda tiene un color específico:
;        * Negro para paredes ('paret).
;        * Blanco para caminos ('cami).
;        * Azul para la entrada ('entrada).
;        * Rojo para la salida ('sortida).
;        * Verde para el jugador ('jugador).
;
; ------------------ DISEÑO FUNCIONAL ------------------------
; El programa está diseñado para dibujar el laberinto y sus elementos de manera eficiente:
; 1. Dibujar el laberinto completo:
;    - Se utiliza la función (dibujar-laberinto) para recorrer y dibujar todas las filas y columnas del laberinto.
;    - Las funciones (dibujar-filas) y (dibujar-columnas) manejan la recursión para dibujar cada celda.
;
; 2. Dibujar celdas individuales:
;    - La función (dibujar-celda) se encarga de dibujar una celda específica con el color y tamaño adecuados.
;
; 3. Ajuste dinámico:
;    - La función (calcular-celda-tam) asegura que las celdas se ajusten al tamaño de la ventana gráfica.
;
; ------------------------------------------------------------

; Dibuja la celda del jugador en su nueva posición.
; Parámetros:
;   posicio        -> Lista (x y) con la posición del jugador.
;   laberint       -> Laberinto como lista de listas.
;   ancho-ventana  -> Ancho de la ventana gráfica.
;   alto-ventana   -> Alto de la ventana gráfica.
(defun dibujar-celda-jugador (posicio laberint ancho-ventana alto-ventana)
  (let ((x (first posicio))
        (y (second posicio))
        (celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-celda (* y celda-tam) (* x celda-tam) 'jugador celda-tam)))

; Redibuja la celda en su estado original (sin el jugador).
; Parámetros:
;   laberint       -> Laberinto como lista de listas.
;   posicio        -> Lista (x y) con la posición a actualizar.
;   ancho-ventana  -> Ancho de la ventana gráfica.
;   alto-ventana   -> Alto de la ventana gráfica.
(defun actualizar-celda (laberint posicio ancho-ventana alto-ventana)
  (let* ((x (first posicio))
         (y (second posicio))
         (tipo (get-celda laberint x y)) ; Obtener el tipo original de la celda
         (celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-celda (* y celda-tam) (* x celda-tam) tipo celda-tam)))

; Dibuja todo el laberinto, incluyendo la posición del jugador y la meta.
; Parámetros:
;   laberint        -> Laberinto como lista de listas.
;   posicio-jugador -> Lista (x y) con la posición del jugador.
;   posicio-meta    -> Lista (x y) con la posición de la meta.
;   ancho-ventana   -> Ancho de la ventana gráfica.
;   alto-ventana    -> Alto de la ventana gráfica.
(defun dibujar-laberinto (laberint posicio-jugador posicio-meta ancho-ventana alto-ventana)
  (let ((celda-tam (calcular-celda-tam ancho-ventana alto-ventana laberint)))
    (dibujar-filas laberint 0 celda-tam posicio-jugador posicio-meta)))

; Dibuja todas las filas del laberinto recursivamente.
; Parámetros:
;   laberint        -> Laberinto como lista de listas.
;   fila-index      -> Índice de la fila actual.
;   celda-tam       -> Tamaño de cada celda.
;   posicio-jugador -> Lista (x y) con la posición del jugador.
;   posicio-meta    -> Lista (x y) con la posición de la meta.
(defun dibujar-filas (laberint fila-index celda-tam posicio-jugador posicio-meta)
  (cond
   ((null laberint) nil) ; Si no hay más filas, termina
   (t
    (dibujar-columnas (car laberint) fila-index 0 celda-tam posicio-jugador posicio-meta)
    (dibujar-filas (cdr laberint) (+ fila-index 1) celda-tam posicio-jugador posicio-meta))))

; Dibuja todas las columnas de una fila recursivamente.
; Parámetros:
;   fila            -> Lista con los elementos de la fila.
;   fila-index      -> Índice de la fila actual.
;   col-index       -> Índice de la columna actual.
;   celda-tam       -> Tamaño de cada celda.
;   posicio-jugador -> Lista (x y) con la posición del jugador.
;   posicio-meta    -> Lista (x y) con la posición de la meta.
(defun dibujar-columnas (fila fila-index col-index celda-tam posicio-jugador posicio-meta)
  (cond
   ((null fila) nil) ; Si no hay más columnas, termina
   (t
    (let ((tipo (cond
                 ((equal (list fila-index col-index) posicio-jugador) 'jugador)
                 ((equal (list fila-index col-index) posicio-meta) 'sortida)
                 (t (car fila)))))
      (dibujar-celda (* col-index celda-tam) (* fila-index celda-tam) tipo celda-tam))
    (dibujar-columnas (cdr fila) fila-index (+ col-index 1) celda-tam posicio-jugador posicio-meta))))

; Dibuja una celda en la posición (x, y) con el tipo especificado.
; Parámetros:
;   x         -> Coordenada x en la ventana.
;   y         -> Coordenada y en la ventana.
;   tipo      -> Tipo de celda ('paret, 'cami, 'entrada, 'sortida, 'jugador).
;   celda-tam -> Tamaño de la celda.
(defun dibujar-celda (x y tipo celda-tam)
  (cond
   ((eq tipo 'paret) (color 0 0 0))      ; Negro para paredes
   ((eq tipo 'cami) (color 255 255 255)) ; Blanco para caminos
   ((eq tipo 'entrada) (color 0 0 255))  ; Azul para la entrada
   ((eq tipo 'sortida) (color 255 0 0))  ; Rojo para la salida
   ((eq tipo 'jugador) (color 0 255 0))) ; Verde para el jugador
  (move x y)            ; Mueve el cursor a la posición (x, y)
  (quadrat celda-tam))  ; Dibuja el cuadrado de la celda

; Calcula el tamaño de las celdas dinámicamente para que el laberinto quepa en la ventana.
; Parámetros:
;   ancho-ventana -> Ancho de la ventana gráfica.
;   alto-ventana  -> Alto de la ventana gráfica.
;   laberinto     -> Laberinto como lista de listas.
(defun calcular-celda-tam (ancho-ventana alto-ventana laberinto)
  (let ((filas (contar-elementos laberinto))
        (columnas (contar-elementos (car laberinto))))
    (floor (min (/ ancho-ventana columnas) (/ alto-ventana filas)))))

; Dibuja un cuadrado relleno de tamaño m en la posición actual.
; Parámetros:
;   m -> Tamaño del cuadrado.  
(defun quadrat (m)
 (drawrel m 0)
 (drawrel 0 m)
 (drawrel (- m) 0)
 (drawrel 0 (- m))
 (cond ((> m 0) (moverel 1 1) (quadrat (- m 1)))))

; ------------------ FUNCIONES AUXILIARES -------------------

; Cuenta los elementos de una lista de forma recursiva.
; Parámetros:
;   lista -> Lista a contar.
 (defun contar-elementos (lista)
  (cond
   ((null lista) 0)
   (t (+ 1 (contar-elementos (cdr lista))))))
    