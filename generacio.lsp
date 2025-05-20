; ------------------- BLOQUE DE COMENTARIO -------------------
; Autores: Pau Toni Bibiloni Martínez & Hugo Guerreiro Paredes
; Fecha: 29 de abril de 2025
; Asignatura: Lenguajes de Programación
; Grupo: 3
; Profesores: Antoni Oliver Tomàs
; Convocatoria: Ordinaria
; ------------------------------------------------------------

; ---------------------- INSTRUCCIONES -----------------------
; Este archivo contiene las funciones necesarias para generar laberintos.
; 
; Pasos para usar las funciones:
; 1. Cargar el archivo:
;    - Ejecuta el comando: (load "generacio.lsp")
;    - Esto cargará todas las funciones necesarias para generar laberintos.
;
; 2. Generar un laberinto:
;    - Función: (genera nom-fitxer files columnes)
;    - Parámetros:
;        * nom-fitxer: Nombre del archivo donde se guardará el laberinto.
;        * files: Número de filas del laberinto.
;        * columnes: Número de columnas del laberinto.
;    - Ejemplo: (genera "laberint.txt" 10 10)
;      Esto generará un laberinto de 10x10 y lo guardará en el archivo "laberint.txt".
;
; ------------------ ASPECTOS OPCIONALES ---------------------
; 1. Generación aleatoria de entrada y salida:
;    - Implementado en las funciones (seleccionar-entrada) y (seleccionar-salida).
;    - Estas funciones seleccionan posiciones aleatorias dentro del laberinto para la entrada y la salida.
;
; 2. Algoritmo DFS para generar caminos:
;    - Implementado en las funciones (dfs-generar-laberinto) y (dfs-visitar-vecinos).
;    - Estas funciones generan caminos en el laberinto utilizando un algoritmo de búsqueda en profundidad (DFS).
;
; ------------------ DISEÑO FUNCIONAL ------------------------
; El programa está diseñado para generar laberintos de manera eficiente y flexible:
; 1. Creación del laberinto:
;    - Se utiliza la función (crear-laberinto-vacio) para crear un laberinto inicial lleno de paredes.
;    - Se añaden bordes al laberinto con la función (agregar-bordes).
;
; 2. Generación de caminos:
;    - Se utiliza un algoritmo DFS para generar caminos aleatorios en el laberinto.
;    - Las funciones (mezclar-direcciones) y (es-pared-valida) aseguran que los caminos sean válidos.
;
; 3. Escritura del laberinto en un archivo:
;    - La función (guardar-laberinto) escribe el laberinto generado en un archivo de texto.
;    - Cada celda del laberinto se convierte en un carácter con la función (celda-a-caracter).
;
; ------------------------------------------------------------

; Genera un laberinto de tamaño (n-2)x(m-2) usando DFS recursivo y lo guarda en un archivo.
; Parámetros:
;   nom-fitxer -> Nombre del archivo donde se guardará el laberinto.
;   n          -> Número de filas totales (incluyendo bordes).
;   m          -> Número de columnas totales (incluyendo bordes).
(defun genera (nom-fitxer n m)
  (let* ((n-reducido (- n 2))
         (m-reducido (- m 2))
         (laberinto (crear-laberinto-vacio n-reducido m-reducido)))
    (let ((entrada (seleccionar-entrada laberinto n-reducido m-reducido)))
      (let ((laberinto-con-entrada (colocar-entrada laberinto entrada)))
        (let ((laberinto-generado (dfs-generar-laberinto laberinto-con-entrada entrada n-reducido m-reducido)))
          (let ((salida (seleccionar-salida laberinto-generado n-reducido m-reducido)))
            (let ((laberinto-final (colocar-salida laberinto-generado salida)))
              (let ((laberinto-final-con-entrada (colocar-entrada laberinto-final entrada)))
                (let ((laberinto-con-bordes (agregar-bordes laberinto-final-con-entrada)))
                  (guardar-laberinto nom-fitxer laberinto-con-bordes)
                  laberinto-con-bordes)))))))))

; Crea una lista de N listas con M paredes cada una.
; Parámetros:
;   n -> Número de filas.
;   m -> Número de columnas.
(defun crear-laberinto-vacio (n m)
  (construir-filas n m))

; Construye una lista de filas, cada una con columnas de paredes.
; Parámetros:
;   filas    -> Número de filas.
;   columnas -> Número de columnas por fila.
(defun construir-filas (filas columnas)
  (cond ((= filas 0) nil)
        (t (cons (construir-fila columnas)
                 (construir-filas (- filas 1) columnas)))))

; Construye una fila con columnas de paredes.
; Parámetros:
;   columnas -> Número de columnas en la fila.
(defun construir-fila (columnas)
  (cond ((= columnas 0) nil)
        (t (cons 'paret (construir-fila (- columnas 1))))))

; Agrega una capa de paredes alrededor del laberinto.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
(defun agregar-bordes (laberinto)
  (let* ((ancho (+ 2 (contar-elementos (car laberinto))))
         (fila-borde (construir-fila ancho)))
    (cons fila-borde
          (append (mapcar (lambda (fila)
                            (cons 'paret (append fila '(paret))))
                          laberinto)
                  (list fila-borde)))))

; Selecciona una posición aleatoria dentro del laberinto como entrada.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   n         -> Número de filas internas.
;   m         -> Número de columnas internas.
(defun seleccionar-entrada (laberinto n m)
  (list (random n) (random m)))

; Selecciona una posición aleatoria dentro del laberinto como salida.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   n         -> Número de filas internas.
;   m         -> Número de columnas internas.
(defun seleccionar-salida (laberinto n m)
  (list (random n) (random m)))

; Coloca la entrada en la posición especificada.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   pos       -> Lista (fila columna) con la posición de la entrada.
(defun colocar-entrada (laberinto pos)
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'entrada))

; Coloca la salida en la posición especificada.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   pos       -> Lista (fila columna) con la posición de la salida.
(defun colocar-salida (laberinto pos)
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'sortida))

; Mezcla aleatoriamente una lista de direcciones.
; Parámetros:
;   direcciones -> Lista de direcciones (listas de dos elementos).
(defun mezclar-direcciones (direcciones)
  (cond
   ((null direcciones) nil) ; Caso base: lista vacía
   (t (let ((indice (random (contar-elementos direcciones))))
        (let ((elemento (obtener-elemento direcciones indice))
              (resto (eliminar-indice indice direcciones)))
          (cons elemento (mezclar-direcciones resto)))))))

; Elimina un elemento de la lista por índice.
; Parámetros:
;   indice -> Índice del elemento a eliminar.
;   lista  -> Lista de la que eliminar el elemento.
(defun eliminar-indice (indice lista)
  (cond
   ((null lista) nil) ; Caso base: lista vacía
   ((= indice 0) (cdr lista)) ; Si el índice es 0, elimina el primer elemento
   (t (cons (car lista) (eliminar-indice (- indice 1) (cdr lista))))))

; Obtiene el elemento en la posición `indice` de una lista de forma recursiva.
; Parámetros:
;   lista  -> Lista de la que obtener el elemento.
;   indice -> Índice del elemento.
(defun obtener-elemento (lista indice)
  (cond
   ((null lista) nil) ; Caso base: lista vacía
   ((= indice 0) (car lista)) ; Caso base: índice 0, devuelve el primer elemento
   (t (obtener-elemento (cdr lista) (- indice 1))))) ; Llamada recursiva

; Genera caminos en el laberinto usando DFS.
; Parámetros:
;   laberinto  -> Laberinto como lista de listas.
;   pos-actual -> Posición actual (lista de dos elementos).
;   n          -> Número de filas internas.
;   m          -> Número de columnas internas.
(defun dfs-generar-laberinto (laberinto pos-actual n m)
  (let ((laberinto-actualizado (reemplazar-posicion laberinto (car pos-actual) (cadr pos-actual) 'cami)))
    (let ((dirs (mezclar-direcciones '((0 -1) (1 0) (0 1) (-1 0))))) ; Direcciones: izquierda, abajo, derecha, arriba
      (dfs-visitar-vecinos laberinto-actualizado pos-actual dirs n m))))

; Visita las celdas vecinas recursivamente usando DFS.
; Parámetros:
;   laberinto  -> Laberinto como lista de listas.
;   pos-actual -> Posición actual.
;   dirs       -> Lista de direcciones a visitar.
;   n          -> Número de filas internas.
;   m          -> Número de columnas internas.
(defun dfs-visitar-vecinos (laberinto pos-actual dirs n m)
  (cond
   ((null dirs) laberinto) ; Caso base: no hay más direcciones
   (t (let ((dir (car dirs)))
        (let ((nueva-pos (list (+ (car pos-actual) (car dir))
                               (+ (cadr pos-actual) (cadr dir)))))
          (if (es-pared-valida laberinto nueva-pos n m)
              (dfs-visitar-vecinos
               (dfs-generar-laberinto laberinto nueva-pos n m)
               pos-actual
               (cdr dirs)
               n m)
              (dfs-visitar-vecinos laberinto pos-actual (cdr dirs) n m)))))))

; Verifica si una celda es una pared válida para ser convertida en camino.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   pos       -> Posición a comprobar.
;   n         -> Número de filas internas.
;   m         -> Número de columnas internas.
(defun es-pared-valida (laberinto pos n m)
  (and (es-posicion-valida pos n m)
       (eq (obtener-celda laberinto (car pos) (cadr pos)) 'paret)
       (= (contar-celdas-camino-adyacentes laberinto pos n m) 1)))

; Verifica si la posición está dentro de los límites del laberinto.
; Parámetros:
;   pos -> Lista (fila columna) con la posición.
;   n   -> Número de filas internas.
;   m   -> Número de columnas internas.
(defun es-posicion-valida (pos n m)
  (and (>= (car pos) 0) (< (car pos) n)
       (>= (cadr pos) 0) (< (cadr pos) m)))

; Obtiene el valor de la celda en la posición (fila, col) del laberinto.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   fila      -> Índice de la fila.
;   col       -> Índice de la columna.
(defun get-celda (laberinto fila col)
  (iterar-hasta col (iterar-hasta fila laberinto)))

; Obtiene el valor de la celda en la posición (fila, col) del laberinto.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   fila      -> Índice de la fila.
;   col       -> Índice de la columna.
(defun obtener-celda (laberinto fila col)
  (let ((fila-deseada (iterar-hasta fila laberinto)))
       (iterar-hasta col fila-deseada)))

; Itera hasta el n-ésimo elemento de una lista.
; Parámetros:
;   n     -> Índice al que llegar.
;   lista -> Lista sobre la que iterar.
(defun iterar-hasta (n lista)
  (cond ((= n 0) (car lista))
        (t (iterar-hasta (- n 1) (cdr lista)))))

; Cuenta cuántas celdas adyacentes son caminos válidos.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   pos       -> Posición a comprobar.
;   n         -> Número de filas internas.
;   m         -> Número de columnas internas.
(defun contar-celdas-camino-adyacentes (laberinto pos n m)
  (let ((adyacentes (list (list (- (car pos) 1) (cadr pos))    ; Arriba
                          (list (+ (car pos) 1) (cadr pos))    ; Abajo
                          (list (car pos) (- (cadr pos) 1))    ; Izquierda
                          (list (car pos) (+ (cadr pos) 1))))) ; Derecha
       (contar-celdas-camino-adyacentes-rec laberinto adyacentes n m 0)))

; Cuenta recursivamente las celdas adyacentes que son caminos.
; Parámetros:
;   laberinto  -> Laberinto como lista de listas.
;   adyacentes -> Lista de posiciones adyacentes.
;   n          -> Número de filas internas.
;   m          -> Número de columnas internas.
;   contador   -> Acumulador de caminos encontrados.
(defun contar-celdas-camino-adyacentes-rec (laberinto adyacentes n m contador)
  (cond
   ((null adyacentes) contador)
   ((and (es-posicion-valida (car adyacentes) n m)
         (eq (obtener-celda laberinto (car (car adyacentes)) (cadr (car adyacentes))) 'cami))
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) n m (+ contador 1)))
   (t
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) n m contador))))

; Reemplaza el valor de una celda en el laberinto.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   fila      -> Índice de la fila.
;   col       -> Índice de la columna.
;   valor     -> Nuevo valor para la celda.
(defun reemplazar-posicion (laberinto fila col valor)
  (reemplazar-fila laberinto fila 
    (reemplazar-en-fila (nth fila laberinto) col valor)))

; Reemplaza una fila en el laberinto.
; Parámetros:
;   laberinto  -> Laberinto como lista de listas.
;   fila       -> Índice de la fila a reemplazar.
;   nueva-fila -> Nueva fila.
(defun reemplazar-fila (laberinto fila nueva-fila)
  (cond ((= fila 0) (cons nueva-fila (cdr laberinto)))
        (t (cons (car laberinto) 
                 (reemplazar-fila (cdr laberinto) (- fila 1) nueva-fila)))))

; Reemplaza el valor de una celda en una fila.
; Parámetros:
;   fila  -> Fila como lista.
;   col   -> Índice de la columna.
;   valor -> Nuevo valor para la celda.
(defun reemplazar-en-fila (fila col valor)
  (cond ((= col 0) (cons valor (cdr fila)))
        (t (cons (car fila) 
                 (reemplazar-en-fila (cdr fila) (- col 1) valor)))))

;------------------- ESCRIBIR LABERINTO EN ARCHIVO -------------------

; Escribe un laberinto directamente en un archivo de texto.
; Parámetros:
;   nom-fitxer -> Nombre del archivo de salida.
;   laberint   -> Laberinto como lista de listas.
(defun guardar-laberinto (nom-fitxer laberint)
  (let ((fp (open nom-fitxer :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (cond
     (fp
      (escriu-laberint-rec laberint fp)
      (close fp))
     (t (format t "Error: No se pudo abrir el archivo ~A~%" nom-fitxer)))))

; Escribe cada fila del laberinto en el archivo recursivamente.
; Parámetros:
;   laberint -> Laberinto como lista de listas.
;   fp       -> Descriptor de archivo abierto para escritura.
(defun escriu-laberint-rec (laberint fp)
  (cond
   ((null laberint) nil) ; Caso base: no hay más filas
   (t (escriu-fila (car laberint) fp)
      (write-char #\Newline fp) ; Salto de línea después de cada fila
      (escriu-laberint-rec (cdr laberint) fp))))

; Escribe cada celda de una fila en el archivo recursivamente.
; Parámetros:
;   fila -> Fila como lista.
;   fp   -> Descriptor de archivo abierto para escritura.
(defun escriu-fila (fila fp)
  (cond
   ((null fila) nil) ; Caso base: fila vacía
   (t (write-char (celda-a-caracter (car fila)) fp)
      (escriu-fila (cdr fila) fp))))

; Convierte una celda en su representación de texto.
; Parámetros:
;   celda -> Símbolo de la celda ('paret, 'cami, 'entrada, 'sortida).
(defun celda-a-caracter (celda)
  (cond
   ((eq celda 'paret) #\#)
   ((eq celda 'cami) #\.)
   ((eq celda 'entrada) #\e)
   ((eq celda 'sortida) #\s)
   (t #\ ))) ; Celda desconocida

 ; ------------------ FUNCIONES AUXILIARES -------------------

; Cuenta los elementos de una lista de forma recursiva.
; Parámetros:
;   lista -> Lista a contar.
 (defun contar-elementos (lista)
  (cond
   ((null lista) 0)
   (t (+ 1 (contar-elementos (cdr lista))))))
