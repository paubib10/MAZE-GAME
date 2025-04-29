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

(defun genera (nom-fitxer n m)
  "Genera un laberinto (n-2)x(m-2) usando DFS recursivo y lo guarda en archivo"
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

(defun crear-laberinto-vacio (n m)
  "Crea una lista de N listas con M paredes cada una"
  (construir-filas n m))

(defun construir-filas (filas columnas)
  "Construye una lista de filas con columnas de paredes"
  (cond ((= filas 0) nil)
        (t (cons (construir-fila columnas)
                 (construir-filas (- filas 1) columnas)))))

(defun construir-fila (columnas)
  "Construye una fila con columnas de paredes"
  (cond ((= columnas 0) nil)
        (t (cons 'paret (construir-fila (- columnas 1))))))

(defun agregar-bordes (laberinto)
  "Agrega una capa de paredes alrededor del laberinto"
  (let* ((ancho (+ 2 (contar-elementos (car laberinto))))
         (fila-borde (construir-fila ancho)))
    (cons fila-borde
          (append (mapcar (lambda (fila)
                            (cons 'paret (append fila '(paret))))
                          laberinto)
                  (list fila-borde)))))

(defun seleccionar-entrada (laberinto n m)
  "Selecciona una posición aleatoria dentro del laberinto como entrada."
  (list (random n) (random m)))

(defun seleccionar-salida (laberinto n m)
  "Selecciona una posición aleatoria dentro del laberinto como salida."
  (list (random n) (random m)))

(defun colocar-entrada (laberinto pos)
  "Coloca la entrada en la posición especificada"
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'entrada))

(defun colocar-salida (laberinto pos)
  "Coloca la salida en la posición especificada"
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'sortida))

(defun mezclar-direcciones (direcciones)
  "Mezcla aleatoriamente una lista de direcciones."
  (cond
   ((null direcciones) nil) ; Caso base: lista vacía
   (t (let ((indice (random (contar-elementos direcciones))))
        (let ((elemento (obtener-elemento direcciones indice))
              (resto (eliminar-indice indice direcciones)))
          (cons elemento (mezclar-direcciones resto)))))))

(defun eliminar-indice (indice lista)
  "Elimina un elemento de la lista por indice."
  (cond
   ((null lista) nil) ; Caso base: lista vacía
   ((= indice 0) (cdr lista)) ; Si el índice es 0, elimina el primer elemento
   (t (cons (car lista) (eliminar-indice (- indice 1) (cdr lista))))))

(defun obtener-elemento (lista indice)
  "Obtiene el elemento en la posición `indice` de una lista de forma recursiva."
  (cond
   ((null lista) nil) ; Caso base: lista vacía
   ((= indice 0) (car lista)) ; Caso base: índice 0, devuelve el primer elemento
   (t (obtener-elemento (cdr lista) (- indice 1))))) ; Llamada recursiva

(defun dfs-generar-laberinto (laberinto pos-actual n m)
  "Genera caminos en el laberinto usando DFS."
  (let ((laberinto-actualizado (reemplazar-posicion laberinto (car pos-actual) (cadr pos-actual) 'cami)))
    (let ((dirs (mezclar-direcciones '((0 -1) (1 0) (0 1) (-1 0))))) ; Direcciones: izquierda, abajo, derecha, arriba
      (dfs-visitar-vecinos laberinto-actualizado pos-actual dirs n m))))

(defun dfs-visitar-vecinos (laberinto pos-actual dirs n m)
  "Visita las celdas vecinas recursivamente usando DFS."
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

(defun es-pared-valida (laberinto pos n m)
  "Verifica si una celda es una pared válida para ser convertida en camino."
  (and (es-posicion-valida pos n m)
       (eq (obtener-celda laberinto (car pos) (cadr pos)) 'paret)
       (= (contar-celdas-camino-adyacentes laberinto pos n m) 1)))

(defun es-posicion-valida (pos n m)
  "Verifica si la posición está dentro de los límites del laberinto."
  (and (>= (car pos) 0) (< (car pos) n)
       (>= (cadr pos) 0) (< (cadr pos) m)))

(defun get-celda (laberinto fila col)
  "Obtiene el valor de la celda en la posición (fila, col) del laberinto."
  (iterar-hasta col (iterar-hasta fila laberinto)))

(defun obtener-celda (laberinto fila col)
  "Obtiene el valor de la celda en la posición (fila, col) del laberinto."
  (let ((fila-deseada (iterar-hasta fila laberinto)))
       (iterar-hasta col fila-deseada)))

(defun iterar-hasta (n lista)
  "Itera hasta el n-ésimo elemento de una lista."
  (cond ((= n 0) (car lista))
        (t (iterar-hasta (- n 1) (cdr lista)))))

(defun contar-celdas-camino-adyacentes (laberinto pos n m)
  "Cuenta cuántas celdas adyacentes son caminos válidos."
  (let ((adyacentes (list (list (- (car pos) 1) (cadr pos))    ; Arriba
                          (list (+ (car pos) 1) (cadr pos))    ; Abajo
                          (list (car pos) (- (cadr pos) 1))    ; Izquierda
                          (list (car pos) (+ (cadr pos) 1))))) ; Derecha
       (contar-celdas-camino-adyacentes-rec laberinto adyacentes n m 0)))

(defun contar-celdas-camino-adyacentes-rec (laberinto adyacentes n m contador)
  "Cuenta recursivamente las celdas adyacentes que son caminos."
  (cond
   ((null adyacentes) contador)
   ((and (es-posicion-valida (car adyacentes) n m)
         (eq (obtener-celda laberinto (car (car adyacentes)) (cadr (car adyacentes))) 'cami))
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) n m (+ contador 1)))
   (t
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) n m contador))))

(defun reemplazar-posicion (laberinto fila col valor)
  "Reemplaza el valor de una celda en el laberinto."
  (reemplazar-fila laberinto fila 
    (reemplazar-en-fila (nth fila laberinto) col valor)))

(defun reemplazar-fila (laberinto fila nueva-fila)
  "Reemplaza una fila en el laberinto."
  (cond ((= fila 0) (cons nueva-fila (cdr laberinto)))
        (t (cons (car laberinto) 
                 (reemplazar-fila (cdr laberinto) (- fila 1) nueva-fila)))))

(defun reemplazar-en-fila (fila col valor)
  "Reemplaza el valor de una celda en una fila."
  (cond ((= col 0) (cons valor (cdr fila)))
        (t (cons (car fila) 
                 (reemplazar-en-fila (cdr fila) (- col 1) valor)))))

;------------------- ESCRIBIR LABERINTO EN ARCHIVO -------------------
(defun guardar-laberinto (nom-fitxer laberint)
  "Escribe un laberinto directamente en un archivo de texto."
  (let ((fp (open nom-fitxer :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (cond
     (fp
      (escriu-laberint-rec laberint fp)
      (close fp))
     (t (format t "Error: No se pudo abrir el archivo ~A~%" nom-fitxer)))))

(defun escriu-laberint-rec (laberint fp)
  "Escribe cada fila del laberinto en el archivo recursivamente."
  (cond
   ((null laberint) nil) ; Caso base: no hay más filas
   (t (escriu-fila (car laberint) fp)
      (write-char #\Newline fp) ; Salto de línea después de cada fila
      (escriu-laberint-rec (cdr laberint) fp))))

(defun escriu-laberint-rec (laberint fp)
  "Escribe cada fila del laberinto en el archivo recursivamente."
  (cond
   ((null laberint) nil) ; Caso base: no hay más filas
   (t (escriu-fila (car laberint) fp)
      (write-char #\Newline fp) ; Salto de línea después de cada fila
      (escriu-laberint-rec (cdr laberint) fp))))

(defun escriu-fila (fila fp)
  "Escribe cada celda de una fila en el archivo recursivamente."
  (cond
   ((null fila) nil) ; Caso base: fila vacía
   (t (write-char (celda-a-caracter (car fila)) fp)
      (escriu-fila (cdr fila) fp))))

(defun celda-a-caracter (celda)
  "Convierte una celda en su representación de texto."
  (cond
   ((eq celda 'paret) #\#)
   ((eq celda 'cami) #\.)
   ((eq celda 'entrada) #\e)
   ((eq celda 'sortida) #\s)
   (t #\ ))) ; Celda desconocida

 ; ------------------ FUNCIONES AUXILIARES -------------------
 (defun contar-elementos (lista)
  "Cuenta los elementos de una lista de forma recursiva."
  (cond
   ((null lista) 0)
   (t (+ 1 (contar-elementos (cdr lista))))))
