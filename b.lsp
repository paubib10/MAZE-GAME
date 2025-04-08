;;; Función principal para generar el laberinto
(defun genera (nom-fitxer n m)
  "Genera un laberinto (n-2)x(m-2) usando DFS recursivo y lo guarda en archivo"
  (let* ((n-reducido (- n 2))
         (m-reducido (- m 2))
         (laberinto (crear-laberinto-vacio n-reducido m-reducido)))
    (let ((entrada (seleccionar-entrada laberinto n-reducido m-reducido)))
      (let ((laberinto-con-entrada (colocar-entrada laberinto entrada)))
        (let ((laberinto-generado (dfs-generar-laberinto laberinto-con-entrada entrada)))
          (let ((salida (seleccionar-salida laberinto-generado n-reducido m-reducido)))
            (let ((laberinto-final (colocar-salida laberinto-generado salida)))
              ;; Asegurarse de que la entrada no se sobrescriba
              (let ((laberinto-final-con-entrada (colocar-entrada laberinto-final entrada)))
                (let ((laberinto-con-bordes (agregar-bordes laberinto-final-con-entrada)))
                  (guardar-laberinto nom-fitxer laberinto-con-bordes)
                  laberinto-con-bordes)))))))))

;;; Crear laberinto vacío como lista de listas
(defun crear-laberinto-vacio (n m)
  "Crea una lista de N listas con M paredes cada una"
  (construir-filas n m))

(defun construir-filas (filas columnas)
  (cond ((= filas 0) nil)
        (t (cons (construir-fila columnas)
                 (construir-filas (- filas 1) columnas)))))

(defun construir-fila (columnas)
  (cond ((= columnas 0) nil)
        (t (cons 'paret (construir-fila (- columnas 1))))))

;;; Agregar bordes al laberinto
(defun agregar-bordes (laberinto)
  "Agrega una capa de paredes alrededor del laberinto"
  (let* ((ancho (+ 2 (length (car laberinto))))
         (fila-borde (construir-fila ancho)))
    (cons fila-borde
          (append (mapcar (lambda (fila)
                            (cons 'paret (append fila '(paret))))
                          laberinto)
                  (list fila-borde)))))

;;; Seleccionar entrada en el borde
(defun seleccionar-entrada (laberinto n m)
  "Selecciona una posición aleatoria en el borde del laberinto como entrada"
  (let ((borde (random 4)))
    (cond ((= borde 0) (list 0 (random-entre 1 (- m 2))))        ; Borde superior
          ((= borde 1) (list (random-entre 1 (- n 2)) (- m 1))) ; Borde derecho
          ((= borde 2) (list (- n 1) (random-entre 1 (- m 2)))) ; Borde inferior
          (t (list (random-entre 1 (- n 2)) 0)))))              ; Borde izquierdo

;;; Seleccionar salida en el borde
(defun seleccionar-salida (laberinto n m)
  "Selecciona una posición aleatoria en el borde del laberinto como salida"
  (let ((borde (random 4)))
    (cond ((= borde 0) (list 0 (random-entre 1 (- m 2))))        ; Borde superior
          ((= borde 1) (list (random-entre 1 (- n 2)) (- m 1))) ; Borde derecho
          ((= borde 2) (list (- n 1) (random-entre 1 (- m 2)))) ; Borde inferior
          (t (list (random-entre 1 (- n 2)) 0)))))           ; Borde izquierdo

(defun random-entre (min max)
  (+ min (random (+ (- max min) 1))))

;;; Colocar entrada en el laberinto
(defun colocar-entrada (laberinto pos)
  "Coloca la entrada en la posición especificada"
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'entrada))

;;; Colocar salida en el laberinto
(defun colocar-salida (laberinto pos)
  "Coloca la salida en la posición especificada"
  (reemplazar-posicion laberinto (car pos) (cadr pos) 'sortida))

;;; Mezclar una lista de direcciones aleatoriamente
(defun mezclar-direcciones (direcciones)
  (cond
   ((null direcciones) nil) ; Caso base: lista vacía
   (t (let ((indice (random (length direcciones))))
        (let ((elemento (nth indice direcciones))
              (resto (eliminar-indice indice direcciones)))
          (cons elemento (mezclar-direcciones resto)))))))

;;; Eliminar un elemento de la lista por índice
(defun eliminar-indice (indice lista)
  (cond
   ((null lista) nil) ; Caso base: lista vacía
   ((= indice 0) (cdr lista)) ; Si el índice es 0, elimina el primer elemento
   (t (cons (car lista) (eliminar-indice (- indice 1) (cdr lista))))))

;;; Función DFS recursiva para generar caminos
(defun dfs-generar-laberinto (laberinto pos-actual)
  (let ((laberinto-actualizado (reemplazar-posicion laberinto (car pos-actual) (cadr pos-actual) 'cami)))
    (let ((dirs (mezclar-direcciones '((0 -1) (1 0) (0 1) (-1 0))))) ; Direcciones: izquierda, abajo, derecha, arriba
      (dfs-visitar-vecinos laberinto-actualizado pos-actual dirs))))

(defun dfs-visitar-vecinos (laberinto pos-actual dirs)
  (cond
   ((null dirs) laberinto) ; Caso base: no hay más direcciones
   (t (let ((dir (car dirs)))
        (let ((nueva-pos (list (+ (car pos-actual) (car dir))
                               (+ (cadr pos-actual) (cadr dir)))))
          (if (es-pared-valida laberinto nueva-pos)
              (dfs-visitar-vecinos
               (dfs-generar-laberinto laberinto nueva-pos)
               pos-actual
               (cdr dirs))
              (dfs-visitar-vecinos laberinto pos-actual (cdr dirs))))))))

(defun es-pared-valida (laberinto pos)
  (and (es-posicion-valida pos)
       (eq (obtener-celda laberinto (car pos) (cadr pos)) 'paret)
       (= (contar-celdas-camino-adyacentes laberinto pos) 1)))

(defun es-posicion-valida (pos)
  (and (>= (car pos) 0) (< (car pos) 25)
       (>= (cadr pos) 0) (< (cadr pos) 25)))

(defun get-celda (laberinto fila col)
  "Obtiene el valor de la celda en la posición (fila, col) del laberinto."
  (nth col (nth fila laberinto)))

;;; Obtener celda del laberinto
(defun obtener-celda (laberinto fila col)
    (let ((fila-deseada (iterar-hasta fila laberinto)))
        (iterar-hasta col fila-deseada)))

(defun iterar-hasta (n lista)
    (cond ((= n 0) (car lista))
                (t (iterar-hasta (- n 1) (cdr lista)))))

(defun contar-celdas-camino-adyacentes (laberinto pos)
  (let ((adyacentes (list (list (- (car pos) 1) (cadr pos)) ; Arriba
                          (list (+ (car pos) 1) (cadr pos)) ; Abajo
                          (list (car pos) (- (cadr pos) 1)) ; Izquierda
                          (list (car pos) (+ (cadr pos) 1))))) ; Derecha
    (contar-celdas-camino-adyacentes-rec laberinto adyacentes 0)))

(defun contar-celdas-camino-adyacentes-rec (laberinto adyacentes contador)
  (cond
   ((null adyacentes) contador)
   ((and (es-posicion-valida (car adyacentes))
         (eq (obtener-celda laberinto (car (car adyacentes)) (cadr (car adyacentes))) 'cami))
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) (+ contador 1)))
   (t
    (contar-celdas-camino-adyacentes-rec laberinto (cdr adyacentes) contador))))

;;; Reemplazar celda en el laberinto
(defun reemplazar-posicion (laberinto fila col valor)
  (reemplazar-fila laberinto fila 
    (reemplazar-en-fila (nth fila laberinto) col valor)))

(defun reemplazar-fila (laberinto fila nueva-fila)
  (cond ((= fila 0) (cons nueva-fila (cdr laberinto)))
        (t (cons (car laberinto) 
                 (reemplazar-fila (cdr laberinto) (- fila 1) nueva-fila)))))

(defun reemplazar-en-fila (fila col valor)
  (cond ((= col 0) (cons valor (cdr fila)))
        (t (cons (car fila) 
                 (reemplazar-en-fila (cdr fila) (- col 1) valor)))))

;;; Guardar laberinto en archivo sin usar with-open-file
(defun guardar-laberinto (nombre-archivo laberinto)
  (let ((archivo (open nombre-archivo :direction :output :if-exists :supersede)))
    (unwind-protect
        (progn
          (princ (laberinto-a-texto laberinto) archivo)) ; Escribir el contenido en el archivo
      (close archivo)))) ; Asegurar que el archivo se cierre

;;; Convertir el laberinto a una cadena de texto
(defun laberinto-a-texto (laberinto)
  (cond ((null laberinto) "") ; Caso base: laberinto vacío
        (t (concatenate 'string
                        (fila-a-texto (car laberinto)) "\n"
                        (laberinto-a-texto (cdr laberinto))))))

;;; Convertir una fila del laberinto a una cadena de texto
(defun fila-a-texto (fila)
  (cond ((null fila) "") ; Caso base: fila vacía
        (t (concatenate 'string
                        (celda-a-caracter (car fila))
                        (fila-a-texto (cdr fila))))))

;;; Convertir una celda a su representación de texto
(defun celda-a-caracter (celda)
  (cond ((eq celda 'paret) "#")
        ((eq celda 'cami) ".")
        ((eq celda 'entrada) "e")
        ((eq celda 'sortida) "s")
        (t " "))) ; Celda desconocida