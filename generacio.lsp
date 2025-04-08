;;; Función principal para generar el laberinto
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
              ;; Asegurarse de que la entrada no se sobrescriba
              (let ((laberinto-final-con-entrada (colocar-entrada laberinto-final entrada)))
                (let ((laberinto-con-bordes (agregar-bordes laberinto-final-con-entrada)))
                  (guardar-laberinto nom-fitxer laberinto-con-bordes)
                  laberinto-con-bordes)))))))))

;;; Crear laberinto vacío como lista de listasx 
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

(defun seleccionar-entrada (laberinto n m)
  "Selecciona una posición aleatoria dentro del laberinto como entrada."
  (list (random n) (random m)))

(defun seleccionar-salida (laberinto n m)
  "Selecciona una posición aleatoria dentro del laberinto como salida."
  (list (random n) (random m)))

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

(defun iterar-hasta (n lista)
  (cond ((= n 0) (car lista))
        (t (iterar-hasta (- n 1) (cdr lista)))))

;;; Obtener celda del laberinto
(defun obtener-celda (laberinto fila col)
    (let ((fila-deseada (iterar-hasta fila laberinto)))
        (iterar-hasta col fila-deseada)))

(defun iterar-hasta (n lista)
    (cond ((= n 0) (car lista))
                (t (iterar-hasta (- n 1) (cdr lista)))))

(defun contar-celdas-camino-adyacentes (laberinto pos n m)
  "Cuenta cuántas celdas adyacentes son caminos válidos."
  (let ((adyacentes (list (list (- (car pos) 1) (cadr pos)) ; Arriba
                          (list (+ (car pos) 1) (cadr pos)) ; Abajo
                          (list (car pos) (- (cadr pos) 1)) ; Izquierda
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