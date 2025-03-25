(defun genera-laberint (n m)
  ; "Genera un laberinto aleatorio de tamaño N × M usando DFS."
  (let ((laberint (make-laberint n m 'paret)))
    (let* ((fila-inicio (+ 1 (random (- n 2))))
           (col-inicio (+ 1 (random (- m 2)))))
      (set-celda laberint fila-inicio col-inicio 'entrada)
      (dfs-generar laberint fila-inicio col-inicio)
      (establecer-entrada laberint)
      (establecer-salida laberint)
      laberint)))

(defun make-laberint (n m valor)
  ; "Crea un laberinto vacío representado como una lista de listas."
  (cond
   ((zerop n) nil)
   (t (cons (make-fila m valor) (make-laberint (- n 1) m valor)))))

(defun make-fila (m valor)
  ; "Crea una fila del laberinto con un valor inicial."
  (cond
   ((zerop m) nil)
   (t (cons valor (make-fila (- m 1) valor)))))

(defun get-celda (laberint fila col)
  ; "Obtiene el valor de una celda en el laberinto."
  (nth col (nth fila laberint)))

(defun set-celda (laberint fila col valor)
  ; "Establece el valor de una celda en el laberinto."
  (let ((fila-actual (nth fila laberint)))
    (setf (nth col fila-actual) valor)))

(defun dfs-generar (laberint fila col)
  ; "Genera caminos en el laberinto usando DFS recursivo."
  (set-celda laberint fila col 'cami)
  (dfs-rec laberint fila col (shuffle '((0 1) (1 0) (0 -1) (-1 0)))))

(defun dfs-rec (laberint fila col dirs)
  ; "Recorre las direcciones recursivamente para generar caminos."
  (cond
   ((null dirs) nil)
   (t
    (let* ((dir (car dirs))
           (dx (car dir)) (dy (cadr dir))
           (new-x (+ fila (* 2 dx))) (new-y (+ col (* 2 dy))))
      (cond
       ((y-se-puede-expander laberint new-x new-y)
        (set-celda laberint (+ fila dx) (+ col dy) 'cami)
        (dfs-generar laberint new-x new-y)))
      (dfs-rec laberint fila col (cdr dirs))))))

(defun y-se-puede-expander (laberint x y)
  ; "Verifica si la casilla (x, y) es expandible en DFS."
  (and (>= x 1) (< x (- (length laberint) 1))
       (>= y 1) (< y (- (length (car laberint)) 1))
       (eql (get-celda laberint x y) 'paret)))

(defun establecer-entrada (laberint)
  ; "Coloca la entrada en una posición aleatoria del laberinto."
  (establecer-entrada-rec laberint))

(defun establecer-entrada-rec (laberint)
  ; "Busca recursivamente una posición válida para la entrada."
  (let ((x (+ 1 (random (- (length laberint) 2))))
        (y (+ 1 (random (- (length (car laberint)) 2)))))
    (cond
     ((eql (get-celda laberint x y) 'cami)
      (set-celda laberint x y 'entrada))
     (t (establecer-entrada-rec laberint)))))

(defun establecer-salida (laberint)
  ; "Coloca la salida en una posición aleatoria del laberinto."
  (establecer-salida-rec laberint))

(defun establecer-salida-rec (laberint)
  ; "Busca recursivamente una posición válida para la salida."
  (let ((x (+ 1 (random (- (length laberint) 2))))
        (y (+ 1 (random (- (length (car laberint)) 2)))))
    (cond
     ((eql (get-celda laberint x y) 'cami)
      (set-celda laberint x y 'sortida))
     (t (establecer-salida-rec laberint)))))

(defun shuffle (list)
  ; "Mezcla aleatoriamente una lista (útil para los movimientos en DFS)."
  (cond
   ((null list) nil)
   (t (let ((elem (nth (random (length list)) list)))
        (cons elem (shuffle (remove elem list)))))))

(defun laberint-a-texto (laberint)
  ; "Convierte el laberinto a una lista de caracteres para escribirlo en un archivo."
  (let ((caracteres '((paret . #\#) (cami . #\.) (entrada . #\e) (sortida . #\s))))
    (laberint-a-texto-rec laberint caracteres)))

(defun laberint-a-texto-rec (laberint caracteres)
  ; "Convierte recursivamente el laberinto a texto."
  (cond
   ((null laberint) nil)
   (t (append (fila-a-texto (car laberint) caracteres)
              (list #\Newline)
              (laberint-a-texto-rec (cdr laberint) caracteres)))))

(defun fila-a-texto (fila caracteres)
  ; "Convierte una fila del laberinto a texto."
  (cond
   ((null fila) nil)
   (t (cons (cdr (assoc (car fila) caracteres))
            (fila-a-texto (cdr fila) caracteres)))))

(defun escriu-laberint (nom-fitxer laberint)
  ; "Guarda un laberinto en un archivo de texto."
  (let ((fp (open nom-fitxer :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (escriu-caracteres (laberint-a-texto laberint) fp)
    (close fp)))

(defun escriu-caracteres (caracteres fp)
  ; "Escribe recursivamente los caracteres en un archivo."
  (cond
   ((null caracteres) nil)
   (t (write-char (car caracteres) fp)
      (escriu-caracteres (cdr caracteres) fp))))

(defun genera (nom-fitxer n m)
  ; "Genera un laberinto aleatorio de tamaño NxM y lo escribe en un archivo."
  (escriu-laberint nom-fitxer (genera-laberint n m)))

; (genera "laberinto.txt" 10 10)