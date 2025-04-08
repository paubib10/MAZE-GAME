(load "dibuixar.lsp")

; ---------------------------- EXPLORA -------------------------------
(defun explora (nom-fitxer &optional (viewport nil) (passes 0))
  "Inicia l'exploració del laberint carregat des de nom-fitxer."
  (let* ((laberint (cargar-laberinto nom-fitxer))
         (posicio-jugador (buscar-posicion laberint 'entrada))
         (posicio-meta (buscar-posicion laberint 'sortida)))
    (cond
     ((or (null posicio-jugador) (null posicio-meta))
      (format t "El laberint no te entrada o sortida valides.~%"))
     (t
      (cls)
      (explora-rec laberint posicio-jugador posicio-meta passes nom-fitxer)))))

(defun explora-rec (laberint posicio-jugador posicio-meta passes nom-fitxer)
  "Funció recursiva per explorar el laberint."
  (cls)
  (dibujar-laberinto laberint posicio-jugador posicio-meta)
  (let ((moviment (llegir-moviment)))
    (cond
     ((eq moviment 'sortir)
      (format t "Has sortit de l'exploracio.~%"))
     ((es-meta? posicio-jugador posicio-meta)
      (format t "Enhorabona! Has arribat a la meta en ~A passes.~%" passes)
      (guardar-nom-i-passes passes nom-fitxer))
     ((moviment-valid? laberint posicio-jugador moviment)
      (explora-rec laberint
                   (moure-jugador posicio-jugador moviment)
                   posicio-meta
                   (+ passes 1)
                   nom-fitxer))
     (t
      (explora-rec laberint posicio-jugador posicio-meta passes nom-fitxer)))))

; -------------------------- CARGAR_LABERINTO ---------------------------
(defun cargar-laberinto (nom-fitxer)
  ; "Carga el laberinto desde un archivo de texto y lo convierte en una lista de listas."
  (let ((fp (open nom-fitxer :direction :input)))
    (let ((laberinto (cargar-lineas-rec fp)))
      (close fp)
      laberinto)))

(defun cargar-lineas-rec (fp)
  ; "Lee las líneas del archivo y las convierte en listas de símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) nil) ; Si es el final del archivo, termina
     ((eq caracter #\Newline) ; Si es un salto de línea, procesa la siguiente línea
      (cons nil (cargar-lineas-rec fp)))
     (t ; Acumula caracteres en la línea actual
      (let ((linea (cargar-linea-rec fp (list (caracter-a-simbolo caracter)))))
        (cons linea (cargar-lineas-rec fp)))))))

(defun cargar-linea-rec (fp acumulador)
  ; "Lee caracteres de una línea y los convierte en símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) (reverse acumulador)) ; Fin del archivo
     ((eq caracter #\Newline) (reverse acumulador)) ; Fin de la línea
     (t (cargar-linea-rec fp (cons (caracter-a-simbolo caracter) acumulador))))))

; -------------------------- BUSCAR_POSICION -----------------------------
(defun buscar-posicion (laberinto simbolo)
  ; "Busca la posición de un símbolo en el laberinto."
  (buscar-en-laberinto laberinto simbolo 0))

(defun buscar-en-laberinto (laberinto simbolo fila-index)
  ; "Busca el símbolo en el laberinto fila por fila."
  (cond
   ((null laberinto) nil)
   (t (let ((pos (buscar-en-fila (car laberinto) simbolo fila-index 0)))
        (cond
         (pos pos)
         (t (buscar-en-laberinto (cdr laberinto) simbolo (+ fila-index 1))))))))

(defun buscar-en-fila (fila simbolo fila-index col-index)
  ; "Busca el símbolo en una fila específica."
  (cond
   ((null fila) nil)
   ((eq (car fila) simbolo) (list fila-index col-index))
   (t (buscar-en-fila (cdr fila) simbolo fila-index (+ col-index 1)))))

; -------------------------- GUARDAR NOM I PASSES ------------------------
(defun guardar-nom-i-passes (passes nom-fitxer)
  "Demana el nom del jugador i guarda el nom, passes i nom del fitxer al fitxer de classificació."
  (let ((fitxer "clasificacion.txt"))
    (let ((nom (read-line)))
      (let ((fp (open fitxer :direction :output :if-exists :append :if-does-not-exist :create)))
        (cond
         (fp
          (format fp "~A: ~A passes (Mapa: ~A)~%" nom passes nom-fitxer)
          (close fp)
          (format t "S'ha guardat el teu nom, passes i mapa al fitxer ~A.~%" fitxer))
         (t (format t "Error: No s'ha pogut obrir el fitxer ~A.~%" fitxer)))))))

;-------------------------- MOVIMENT DEL JUGADOR ------------------------
(defun llegir-moviment ()
  ; "Llegeix el moviment del jugador des del teclat amb get-key."
  (let ((tecla (get-key)))
    (cond
     ((or (= tecla 65) (= tecla 97) (= tecla 331)) 'esquerra) ; A o ←
     ((or (= tecla 68) (= tecla 100) (= tecla 333)) 'dreta)   ; D o →
     ((or (= tecla 87) (= tecla 119) (= tecla 328)) 'amunt)   ; W o ↑
     ((or (= tecla 83) (= tecla 115) (= tecla 336)) 'avall)   ; S o ↓
     ((= tecla 27) 'sortir)                                  ; ESC
     (t nil))))                                              ; Qualsevol altra tecla

(defun moviment-valid? (laberint posicio moviment)
  ; "Comprova si el moviment és vàlid."
  (let* ((x (first posicio))
         (y (second posicio))
         (nova-posicio
          (cond
           ((eq moviment 'amunt) (list (- x 1) y))
           ((eq moviment 'avall) (list (+ x 1) y))
           ((eq moviment 'esquerra) (list x (- y 1)))
           ((eq moviment 'dreta) (list x (+ y 1)))
           (t posicio))))
    (and (>= (first nova-posicio) 0) ; Comprova que la fila està dins dels límits
         (< (first nova-posicio) (contar-elementos laberint)) ; Fila no supera el límit
         (>= (second nova-posicio) 0) ; Comprova que la columna està dins dels límits
         (< (second nova-posicio) (contar-elementos (car laberint))) ; Columna no supera el límit
         (or (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'cami)
             (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'entrada) ; Permet moure's a 'entrada
             (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'sortida))))) ; Permet moure's a 'sortida

(defun moure-jugador (posicio moviment)
  ; "Actualitza la posició del jugador segons el moviment."
  (let ((x (first posicio))
        (y (second posicio)))
    (cond
     ((eq moviment 'amunt) (list (- x 1) y))    ; Mou cap amunt (fila -1)
     ((eq moviment 'avall) (list (+ x 1) y))    ; Mou cap avall (fila +1)
     ((eq moviment 'esquerra) (list x (- y 1))) ; Mou cap a l'esquerra (columna -1)
     ((eq moviment 'dreta) (list x (+ y 1)))    ; Mou cap a la dreta (columna +1)
     (t posicio))))                             ; Si no és vàlid, no es mou

(defun es-meta? (posicio-jugador posicio-meta)
  ; "Comprova si el jugador ha arribat a la meta."
  (equal posicio-jugador posicio-meta))

; -------------------------- FUNCIONES AUXILIARES ------------------------
(defun caracter-a-simbolo (caracter)
  ; "Convierte un carácter del archivo a su representación simbólica."
  (cond
   ((eq caracter #\#) 'paret)
   ((eq caracter #\.) 'cami)
   ((eq caracter #\e) 'entrada)
   ((eq caracter #\s) 'sortida)
   (t 'desconocido)))