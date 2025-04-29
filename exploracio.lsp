; ------------------- BLOQUE DE COMENTARIO -------------------
; Autores: Pau Toni Bibiloni Martínez & Hugo Guerreiro Paredes
; Fecha: 29 de abril de 2025
; Asignatura: Lenguajes de Programación
; Grupo: 3
; Profesores: Antoni Oliver Tomàs
; Convocatoria: Ordinaria
; ------------------------------------------------------------

; ---------------------- INSTRUCCIONES -----------------------
; Este archivo contiene las funciones necesarias para explorar laberintos.
; 
; Pasos para usar las funciones:
; 1. Cargar el archivo:
;    - Ejecuta el comando: (load "exploracio.lsp")
;    - Esto cargará todas las funciones necesarias para explorar laberintos.
;
; 2. Explorar un laberinto:
;    - Función: (explora nom-fitxer)
;    - Parámetros:
;        * nom-fitxer: Nombre del archivo que contiene el laberinto.
;    - Ejemplo: (explora "laberint.txt")
;      Esto permitirá al usuario explorar el laberinto guardado en "laberint.txt".
;      El programa pedirá al usuario que introduzca su nombre antes de comenzar.
;
; ------------------ ASPECTOS OPCIONALES ---------------------
; 1. Guardado de clasificación:
;    - Implementado en la función (guardar-nom-i-passes).
;    - Guarda el nombre del jugador, el número de pasos y el nombre del archivo en un archivo de clasificación.
;
; 2. Visualización de clasificación:
;    - Implementado en la función (mostrar-classificacio).
;    - Muestra las 10 mejores exploraciones de un laberinto específico.
;
; 3. Validación de movimientos:
;    - Implementado en la función (moviment-valid?).
;    - Comprueba si un movimiento es válido antes de actualizar la posición del jugador.
;
; ------------------ DISEÑO FUNCIONAL ------------------------
; El programa está diseñado para permitir la exploración de laberintos de manera interactiva:
; 1. Carga del laberinto:
;    - Se utiliza la función (cargar-laberinto) para leer un archivo de texto y convertirlo en una estructura de datos.
;
; 2. Exploración del laberinto:
;    - La función principal (explora) inicia la exploración del laberinto.
;    - La función recursiva (explora-rec) gestiona los movimientos del jugador y las condiciones de salida.
;
; 3. Movimientos del jugador:
;    - Se utilizan las funciones (llegir-moviment), (moviment-valid?) y (moure-jugador) para gestionar los movimientos.
;
; 4. Clasificación:
;    - Se guarda y muestra la clasificación de los jugadores que han explorado el laberinto.
;
; ------------------------------------------------------------

(load "dibuixar.lsp")

; ---------------------------- EXPLORA -------------------------------
(defun explora (nom-fitxer)
  "Inicia l'exploració del laberint carregat des de nom-fitxer."
  (let ((nom-jugador (solicitar-nombre-jugador))) ; Llama al submétodo para obtener el nombre
    (let* ((laberint (reverse (cargar-laberinto nom-fitxer)))
           (posicio-jugador (buscar-posicion laberint 'entrada))
           (posicio-meta (buscar-posicion laberint 'sortida))
           (ancho-ventana 500) ; Define el ancho de la ventana gráfica
           (alto-ventana 375)) ; Define el alto de la ventana gráfica
      (cls)
      (dibujar-laberinto laberint posicio-jugador posicio-meta ancho-ventana alto-ventana)
      (explora-rec laberint posicio-jugador posicio-meta 0 nom-fitxer nom-jugador ancho-ventana alto-ventana))))

(defun explora-rec (laberint posicio-jugador posicio-meta passes nom-fitxer nom-jugador ancho-ventana alto-ventana)
  "Funció recursiva per explorar el laberint."
  (let ((moviment (llegir-moviment)))
    (cond
     ((eq moviment 'sortir)
      (color 0 0 0)
      (cls)
      (format t "Has sortit de l'exploracio.~%"))
     ((es-meta? posicio-jugador posicio-meta)
      (color 0 0 0)
      (cls)
      (format t "Enhorabona, ~A! Has arribat a la meta en ~A passes.~%" nom-jugador passes)
      (guardar-nom-i-passes passes nom-fitxer nom-jugador)
      (mostrar-classificacio nom-fitxer))
     ((moviment-valid? laberint posicio-jugador moviment)
      (let ((nova-posicio (moure-jugador posicio-jugador moviment)))
        (actualizar-celda laberint posicio-jugador ancho-ventana alto-ventana)
        (dibujar-celda-jugador nova-posicio laberint ancho-ventana alto-ventana)
        (explora-rec laberint nova-posicio posicio-meta (+ passes 1) nom-fitxer nom-jugador ancho-ventana alto-ventana)))
     (t
      (explora-rec laberint posicio-jugador posicio-meta passes nom-fitxer nom-jugador ancho-ventana alto-ventana)))))

(defun solicitar-nombre-jugador ()
  "Solicita el nombre del jugador y valida que no esté vacío."
  (let ((nom-jugador (read-line))) ; Lee el nombre del jugador
    (cond
     ((and nom-jugador (not (equal nom-jugador ""))) nom-jugador) ; Si el nombre es válido, lo devuelve
     (t
      (format t "Introdueix el teu nom: ") ; Vuelve a mostrar el mensaje
      (solicitar-nombre-jugador))))) ; Llama recursivamente hasta obtener un nombre válido
        
; -------------------------- CARGAR_LABERINTO ---------------------------
(defun cargar-laberinto (nom-fitxer)
  "Carga el laberinto desde un archivo de texto y lo convierte en una lista de listas."
  (let ((fp (open nom-fitxer :direction :input)))
    (let ((laberinto (cargar-lineas-rec fp)))
      (close fp)
      laberinto)))

(defun cargar-lineas-rec (fp)
  "Lee las líneas del archivo y las convierte en listas de símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) nil) ; Si es el final del archivo, termina
     ((eq caracter #\Newline) ; Si es un salto de línea, procesa la siguiente línea
      (cons nil (cargar-lineas-rec fp)))
     (t ; Acumula caracteres en la línea actual
      (let ((linea (cargar-linea-rec fp (list (caracter-a-simbolo caracter)))))
        (cons linea (cargar-lineas-rec fp)))))))

(defun cargar-linea-rec (fp acumulador)
  "Lee caracteres de una línea y los convierte en símbolos recursivamente."
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) (reverse acumulador)) ; Fin del archivo
     ((eq caracter #\Newline) (reverse acumulador)) ; Fin de la línea
     (t (cargar-linea-rec fp (cons (caracter-a-simbolo caracter) acumulador))))))

; -------------------------- BUSCAR_POSICION -----------------------------
(defun buscar-posicion (laberinto simbolo)
  "Busca la posición de un símbolo en el laberinto."
  (buscar-en-laberinto laberinto simbolo 0))

(defun buscar-en-laberinto (laberinto simbolo fila-index)
  "Busca el símbolo en el laberinto fila por fila."
  (cond
   ((null laberinto) nil)
   (t (let ((pos (buscar-en-fila (car laberinto) simbolo fila-index 0)))
        (cond
         (pos pos)
         (t (buscar-en-laberinto (cdr laberinto) simbolo (+ fila-index 1))))))))

(defun buscar-en-fila (fila simbolo fila-index col-index)
  "Busca el símbolo en una fila específica."
  (cond
   ((null fila) nil)
   ((eq (car fila) simbolo) (list fila-index col-index))
   (t (buscar-en-fila (cdr fila) simbolo fila-index (+ col-index 1)))))

; -------------------------- GUARDAR CLASSIFICACIO --------------------------
(defun guardar-nom-i-passes (passes nom-fitxer nom-jugador)
  "Guarda el número de pasos, el nombre del jugador y el nombre del fichero en el archivo de clasificación."
  (let ((fitxer (format nil "~A_classificacio.txt" nom-fitxer)))
    (let ((fp (open fitxer :direction :output :if-exists :append :if-does-not-exist :create)))
      (cond
       (fp
        (format fp "~S~%" (list passes nom-jugador nom-fitxer)) ; Guarda como lista
        (close fp)
        (format t "S'ha guardat el teu nom, passes i mapa al fitxer ~A.~%" fitxer))
       (t (format t "Error: No s'ha pogut obrir el fitxer ~A.~%" fitxer))))))

; -------------------------- MOSTRAR CLASSIFICACIO --------------------------
(defun mostrar-classificacio (nom-fitxer)
  "Mostra les 10 millors exploracions del laberint especificat."
  (let ((fitxer (format nil "~A_classificacio.txt" nom-fitxer)))
    (let ((classificacio (carregar-classificacio fitxer)))
      (format t "Classificacio de les 10 millors exploracions de ~A:~%" nom-fitxer)
      (mostrar-entrades classificacio 10))))

(defun mostrar-entrades (classificacio n)
  "Mostra les primeres n entrades de la classificació."
  (cond
   ((or (null classificacio) (<= n 0)) nil)
   (t (let ((entrada (car classificacio)))
        (format t "~A passes: ~A (Mapa: ~A)~%" (car entrada) (cadr entrada) (caddr entrada))
        (mostrar-entrades (cdr classificacio) (- n 1))))))

(defun carregar-classificacio (fitxer)
  "Carrega la classificació des del fitxer i retorna una llista ordenada per passes."
  (let ((fp (open fitxer :direction :input)))
    (cond
     (fp
      (let ((classificacio (carregar-linies-rec fp)))
        (close fp)
        (ordenar-classificacio classificacio)))
     (t (error "No s'ha pogut obrir el fitxer ~A." fitxer)))))

(defun carregar-linies-rec (fp)
  "Llegeix les línies del fitxer recursivament i les converteix en una llista."
  (let ((linia (read fp nil)))
    (cond
     (linia (cons linia (carregar-linies-rec fp)))
     (t nil))))

(defun ordenar-classificacio (classificacio)
  "Ordena la llista de classificació per passes utilitzant recursió."
  (cond
   ((null classificacio) nil)
   (t (insertar-ordenado (car classificacio) (ordenar-classificacio (cdr classificacio))))))

(defun insertar-ordenado (entrada lista)
  "Insereix una entrada en una llista ordenada per passes."
  (cond
   ((null lista) (list entrada))
   ((< (car entrada) (car (car lista))) (cons entrada lista))
   (t (cons (car lista) (insertar-ordenado entrada (cdr lista))))))

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
  "Comprova si el moviment és vàlid."
  (let* ((x (first posicio))
         (y (second posicio))
         (nova-posicio
          (cond
           ((eq moviment 'amunt) (list (+ x 1) y))    ; Cambiado: ahora suma para 'amunt'
           ((eq moviment 'avall) (list (- x 1) y))    ; Cambiado: ahora resta para 'avall'
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
  "Actualitza la posició del jugador segons el moviment."
  (let ((x (first posicio))
        (y (second posicio)))
    (cond
     ((eq moviment 'amunt) (list (+ x 1) y))    ; Cambiado: ahora suma para 'amunt'
     ((eq moviment 'avall) (list (- x 1) y))    ; Cambiado: ahora resta para 'avall'
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