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

; Inicia la exploración del laberinto cargado desde el archivo indicado.
; Parámetros:
;   nom-fitxer -> Nombre del archivo que contiene el laberinto.
(defun explora (nom-fitxer)
  (let ((nom-jugador (solicitar-nombre-jugador))) ; Llama al submétodo para obtener el nombre
    (let* ((laberint (reverse (cargar-laberinto nom-fitxer)))
           (posicio-jugador (buscar-posicion laberint 'entrada))
           (posicio-meta (buscar-posicion laberint 'sortida))
           (ancho-ventana 500) ; Define el ancho de la ventana gráfica
           (alto-ventana 375)) ; Define el alto de la ventana gráfica
      (cls)
      (dibujar-laberinto laberint posicio-jugador posicio-meta ancho-ventana alto-ventana)
      (explora-rec laberint posicio-jugador posicio-meta 0 nom-fitxer nom-jugador ancho-ventana alto-ventana))))

; Función recursiva que gestiona los movimientos del jugador y las condiciones de salida.
; Parámetros:
;   laberint        -> Laberinto como lista de listas.
;   posicio-jugador -> Posición actual del jugador.
;   posicio-meta    -> Posición de la meta.
;   passes          -> Número de pasos realizados.
;   nom-fitxer      -> Nombre del archivo del laberinto.
;   nom-jugador     -> Nombre del jugador.
;   ancho-ventana   -> Ancho de la ventana gráfica.
;   alto-ventana    -> Alto de la ventana gráfica.
(defun explora-rec (laberint posicio-jugador posicio-meta passes nom-fitxer nom-jugador ancho-ventana alto-ventana)
  (let ((moviment (llegir-moviment)))
    (cond
     ((eq moviment 'sortir)
      (color 0 0 0)
      (cls)
      (format t "Has sortit de l'exploracio.~%")
      t)
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

; Solicita el nombre del jugador y valida que no esté vacío.
; Sin parámetros.
(defun solicitar-nombre-jugador ()
  (let ((nom-jugador (read-line))) ; Lee el nombre del jugador
    (cond
     ((and nom-jugador (not (equal nom-jugador ""))) nom-jugador) ; Si el nombre es válido, lo devuelve
     (t
      (format t "Introdueix el teu nom: ") ; Vuelve a mostrar el mensaje
      (solicitar-nombre-jugador))))) ; Llama recursivamente hasta obtener un nombre válido
        
; -------------------------- CARGAR_LABERINTO ---------------------------

; Carga el laberinto desde un archivo de texto y lo convierte en una lista de listas.
; Parámetros:
;   nom-fitxer -> Nombre del archivo del laberinto.
(defun cargar-laberinto (nom-fitxer)
  (let ((fp (open nom-fitxer :direction :input)))
    (let ((laberinto (cargar-lineas-rec fp)))
      (close fp)
      laberinto)))

; Lee las líneas del archivo y las convierte en listas de símbolos recursivamente.
; Parámetros:
;   fp -> Descriptor de archivo abierto para lectura.
(defun cargar-lineas-rec (fp)
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) nil) ; Si es el final del archivo, termina
     ((eq caracter #\Newline) ; Si es un salto de línea, procesa la siguiente línea
      (cons nil (cargar-lineas-rec fp)))
     (t ; Acumula caracteres en la línea actual
      (let ((linea (cargar-linea-rec fp (list (caracter-a-simbolo caracter)))))
        (cons linea (cargar-lineas-rec fp)))))))

; Lee caracteres de una línea y los convierte en símbolos recursivamente.
; Parámetros:
;   fp         -> Descriptor de archivo abierto para lectura.
;   acumulador -> Lista acumuladora de símbolos.
(defun cargar-linea-rec (fp acumulador)
  (let ((caracter (read-char fp nil 'eof)))
    (cond
     ((eq caracter 'eof) (reverse acumulador)) ; Fin del archivo
     ((eq caracter #\Newline) (reverse acumulador)) ; Fin de la línea
     (t (cargar-linea-rec fp (cons (caracter-a-simbolo caracter) acumulador))))))

; -------------------------- BUSCAR_POSICION -----------------------------

; Busca la posición de un símbolo en el laberinto.
; Parámetros:
;   laberinto -> Laberinto como lista de listas.
;   simbolo   -> Símbolo a buscar ('entrada, 'sortida, etc.).
(defun buscar-posicion (laberinto simbolo)
  (buscar-en-laberinto laberinto simbolo 0))

; Busca el símbolo en el laberinto fila por fila.
; Parámetros:
;   laberinto   -> Laberinto como lista de listas.
;   simbolo     -> Símbolo a buscar.
;   fila-index  -> Índice de la fila actual.
(defun buscar-en-laberinto (laberinto simbolo fila-index)
  (cond
   ((null laberinto) nil)
   (t (let ((pos (buscar-en-fila (car laberinto) simbolo fila-index 0)))
        (cond
         (pos pos)
         (t (buscar-en-laberinto (cdr laberinto) simbolo (+ fila-index 1))))))))

; Busca el símbolo en una fila específica.
; Parámetros:
;   fila        -> Lista con los elementos de la fila.
;   simbolo     -> Símbolo a buscar.
;   fila-index  -> Índice de la fila actual.
;   col-index   -> Índice de la columna actual.
(defun buscar-en-fila (fila simbolo fila-index col-index)
  (cond
   ((null fila) nil)
   ((eq (car fila) simbolo) (list fila-index col-index))
   (t (buscar-en-fila (cdr fila) simbolo fila-index (+ col-index 1)))))

; -------------------------- GUARDAR CLASSIFICACIO --------------------------

; Guarda el número de pasos, el nombre del jugador y el nombre del fichero en el archivo de clasificación.
; Parámetros:
;   passes      -> Número de pasos realizados.
;   nom-fitxer  -> Nombre del archivo del laberinto.
;   nom-jugador -> Nombre del jugador.
(defun guardar-nom-i-passes (passes nom-fitxer nom-jugador)
  (let ((fitxer (format nil "~A_classificacio.txt" nom-fitxer)))
    (let ((fp (open fitxer :direction :output :if-exists :append :if-does-not-exist :create)))
      (cond
       (fp
        (format fp "~S~%" (list passes nom-jugador nom-fitxer)) ; Guarda como lista
        (close fp)
        (format t "S'ha guardat el teu nom, passes i mapa al fitxer ~A.~%~%" fitxer))
       (t (format t "Error: No s'ha pogut obrir el fitxer ~A.~%" fitxer))))))

; -------------------------- MOSTRAR CLASSIFICACIO --------------------------

; Muestra las 10 mejores exploraciones de un laberinto específico.
; Parámetros:
;   nom-fitxer -> Nombre del archivo del laberinto.
(defun mostrar-classificacio (nom-fitxer)
  (let ((fitxer (format nil "~A_classificacio.txt" nom-fitxer)))
    (let ((classificacio (carregar-classificacio fitxer)))
      (format t "Classificacio de les 10 millors exploracions de ~A:~%" nom-fitxer)
      (mostrar-entrades classificacio 10)
      t)))

; Muestra las primeras n entradas de la clasificación en el formato: Nom-Jugador: X passes (nom-laberint).
; Parámetros:
;   classificacio -> Lista de entradas de clasificación.
;   n             -> Número de entradas a mostrar.
(defun mostrar-entrades (classificacio n)
  (cond
   ((or (null classificacio) (<= n 0)) nil) ; Caso base: no hay más entradas o se han mostrado todas
   (t (let ((entrada (car classificacio)))
        (format t "~A: ~A passes ~%" (cadr entrada) (car entrada))
        (mostrar-entrades (cdr classificacio) (- n 1)))))) ; Llamada recursiva para el resto de las entradas

; Carga la clasificación desde el archivo y la devuelve ordenada por número de pasos.
; Parámetros:
;   fitxer -> Nombre del archivo de clasificación.
(defun carregar-classificacio (fitxer)
  (let ((fp (open fitxer :direction :input)))
    (cond
     (fp
      (let ((classificacio (carregar-linies-rec fp)))
        (close fp)
        (ordenar-classificacio classificacio)))
     (t (error "No s'ha pogut obrir el fitxer ~A." fitxer)))))

; Lee las líneas del archivo de clasificación recursivamente y las convierte en una lista.
; Parámetros:
;   fp -> Descriptor de archivo abierto para lectura.
(defun carregar-linies-rec (fp)
  (let ((linia (read fp nil)))
    (cond
     (linia (cons linia (carregar-linies-rec fp)))
     (t nil))))

; Ordena la lista de clasificación por número de pasos usando recursión.
; Parámetros:
;   classificacio -> Lista de entradas de clasificación.
(defun ordenar-classificacio (classificacio)
  (cond
   ((null classificacio) nil)
   (t (insertar-ordenado (car classificacio) (ordenar-classificacio (cdr classificacio))))))

; Inserta una entrada en una lista ordenada por número de pasos.
; Parámetros:
;   entrada -> Entrada a insertar.
;   lista   -> Lista ordenada.
(defun insertar-ordenado (entrada lista)
  (cond
   ((null lista) (list entrada))
   ((< (car entrada) (car (car lista))) (cons entrada lista))
   (t (cons (car lista) (insertar-ordenado entrada (cdr lista))))))

;-------------------------- MOVIMENT DEL JUGADOR ------------------------

; Lee el movimiento del jugador desde el teclado.
; Sin parámetros.
(defun llegir-moviment ()
  (let ((tecla (get-key)))
    (cond
     ((or (= tecla 65) (= tecla 97) (= tecla 331)) 'esquerra) ; A o ←
     ((or (= tecla 68) (= tecla 100) (= tecla 333)) 'dreta)   ; D o →
     ((or (= tecla 87) (= tecla 119) (= tecla 328)) 'amunt)   ; W o ↑
     ((or (= tecla 83) (= tecla 115) (= tecla 336)) 'avall)   ; S o ↓
     ((= tecla 27) 'sortir)                                  ; ESC
     (t nil))))                                              ; Qualsevol altra tecla

; Comprueba si el movimiento es válido.
; Parámetros:
;   laberint -> Laberinto como lista de listas.
;   posicio  -> Posición actual del jugador.
;   moviment -> Movimiento a comprobar.
(defun moviment-valid? (laberint posicio moviment)
  (let* ((x (first posicio))
         (y (second posicio))
         (nova-posicio
          (cond
           ((eq moviment 'amunt) (list (+ x 1) y))    
           ((eq moviment 'avall) (list (- x 1) y))    
           ((eq moviment 'esquerra) (list x (- y 1)))
           ((eq moviment 'dreta) (list x (+ y 1)))
           (t posicio))))
    (and (>= (first nova-posicio) 0) ; Comprueba que la fila esta dentro de los límites
         (< (first nova-posicio) (contar-elementos laberint)) ; Fila no supera el límite
         (>= (second nova-posicio) 0) ; Comprueba que la columna esta dentro de los límites
         (< (second nova-posicio) (contar-elementos (car laberint))) ; Columna no supera el límite
         (or (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'cami) ; Permite moverse a 'cami
             (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'entrada) ; Permite moverse a 'entrada
             (eq (get-celda laberint (first nova-posicio) (second nova-posicio)) 'sortida))))) ; Permite moverse a 'sortida

; Actualiza la posición del jugador según el movimiento.
; Parámetros:
;   posicio  -> Posición actual del jugador.
;   moviment -> Movimiento a realizar.
(defun moure-jugador (posicio moviment)
  (let ((x (first posicio))
        (y (second posicio)))
    (cond
     ((eq moviment 'amunt) (list (+ x 1) y))    ; Cambiado: ahora suma para 'amunt'
     ((eq moviment 'avall) (list (- x 1) y))    ; Cambiado: ahora resta para 'avall'
     ((eq moviment 'esquerra) (list x (- y 1))) ; Mou cap a l'esquerra (columna -1)
     ((eq moviment 'dreta) (list x (+ y 1)))    ; Mou cap a la dreta (columna +1)
     (t posicio))))                             ; Si no és vàlid, no es mou

; Comprueba si el jugador ha llegado a la meta.
; Parámetros:
;   posicio-jugador -> Posición actual del jugador.
;   posicio-meta    -> Posición de la meta.
(defun es-meta? (posicio-jugador posicio-meta)
  (equal posicio-jugador posicio-meta))

; -------------------------- FUNCIONES AUXILIARES ------------------------

; Convierte un carácter del archivo a su representación simbólica.
; Parámetros:
;   caracter -> Carácter leído del archivo.
(defun caracter-a-simbolo (caracter)
  (cond
   ((eq caracter #\#) 'paret)
   ((eq caracter #\.) 'cami)
   ((eq caracter #\e) 'entrada)
   ((eq caracter #\s) 'sortida)
   (t 'desconocido)))