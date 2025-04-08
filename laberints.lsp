(load "generacio.lsp")
(load "exploracio.lsp")

(defun main ()
  "Funcion principal que muestra un menu interactivo para gestionar laberintos."
  (format t "Benvingut al gestor de laberints!~%")
  (menu))

(defun menu ()
  "Muestra un menu interactivo para el usuario."
  (format t "~%Selecciona una opcio:~%")
  (format t "1. Generar un laberint~%")
  (format t "2. Explorar un laberint~%")
  (format t "3. Sortir~%")
  (let ((opcio (read)))
    (cond
     ((= opcio 1) (menu-generar-laberint))
     ((= opcio 2) (menu-explorar-laberint))
     ((= opcio 3) (format t "Sortint del programa. Adeu!~%"))
     (t (format t "Opcio no valida. Torna-ho a intentar.~%")
        (menu)))))

(defun menu-generar-laberint ()
  "Permite al usuario generar un único laberinto."
  (format t "Introdueix el nom del fitxer per al laberint: ")
  (let ((nom-fitxer (read-line)))
    (format t "Introdueix el nombre de files: ")
    (let ((files (read)))
      (format t "Introdueix el nombre de columnes: ")
      (let ((columnes (read)))
        (genera nom-fitxer files columnes)
        (format t "Laberint generat i guardat a ~A.~%~%" nom-fitxer)
        (menu)))))

(defun menu-explorar-laberint ()
  "Permite al usuario explorar un laberinto existente."
  (format t "Introdueix el teu nom: ")
  (let ((nom-jugador (read-line)))
    (format t "Introdueix el nom del fitxer del laberint a explorar: ")
    (let ((nom-fitxer (read-line)))
      (explora nom-fitxer nom-jugador)
      (menu))))