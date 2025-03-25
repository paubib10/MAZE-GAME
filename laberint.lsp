(load "genmaze.lsp")
(load "exploremaze.lsp")

(defun main ()
  ; "Función principal para generar un laberinto y guardarlo en un archivo."
  (let ((nombre-archivo "laberinto.txt") ; Nombre del archivo de salida
        (filas 15)                      ; Número de filas del laberinto
        (columnas 15))                  ; Número de columnas del laberinto
    (genera nombre-archivo filas columnas) ; Generar el laberinto
    (format t "Maze generated and saved in ~a~%" nombre-archivo)
    (explora nombre-archivo))) ; Explorar y dibujar el laberinto

; Llamar al main para ejecutar el programa
(main)