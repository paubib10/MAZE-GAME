(load "genmaze.lsp")
(load "exploremaze.lsp")

(defun main ()
  ; "Función principal para generar un laberinto y guardarlo en un archivo."
  (let ((nombre-archivo "laberinto.txt") ; Nombre del archivo de salida
        (filas 10)                      ; Número de filas del laberinto
        (columnas 10))                  ; Número de columnas del laberinto
    (genera nombre-archivo filas columnas) ; Generar el laberinto
    (format t "Maze generated and saved in ~a~%" nombre-archivo)
    (imprimir-laberinto nombre-archivo))) ; Leer e imprimir el laberinto

; Llamar al main para ejecutar el programa
(main)