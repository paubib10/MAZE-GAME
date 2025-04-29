; ------------------- BLOQUE DE COMENTARIO -------------------
; Autores: Pau Toni Bibiloni Martínez & Hugo Guerreiro Paredes
; Fecha: 2025-04-29
; Asignatura: Lenguajes de Programación
; Grupo: 3
; Profesores: Antoni Oliver Tomàs
; Convocatoria: Ordinaria
; ------------------------------------------------------------

; ---------------------- INSTRUCCIONES -----------------------
; Este programa permite generar y explorar laberintos.
; 
; Pasos para usar el programa:
; 1. Cargar el archivo principal:
;    - Ejecuta el comando: (load "laberints.lsp")
;    - Esto cargará todas las funciones necesarias para generar y explorar laberintos.
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
; 3. Explorar un laberinto:
;    - Función: (explora nom-fitxer)
;    - Parámetros:
;        * nom-fitxer: Nombre del archivo que contiene el laberinto.
;    - Ejemplo: (explora "laberint.txt")
;      Esto permitirá al usuario explorar el laberinto guardado en "laberint.txt".
;      El programa pedirá al usuario que introduzca su nombre antes de comenzar.
;
; ------------------ ASPECTOS OPCIONALES ---------------------
; 1. Ajuste dinámico del tamaño de las celdas:
;    - Implementado en la función (calcular-celda-tam ancho-ventana alto-ventana laberinto).
;    - Permite que el laberinto se adapte al tamaño de la ventana gráfica.
;
; ------------------ DISEÑO FUNCIONAL ------------------------
; El programa está dividido en dos partes principales:
; 1. Generación de laberintos:
;    - Se utiliza la función (genera) para crear un laberinto aleatorio con las dimensiones
;      especificadas por el usuario. El laberinto se guarda en un archivo de texto.
;
; 2. Exploración de laberintos:
;    - Se utiliza la función (explora) para cargar un laberinto desde un archivo y permitir
;      al jugador moverse por él. Se incluyen validaciones para movimientos y detección
;      de la meta.
;
; ------------------------------------------------------------

(load "generacio.lsp")
(load "exploracio.lsp")
