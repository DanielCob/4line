#lang racket/gui
(require 2htdp/image)
(require "algoritmo.rkt")

#|
 -Función crearmatriz
 -Declara la ventana inicial
|#
(define frame (new frame% [label "4Line"]
                          [width 200]
                          [height 200]))

(new message% [parent frame]
              [label "4 line"])

#|
 -Función PanelInput
 -Crea un espacio en la ventana para los botones
|#
(define PanelInput (new horizontal-panel%
                   [parent frame]
                   [alignment '(center center)]))

#|
 -Función filas
 -Crea un Choice para elegir la cantidad de filas
|#
(define filas (new choice% [parent PanelInput]
                                [label "Filas "]
                                [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18")]
                                [vert-margin 10]
                                [horiz-margin 30]))

#|
 -Función columnas
 -Crea un Choice para elegir la cantidad de columnas
|#
(define columnas (new choice% [parent PanelInput]
                                [label "Columnas "]
                                [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18")]
                                [vert-margin 10]
                                [horiz-margin 30]))

#|
 -Botón
 -Llama la ventana de juego y cierra la ventana principal.
|#
(new button% [parent frame]
             [label "Jugar"]
             [vert-margin 10]
             [callback (lambda (button event) (juego (send filas get-string-selection) (send columnas get-string-selection)) (CerrarVentanaInicio event))])

#|
 -Muestra la ventana principal al correr el programas
|#
(send frame show #t)

#|
 -Función CerrarVentanaInicio
 -Cierra la ventanta de inicio
|#
(define (CerrarVentanaInicio event)
  (send frame show #f))

;----------------------------------------------------------------

#|
 -Función dibujarFicha
 -Dibuja las elipses que corresponden a las fichas
 -Recibe el color de la fichas, la posición X y Y donde se va a dibujar y el objeto que lo dibuja.
|#
(define (dibujarFicha color posX posY dc)
  (send dc set-brush color 'solid)
  (send dc set-pen color 0 'solid)
  (send dc draw-ellipse (* 70 posY) (* 56 posX) 70 56))

#|
 -Función juego
 -Crea la ventana donde se va a jugar el juego
 -Recibe el número de filas n y el número de columnas m
|#
(define (juego n m)

  #|
   -Matriz
   -Variable de la matriz donde se almacena el tablero para la interfaz gráfica
  |#
  (define matriz (crearmatriz (string->number n) (string->number m)))

  #|
   -Función clickCasilla
   -Esta función se usa cada vez que se le da click a algun boton de casilla, inserta la ficha en la
   casiila deseada por el jugador, verifica si ganó o si continua la partida y procede a hacer lo mismo con el algoritmo
   enviándole la matriz que acaba de ser modificada por el jugador
   -Recibe el numero de columna del botón al que corresponde la llamada
   -Puede enviar un mensaje de victoria, perdida o empate si se cumplen las condiciones
  |#
  (define (clickCasilla columna)
    (cond ((equal? (turno 1 4 (+ columna 1) matriz) #t) (message-box "Fin de la partida" "Ganaste!"))
          (else (clickCasilla_aux (turno 1 4 (+ columna 1) matriz))))
    (send ventanaJuego refresh))

  (define (clickCasilla_aux matriz_aux)
    (cond ((equal? (turno 2 4 (algoritmo matriz_aux) matriz_aux) #t) (message-box "Fin de la partida" "Ganó la computadora"))
           (else (set! matriz (turno 2 4 (algoritmo matriz_aux) matriz_aux)))))

  #|
   -anchoPX y altoPX
   -Define el largo y ancho de la ventana dependiendo del número de filas y columnas.
  |#
  (define anchoPX 0)
  (define altoPX 0)

  (for ([j (in-range (string->number n))])
    (set! altoPX (+ 56 altoPX)))

  (for ([i (in-range (string->number m))])
    (set! anchoPX (+ 70 anchoPX)))
         
  #|
   -Función ventanaJuego
   -Crea la ventana de juego con el tamaño definido por las columnas y filas
   -Recibe las variable anchoPX y altoPX
  |# 
  (define ventanaJuego (new frame% [label "4Line"]
                          [width anchoPX]
                          [height altoPX]))
  
  (define PanelInput (new horizontal-panel%
                          [parent ventanaJuego]
                          [alignment '(center center)]))

    #|
   -Función botones
   -Crea la cantidad de botones para cada columna
   -Recibe la cantidad de columnas
   |# 
  (define botones
    (for/list ([i (in-range (string->number m))])
      (new button% [parent PanelInput] [label (format "~a" (add1 i))] [min-height 56] [callback (lambda (button event) (clickCasilla i))])))
  
  #|
   -Función dibujarCanvas
   -Dibuja el tablero y lee la matriz para colocar las fichas en pantalla
   -Recibe dos objetos necesarios para dibujar
   |# 
  (define (dibujarCanvas canvas dc)
           
    (send dc set-pen "black" 1 'solid)
    (for ([i (in-range 0 (string->number n))])
      (send dc draw-line 0 (* i 56) anchoPX (* i 56)))

    (for ([j (in-range 1 (string->number m))])
      (send dc draw-line (* j 70) 0 (* j 70) altoPX))

    (for ([i (in-range 0 (+ (string->number n) 1))])
      (for ([j (in-range 0 (+ (string->number m) 1))])
        (cond ((equal? (elementoMatriz matriz (- (string->number n) i) j) 1) (dibujarFicha "red" i (- j 1) dc))
              ((equal? (elementoMatriz matriz (- (string->number n) i) j) 2) (dibujarFicha "blue" i (- j 1) dc))))))
    
  #|
   -Función my-canvas%
   -Se utiliza para dibujar en pantalla
  |# 
  (define my-canvas% (class canvas%
                       (super-new)
                       (define/override (on-event event)
                         (match (send event get-event-type)
                           [else (void)]))))

  #|
   -Función dibujarCanvas
   -Instancia un canvas para dibujar todo en pantalla
   |# 
  (define Canva (new my-canvas% [parent ventanaJuego] [paint-callback dibujarCanvas] [min-width anchoPX] [min-height altoPX]))

  #|
   -Permite mostrar la ventana del juego
   |# 
  (send ventanaJuego show #t)   
  )