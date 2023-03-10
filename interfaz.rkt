#lang racket/gui
(require 2htdp/image)
(require "algoritmo.rkt")

; Declare the starting window.
(define frame (new frame% [label "4Line"]
                          [width 200]
                          [height 200]))

; Declare the game window in a funtion because the size of window depends of the number of rows (n) and columns (M).


;<---------------------------------------------------------------------- START WINDOW -------------------------------------------------------------------------------------------->


; Show a message to player.
(new message% [parent frame]
              [label "4 line"])

; Set the horizontal panel for the input text fields.
(define PanelInput (new horizontal-panel%
                   [parent frame]
                   [alignment '(center center)]))

; Set the input text field for the N parameter for the board.
(define filas (new choice% [parent PanelInput]
                                [label "Filas "]
                                [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18")]
                                [vert-margin 10]
                                [horiz-margin 30]))

; Set the input text field for the M parameter for the board.
(define columnas (new choice% [parent PanelInput]
                                [label "Columnas "]
                                [choices (list "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18")]
                                [vert-margin 10]
                                [horiz-margin 30]))

; Set the "Play!" button.
(new button% [parent frame]
             [label "Jugar"]
             [vert-margin 100]
             [callback (lambda (button event) (juego (send filas get-string-selection) (send columnas get-string-selection)) (CloseStartWindow event))])

; Show the start window.
(send frame show #t)

; Define the event to close the start window.
(define (CloseStartWindow event)
  (send frame show #f))

; Function to set the tokens on the board.
(define (dibujarFicha color posX posY dc)
  (send dc set-brush color 'solid)
  (send dc set-pen color 0 'solid)
  (send dc draw-ellipse (* 70 posY) (* 56 posX) 70 56))

(define (juego n m)
  (define matriz (crearmatriz (string->number n) (string->number m)))

  (define (clickCasilla columna)
    (cond ((equal? (turno 1 4 (+ columna 1) matriz) #t) (message-box "Fin de la partida" "Ganaste!"))
          (else (clickCasilla_aux (turno 1 4 (+ columna 1) matriz))))
    (send ventanaJuego refresh))

  (define (clickCasilla_aux matriz_aux)
    (cond ((equal? (turno 2 4 (algoritmo matriz_aux) matriz_aux) #t) (message-box "Fin de la partida" "Ganó la computadora"))
           (else (set! matriz (turno 2 4 (algoritmo matriz_aux) matriz_aux)))))

  ; Declare the width and height of the game window.
  (define anchoPX 0)
  (define altoPX 0)

  ; for loops to define the value of width and height, depends of the number of rows (n) and columns (m) because each
  ; space of the board have a constant size, so for each space it add 50px to "widthPX" and "heightPX" variables.
  (for ([j (in-range (string->number n))])
    (set! altoPX (+ 56 altoPX)))

  (for ([i (in-range (string->number m))])
    (set! anchoPX (+ 70 anchoPX)))
         
  ; Declare the game window, with a variable size that depends of the n and m parameters. 
  (define ventanaJuego (new frame% [label "4Line"]
                          [width anchoPX]
                          [height altoPX]))
  
  (define PanelInput (new horizontal-panel%
                          [parent ventanaJuego]
                          [alignment '(center center)]))

  (define botones
    (for/list ([i (in-range (string->number m))])
      (new button% [parent PanelInput] [label (format "~a" (add1 i))] [min-height 56] [callback (lambda (button event) (clickCasilla i))])))
  
  ;<-------------CANVAS----------------->
  ; Canva to draw the lines of the board
  (define (draw-canvas canvas dc)
           
    (send dc set-pen "black" 1 'solid)
    (for ([i (in-range 0 (string->number n))])
      (send dc draw-line 0 (* i 56) anchoPX (* i 56)))

    (for ([j (in-range 1 (string->number m))])
      (send dc draw-line (* j 70) 0 (* j 70) altoPX))

    (for ([i (in-range 0 (+ (string->number n) 1))])
      (for ([j (in-range 0 (+ (string->number m) 1))])
        (cond ((equal? (elementoMatriz matriz (- (string->number n) i) j) 1) (dibujarFicha "red" i (- j 1) dc))
              ((equal? (elementoMatriz matriz (- (string->number n) i) j) 2) (dibujarFicha "blue" i (- j 1) dc))))))
    
  ;<------------------------------------>
  ; Define a new class of canvas to control what is going to be drawn in the screen. Contains some code to declare the x and y coordinates and set them
  ; to the actual position of the mouse when the left button is clicked.
  (define my-canvas% (class canvas%
                       (super-new)
                       (define/override (on-event event)
                         (match (send event get-event-type)
                           [else (void)]))))

  ; Instantiate a new canvas to draw all stuff on the window
  (define Canva (new my-canvas% [parent ventanaJuego] [paint-callback draw-canvas] [min-width anchoPX] [min-height altoPX]))

  ; Call the window to be shown
  (send ventanaJuego show #t)   
  )