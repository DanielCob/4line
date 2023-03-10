#lang racket/gui
(require 2htdp/image)
(require "algoritmo.rkt")

; Function to set the tokens on the board.
(define (dibujarFicha color posX posY dc)
  (send dc set-brush color 'solid)
  (send dc set-pen color 0 'solid)
  (send dc draw-ellipse (* 50 posY) (* 50 posX) 50 50))

; Define an event to control the mouse position through x and y coordinates on the screen.
(define (mouse-pos event)
  (define-values (x) (send event get-x))
  (define-values (y) (send event get-y))
  (values x y))

(define Column 0)
(define cargarMatriz #t)

(define (GetColumn x)
  (set! Column (quotient x 50)))


(define (juego n m)
  (define matriz (crearmatriz (string->number n) (string->number m)))

  ; Declare the width and height of the game window.
  (define anchoPX 0)
  (define altoPX 0)

  ; for loops to define the value of width and height, depends of the number of rows (n) and columns (m) because each
  ; space of the board have a constant size, so for each space it add 50px to "widthPX" and "heightPX" variables.
  (for ([j (in-range (string->number n))])
    (set! altoPX (+ 50 altoPX)))

  (for ([i (in-range (string->number m))])
    (set! anchoPX (+ 50 anchoPX)))
         
  ; Declare the game window, with a variable size that depends of the n and m parameters. 
  (define ventanaJuego (new frame% [label "4Line GAME"]
                          [width anchoPX]
                          [height altoPX]))
  
  (define PanelInput (new horizontal-panel%
                          [parent ventanaJuego]
                          [alignment '(center center)]))
  
  (define (escribir texto)
    (print texto))

  (define button (new button%
                      [parent PanelInput]
                      (label "Button")
                      (callback (lambda (button event) (escribir "hola mundo")))
                      [vert-margin 10]
                      [horiz-margin 0]))

  (define button2 (new button%
                       [parent PanelInput]
                       (label "Button")
                       (callback (lambda (button event) (escribir "adios mundo")))
                       [vert-margin 10]
                       [horiz-margin 0]))
  
  ;<-------------CANVAS----------------->
  ; Canva to draw the lines of the board
  (define (draw-canvas canvas dc)
           
    (send dc set-pen "black" 1 'solid)
    (for ([i (in-range 0 (string->number n))])
      (send dc draw-line 0 (* i 50) anchoPX (* i 50)))

    (for ([j (in-range 1 (string->number m))])
      (send dc draw-line (* j 50) 0 (* j 50) altoPX))

    (cond ((equal? cargarMatriz #t)
           (for ([i (in-range 0 (+ (string->number n) 1))])
             (for ([j (in-range 0 (+ (string->number m) 1))])
               (cond ((equal? (elementoMatriz matriz (- (string->number n) i) j) 1) (dibujarFicha "red" i (- j 1) dc))
                     ((equal? (elementoMatriz matriz (- (string->number n) i) j) 2) (dibujarFicha "blue" i (- j 1) dc))))))))
    
  ;<------------------------------------>
  ; Define a new class of canvas to control what is going to be drawn in the screen. Contains some code to declare the x and y coordinates and set them
  ; to the actual position of the mouse when the left button is clicked.
  (define my-canvas% (class canvas%
                       (super-new)
                       (define/override (on-event event)
                         (match (send event get-event-type)
                           ['left-down
                            (let-values (((x y) (mouse-pos event)))
                              (GetColumn x) (displayln Column) (set! cargarMatriz #f) (send ventanaJuego refresh) (send Canva flush))]
                           [else (void)]))))

  ; Instantiate a new canvas to draw all stuff on the window
  (define Canva (new my-canvas% [parent ventanaJuego] [paint-callback draw-canvas] [min-width anchoPX] [min-height altoPX]))

  ; Call the window to be shown
  (send ventanaJuego show #t)   
  )

(juego "8" "8")