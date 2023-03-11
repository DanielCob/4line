#lang racket
(provide crearmatriz elementoMatriz algoritmo turno)

#|
 -Función crearmatriz
 -Crea una matriz con todos sus elementos iguales a cero de tamaño columnaXfila.
 -Recibe la cantidad filas y columnas.
 -Devuelve una matriz con todos sus elementos iguales a cero de tamaño columnaXfila.
|# 
(define (crearmatriz fila columna)
  (cond ((or (zero? fila) (zero? columna))
         '())
        (else (crearmatriz-aux fila columna))))

(define (crearmatriz-aux fila columna)
  (cond ((zero? columna)
         '())
        (else
         (append (list (agregarfilas fila)) (crearmatriz-aux fila (- columna 1))))))

(define (agregarfilas fila)
  (cond ((zero? fila)
         '())
        (else
         (cons 0 (agregarfilas (- fila 1))))))

#|
 -Función elemento
 -Recorre en una lista hasta encontrar el elemento correspondiende al indice recibido
 -Recibe un numero index y una lista
 -Devuelve el elemento en la posición deseada, en caso de el indice ser mas grande que la lista devuelve #f
|#
(define (elemento  index lista)
  (cond ((null? lista)
         lista)
        ((equal? index 1)
         (car lista))
        (else
         (elemento (- index 1) (cdr lista)))))

#|
 -Función elementoMatriz
 -Indexa con ayuda de la función elemento a una fila y columna de la matriz.
 -Recibe una fila y columna, junto a la matriz donde se indexara.
 -Devuelve el elemento de la casilla solicitada.
|#
(define (elementoMatriz matriz fila columna )
  (cond((null? matriz)
        matriz)
       (else
        (elemento fila (elemento columna matriz)))))

#|
 -Funcion agregar
 -Se encarga de agregar una ficha nueva con el numero del jugador en el primer espacio disponible en la columna seleccionada de la matriz
 -Recibe el numero de jugador, la columna donde se colocará y la matriz sin actualizar
 -Devuelve una matriz actualizada con la ficha ya agregada.
|#
(define (agregar jugador columna matriz )
  (cond ((null? matriz)
         matriz)
        (else
         (agregar-aux jugador columna matriz))))

#|
 -Funcion agregar-aux
 -Se encarga de construir la matriz actualizada, agrega en orden las demás columnas y la columna con la ficha agregada
 -Recibe el numero de jugador, la columna donde se colocará y la matriz sin actualizar
 -Devuelve una matriz actualizada con la ficha ya agregada.
|#
(define (agregar-aux jugador columna matriz)
  (cond ((null? matriz)
         matriz)
        ((equal? columna 1)
         (append (list (agregarFicha jugador (car matriz))) (cdr matriz)))
        (else
         (append (list (car matriz)) (agregar-aux jugador (- columna 1) (cdr matriz))))))

#|
 -Funcion agregarFicha
 -Se encarga de agregar una ficha nueva con el numero del jugador en el primer espacio disponible en la lista (columna) recibida
 -Recibe el numero de jugador y la columnda donde se colocará
 -Devuelve la columna con la ficha agregada.
|#
(define (agregarFicha jugador lista)
  (cond ((null? lista)
         lista)
        ((zero? (car lista))
         (append (list jugador) (cdr lista)))
        (else
         (append (list (car lista)) (agregarFicha jugador (cdr lista))))))

#|
 -Funcion numeroFila
 -Es un contador que devuelve el numero de fila donde está el ultimo elemento distinto de cero en una columna.
 -Recibe fila el cual es el contador que irá aumentando, y una lista que será la columna.
 -Devuelve el numero de fila de la ultima ficha.
|#
(define (numeroFila columna)
  (numeroFila_aux 0 columna))

(define (numeroFila_aux indice columna)
  (cond ((null? columna)
          #f)
        ((zero? (car columna))
         indice)
        (else
         (numeroFila_aux (+ indice 1) (cdr columna)))))

#|
 -Funcion algoritmo
 -Elige una columna óptima basada en una heurística.
 -Recibe la matriz del juego.
 -Devuelve una columna donde poner la ficha.
|#
(define (algoritmo matriz)
  (seleccion (objetivo (viabilidad matriz) matriz))
  )

#|
 -Funcion viabilidad
 -Elige las posibles columnas (candidatos) donde insertar fichas.
 -Recibe la matriz del juego.
 -Devuelve una lista con todas las columnas disponibles.
|#
(define (viabilidad matriz)
  (viabilidad_aux matriz '() 1))

(define (viabilidad_aux matriz lista indice)
  (cond ((null? matriz) lista)
        ((false? (numeroFila (car matriz))) (viabilidad_aux (cdr matriz) lista (+ indice 1)))
        (else (viabilidad_aux (cdr matriz) (append lista (list indice)) (+ indice 1)))))

#|
 -Funcion objetivo
 -Asigna un valor de heurística a cada columna dependiendo de su valor estratégico.
 -Recibe una lista con todas las columnas disponibles y la matriz del juego.
 -Devuelve una lista con todas las columnas y sus valores heurísticos.
|#
(define (objetivo columnas matriz)
  (objetivo_aux columnas matriz 1)
  )

(define (objetivo_aux columnas matriz indice)
  (cond ((null? columnas) columnas)
        ((equal? indice (car columnas))
         (cons (list indice (heuristica (numeroFila (elemento (car columnas) (agregar 2 (car columnas) matriz))) (car columnas) matriz)) (objetivo_aux (cdr columnas) matriz (+ indice 1))))
        (else (cons (list indice 0) (objetivo_aux  columnas matriz (+ indice 1))))))

#|
 -Funcion heurística
 -Asigna un valor de heurística a un espacio en el tablero dependiendo de su valor estratégico.
 -Recibe la fila y columna del espacio y la matriz del juego.
 -Devuelve una lista con el indice de la columna y sus valor heurístico.
|#
(define (heuristica fila columna matriz)
  (+ (combo (verifHorizontal (agregar 2 columna matriz) fila columna))
  (combo (verifVertical (agregar 2 columna matriz) fila columna))
  (combo (verifDiagonal_suma (agregar 2 columna matriz) fila columna))
  (combo_enemigo (verifHorizontal (agregar 1 columna matriz) fila columna))
  (combo_enemigo (verifVertical (agregar 1 columna matriz) fila columna))
  (combo_enemigo (verifDiagonal_suma (agregar 1 columna matriz) fila columna))))

(define (combo num)
  (cond ((= num 2) (+ num 1))
        ((>= num 3)(+ num 2))
        (else num)))

(define (combo_enemigo num)
  (cond ((>= num 2) (+ num 2))
        ((>= num 3)(+ num 4))
        (else num)))

#|
 -Funcion selección
 -Elige la primera columna con mayor número de heurística asignado.
 -Recibe las columnas con sus heurísticas de la forma: ((1 0) (2 6) ({columna} {valor})).
 -Devuelve el número de columna con mayor heurística.
|#
(define (seleccion columnas_heuristica)
  (car (argmax cadr columnas_heuristica)))

#|
 -Función turno (Función de solución)
 -Esta función agrega y verifica si la ficha agregada hace un 4 en linea.
 -Recibe el numero de jugador, el combo (brinda flexibilidad para usar la funcion en el algoritmo goloso)
  la columna donde se colocará la ficha y la matriz antes de agregar la ficha.
 -Devuelve #t si la ficha agregada hace un 4 en linea, sino devuelve la matriz actualizada con la ficha.
|#
(define (turno jugador combo columna matriz)
  (cond ((null? matriz)
         matriz)
        ((verificar (numeroFila (elemento columna (agregar jugador columna matriz))) columna combo (agregar jugador columna matriz))
         #t)
        (else
         (agregar jugador columna matriz))))
                
#|
  -Función verificar
  -Verifica si hay cuatro o mas elementos en raya en cualquier dirección.
  -Recibe la fila y columna de la casilla donde se hará la verificación, así como la matriz del juego
  y el combo, que es la cantidad objetivo de elementos en fila.
  -Devuelve verdadero si la casilla cumple con el requisito de tener el combo en linea, y falso si no lo cumple.
|#
(define (verificar fila columna combo matriz)
  (cond ((or (>= (+ (verifHorizontal matriz fila columna) 1) combo)
             (>= (+ (verifVertical matriz fila columna) 1) combo)
             (>= (+ (verifDiagonal matriz fila columna) 1) combo))
         #t)
        (else
         #f)))

#|
 -Función verifHorizontal
 -Cuenta cuantos elementos iguales que el elemento raiz hay a ambos lados del elemento,
  suma los elementos de la izquierda y de la derecha.
 -Recibe la matriz, fila y columna del elemento donde se empezará a contar a los lados.
 -Devuelve la suma de los elementos iguales seguidos en la misma fila.
|#
(define (verifHorizontal matriz fila columna)
  (cond ((null? matriz)
         0)
        (else
         (+ (verifIzquierda matriz fila columna) (verifDerecha matriz fila columna)))))
         
(define (verifIzquierda matriz  fila  columna)
  (cond ((null? (elementoMatriz matriz fila (- columna 1) ))
         0)
        ((equal? (elementoMatriz matriz fila (- columna 1) ) (elementoMatriz matriz fila columna ))
         (+ 1 (verifIzquierda matriz fila (- columna 1) )))
        (else
         0)))

(define (verifDerecha matriz  fila  columna)
  (cond ((null? (elementoMatriz matriz  fila (+ columna 1)))
         0)
        ((equal? (elementoMatriz matriz fila (+ columna 1)) (elementoMatriz matriz fila columna))
         (+ 1 (verifDerecha matriz fila (+ columna 1) )))
        (else
         0)))

#|
 -Función verifVertical
 -Cuenta cuantos elementos iguales hay debajo del elemento raiz.
 -Recibe la matriz del juego, fila y columna del elemento donde se empezará a contar hacia abajo.
 -Devuelve la suma de los elementos debajo del elemento raiz.
|#        
(define (verifVertical matriz fila columna)
  (cond ((null? (elementoMatriz matriz (- fila 1) columna))
         0)
        ((equal? (elementoMatriz matriz (- fila 1) columna ) (elementoMatriz matriz fila columna))
         (+ 1 (verifVertical matriz (- fila 1) columna ) ))
        (else
         0)))

#|
 -Función verifDiagonal
 -Cuenta cuantos elementos iguales hay en las diagonales ascendente y descendente del elemento raiz.
 -Recibe la matriz del juego, fila y columna del elemento donde se empezará.
 -Devuelve la mayor cantidad de elementos entre las dos diagonales.
|# 
(define (verifDiagonal matriz fila columna)
  (cond ((null? matriz)
         0)
        ((>= (verifDiagonalAsc matriz fila columna) (verifDiagonalDesc matriz fila columna))
         (verifDiagonalAsc matriz fila columna))
        (else
         (verifDiagonalDesc matriz fila columna))))

#|
 -Función verifDiagonal_suma
 -Cuenta cuantos elementos iguales hay en las diagonales ascendente y descendente del elemento raiz.
 -Recibe la matriz del juego, fila y columna del elemento donde se empezará.
 -Devuelve el numero de elementos de ambas diagonales.
|# 
(define (verifDiagonal_suma matriz fila columna)
  (cond ((null? matriz)
         0)
        (else
         (+ (verifDiagonalAsc matriz fila columna) (verifDiagonalDesc matriz fila columna)))))

;**********************************************+
(define (verifDiagonalAsc matriz fila columna)
  (cond ((null? matriz)
         0)
        (else
         (+ (verifDiagonalAscIzq matriz fila columna) (verifDiagonalAscDer matriz fila columna)))))


(define (verifDiagonalAscIzq matriz fila columna)
  (cond ((null? (elementoMatriz matriz  (- fila 1) (- columna 1)))
         0)
        ((equal? (elementoMatriz matriz (- fila 1) (- columna 1)) (elementoMatriz matriz fila columna))
         (+ 1 (verifDiagonalAscIzq matriz (- fila 1) (- columna 1))))
        (else
         0)))

(define (verifDiagonalAscDer matriz fila columna)
  (cond ((null? (elementoMatriz matriz  (+ fila 1) (+ columna 1)))
         0)
        ((equal? (elementoMatriz matriz (+ fila 1) (+ columna 1)) (elementoMatriz matriz fila columna))
         (+ 1 (verifDiagonalAscDer matriz (+ fila 1) (+ columna 1))))
        (else
         0)))

;;**************************************
(define (verifDiagonalDesc matriz fila columna)
  (cond ((null? matriz)
         0)
        (else
         (+ (verifDiagonalDescIzq matriz fila columna) (verifDiagonalDescDer matriz fila columna)))))

(define (verifDiagonalDescIzq matriz fila columna)
  (cond ((null? (elementoMatriz matriz  (+ fila 1) (- columna 1)))
         0)
        ((equal? (elementoMatriz matriz (+ fila 1) (- columna 1)) (elementoMatriz matriz fila columna))
         (+ 1 (verifDiagonalDescIzq matriz (+ fila 1) (- columna 1))))
        (else
         0)))

(define (verifDiagonalDescDer matriz fila columna)
  (cond ((null? (elementoMatriz matriz  (- fila 1) (+ columna 1)))
         0)
        ((equal? (elementoMatriz matriz (- fila 1) (+ columna 1)) (elementoMatriz matriz fila columna))
         (+ 1 (verifDiagonalDescDer matriz (- fila 1) (+ columna 1))))
        (else
         0)))