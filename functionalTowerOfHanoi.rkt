;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname functionalTowerOfHanoi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;;developed by Daniel Silva
;
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

 
(define WIDTH 600)
(define HEIGHT 400)

(define QTDE-DISCO 4)
(define ALTURA-DISCO 15)
(define ALTURA-PINO 150)
(define LARGURA-PINO 20)

(define PINO (rectangle LARGURA-PINO ALTURA-PINO "solid" "blue"))
(define DIFERENCA-TAMANHO-DISCO 20)
(define COORDENADA-Y-INICIAL-PINO 250)
(define POSN-ORIGEM (make-posn 180 COORDENADA-Y-INICIAL-PINO))
(define POSN-MEIO (make-posn 280 COORDENADA-Y-INICIAL-PINO))
(define POSN-DESTINO (make-posn 380 COORDENADA-Y-INICIAL-PINO))


(define-struct world (solucao pinoOrigem pinoMeio pinoDestino))

;pinoOrigem armazena o tamanha do pino, tamanho do retangulo:
;(make-posn number number)
;empty
 ;a altura do disco no pino depende da posicao dele na lista pinoOrigem, PinoMeio e PinoDestino

;solucao e uma lista com a solucao da torre de hanoi
;formato:
  ;(list origem->destino origem->meio ...)
  
(define (render w) 
  (cond
     [(world? w) (desenharDisco (world-pinoOrigem w) "origem" (getCoordenadaYInicinal)
                                (desenharDisco (world-pinoMeio w) "meio"  (getCoordenadaYInicinal)
                                (desenharDisco (world-pinoDestino w) "destino"  (getCoordenadaYInicinal) (desenharPino)))) ] ;(listaDisco pino alturaInicial scene)
     [ else (place-image  (text "NOT IMPLEMENTED YET" 24 "olive") 90 340  (empty-scene WIDTH HEIGHT))]
  )
)

(define (getCoordenadaYInicinal)
  ( - (+ COORDENADA-Y-INICIAL-PINO (/ ALTURA-PINO 2)) (/ ALTURA-DISCO  2))
 )
;desenha os 3 pinos
;none -> scene
(define (desenharPino)
     
    (place-image PINO (posn-x POSN-ORIGEM) (posn-y POSN-ORIGEM) 
                 (place-image PINO (posn-x POSN-MEIO) (posn-y POSN-MEIO)
                              (place-image PINO (posn-x POSN-DESTINO) (posn-y POSN-DESTINO) (empty-scene WIDTH HEIGHT))))
   
  
)
;world string string -> world
(define (moverPino world pino1 pino2)
  (cond 
    [(string=? pino1 "origem")
      (cond 
        [(string=? pino2 "meio") (tirarPino (make-world (world-solucao world) (world-pinoOrigem world) (colocarPino (world-pinoOrigem world) (world-pinoMeio world)) (world-pinoDestino world)) "origem")]
        [(string=? pino2 "destino") (tirarPino (make-world (world-solucao world) (world-pinoOrigem world) (world-pinoMeio world)  (colocarPino (world-pinoOrigem world) (world-pinoDestino world))) "origem")]
      ) 
    ]  
    [(string=? pino1 "meio")
      (cond
      [(string=? pino2 "origem") (tirarPino (make-world (world-solucao world) (colocarPino (world-pinoMeio world) (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world)) "meio")]
      [(string=? pino2 "destino") (tirarPino (make-world (world-solucao world) (world-pinoOrigem world) (world-pinoMeio world)  (colocarPino (world-pinoMeio world) (world-pinoDestino world))) "meio")]
      ) 
    ]
    [(string=? pino1 "destino")
      (cond
        [(string=? pino2 "origem") (tirarPino (make-world (world-solucao world) (colocarPino (world-pinoDestino world) (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world)) "destino")]
        [(string=? pino2 "meio") (tirarPino (make-world  (world-solucao world) (world-pinoOrigem world) (colocarPino (world-pinoDestino world) (world-pinoMeio world)) (world-pinoDestino world)) "destino")]
      )
    ] 
    ))


(define (colocarPino listaDiscoOrigem listaDiscoDestino)
  (cond 
    [(empty? listaDiscoOrigem) listaDiscoDestino]
    [(empty? (rest listaDiscoOrigem)) (append listaDiscoDestino (list (first listaDiscoOrigem)) )]
    [else (colocarPino (rest listaDiscoOrigem) listaDiscoDestino)]
  )  
)
;listaDeDiscos -> listaDeDiscos
(define (tirarPinoHelper listaDisco)
  (cond
   [(empty? listaDisco) empty]
   [else (cond
           [(empty? (rest listaDisco)) empty]  
           [else (cons (first listaDisco) (tirarPinoHelper (rest listaDisco))) ]
 ) ]
 )
  )

;world -> world
(define (tirarPino world pino )
  (cond
     [(string=? pino "origem") (make-world (world-solucao world) (tirarPinoHelper (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world))]
     [(string=? pino "meio")(make-world  (world-solucao world) (world-pinoOrigem world) (tirarPinoHelper (world-pinoMeio world)) (world-pinoDestino world))]
     [(string=? pino "destino") (make-world (world-solucao world) (world-pinoOrigem world)  (world-pinoMeio world) (tirarPinoHelper (world-pinoDestino world)))]
  )
 
 )
 
 
(define (desenharDisco listaDisco pino alturaInicial scene)
  (cond
   [(string=? pino "origem") 
      (cond
       [(empty? listaDisco) scene]
       [else (place-image (getDisco (first listaDisco)) (posn-x POSN-ORIGEM) alturaInicial (desenharDisco (rest listaDisco) pino (- alturaInicial ALTURA-DISCO) scene))]
      )
   ]
   [(string=? pino "meio") 
     (cond
       [(empty? listaDisco) scene]
       [else  (place-image (getDisco (first listaDisco)) (posn-x POSN-MEIO) alturaInicial (desenharDisco (rest listaDisco) pino (- alturaInicial ALTURA-DISCO) scene))]
      )
    ]
   [(string=? pino "destino")
    (cond
       [(empty? listaDisco) scene]
       [else (place-image (getDisco (first listaDisco)) (posn-x POSN-DESTINO) alturaInicial ( desenharDisco (rest listaDisco) pino (- alturaInicial ALTURA-DISCO) scene))]
      )
    ]
 ) 
)

;retorna  um disco de um tamanho especifico
(define (getDisco posicao)
     (rectangle (posn-x posicao) (posn-y posicao) "solid" "red")
)


;------------------------------------------------------------------------------------------------


;list -> list
;formato exemplo:
;  converte (list "origem->destino") para (list "origem" "destino")
(define (formatarSolucao listaSolucao)
  (cond 
     [(empty? listaSolucao) empty] 
     [else 
        (string-split (string-replace (first listaSolucao) "->" " "))
     ]
  )  
)

;numero minimo de movimentos: (2^n) -1
(define (torre-de-hanoi n origem destino meio)
  (cond [(= n 1)
           (string-append origem "->" destino)
        ]
        [else  
           (string-append  (torre-de-hanoi  (sub1 n) origem meio destino)" " origem "->" destino " " (torre-de-hanoi  (sub1 n) meio destino origem)) 
       ]
   ) 
 )


;----------------------------------------------------------------------------

;world -> string
;retorna a origem do movimento do disco da lista de solucao do world
(define (getOrigem world)
    (first (formatarSolucao (world-solucao world)))
  )
; world -> string
;retorna o destino do movimento do disco da lista de solucao do world
(define (getDestino world)
  (first (rest (formatarSolucao  (world-solucao world))) )
)

;world -> world
;remove um elemento da lista de solucao, depois que o movimento do disco e executado
(define (removerMovimentoDalistaSolucao world)
   (make-world (rest (world-solucao world)) (world-pinoOrigem world) (world-pinoMeio world) (world-pinoDestino world))
 )
  
;timer
(define (tick world) 
  
    (cond  
       [(empty? (world-solucao world)) world]
         
       [else
           (removerMovimentoDalistaSolucao (moverPino world (getOrigem world) (getDestino world))) 
       ] 
     )
    
 )   

(define (criarConjuntoDiscos qtde tamanho-inicial)
 (cond 
  [(< qtde 1) empty]
  [else
    (cons (make-posn  tamanho-inicial ALTURA-DISCO) (criarConjuntoDiscos (sub1 qtde) (- tamanho-inicial DIFERENCA-TAMANHO-DISCO) )) 
  ]
 )  
)

(define (getTamanhoDiscoInicial)
(+ (* QTDE-DISCO DIFERENCA-TAMANHO-DISCO) 20)  
)
(define sample-world (make-world  (string-split (torre-de-hanoi  QTDE-DISCO "origem" "destino" "meio")) (criarConjuntoDiscos QTDE-DISCO (getTamanhoDiscoInicial)) empty empty))
 
(define (stop world)
  (cond
   [(empty? (world-solucao world)) true]
   [else false]
  )  
)

(big-bang sample-world
          [to-draw render] 
          [on-tick tick 1/2]
          [stop-when stop]
          
)
 