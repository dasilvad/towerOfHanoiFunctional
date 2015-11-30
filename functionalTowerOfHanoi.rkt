;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname functionalTowerOfHanoi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;;developed by Daniel Silva
;
;bibliotecas
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

;dimensões da cena
(define WIDTH 600)
(define HEIGHT 400)

(define QTDE-DISCO 4)
(define ALTURA-DISCO 15)

;coordenadas para desenhar os pinos
(define COORDENADA-Y-INICIAL-PINO 250)
(define POSN-ORIGEM (make-posn 180 COORDENADA-Y-INICIAL-PINO))
(define POSN-MEIO (make-posn 280 COORDENADA-Y-INICIAL-PINO))
(define POSN-DESTINO (make-posn 380 COORDENADA-Y-INICIAL-PINO))

;Dimensões do pino
(define ALTURA-PINO 150)
(define LARGURA-PINO 20)
(define PINO (rectangle LARGURA-PINO ALTURA-PINO "solid" "blue"))
 
;diferença do disco 1 para o disco 2... 
(define DIFERENCA-TAMANHO-DISCO 20)


;estrutura de dados
(define-struct world (solucao pinoOrigem pinoMeio pinoDestino))

;world é um de:
  ;(make-world string listaDeDiscos listaDeDiscos listaDeDiscos)
  ;empty
;solucao e uma string com a solucao da torre de hanoi:
  ;formato:
  ;"pino1->pino2 pino2->3 ..."

;pinoOrigem armazena uma lista de discos. Ela é uma de:
  ;(cons (make-posn  tamanho-inicial ALTURA-DISCO) pinoOrigem)
  ;empty

;pinoMeio armazena uma lista de discos. Ela é uma de:
  ;(cons (make-posn  tamanho-inicial ALTURA-DISCO) pinoMeio)
  ;empty

;pinoDestino armazena uma lista de discos. Ela é uma de:
  ;(cons (make-posn  tamanho-inicial ALTURA-DISCO) pinoDestino)
  ;empty


;desenha os pinos e os discos
;é chamada toda vez que o timer estourar
;world -> image  
(define (render w) 
  (cond
     [(world? w) (desenharDisco (world-pinoOrigem w) "origem" (getCoordenadaYInicinal)
                                (desenharDisco (world-pinoMeio w) "meio"  (getCoordenadaYInicinal)
                                (desenharDisco (world-pinoDestino w) "destino"  (getCoordenadaYInicinal) (desenharPino)))) ] ;(listaDisco pino alturaInicial scene)
     [ else (place-image  (text "NOT IMPLEMENTED YET" 24 "olive") 90 340  (empty-scene WIDTH HEIGHT))]
  )
)

;retorna a coordenada y inicial quer será usada para desenhar o disco da base no pino.
;none -> number
(define (getCoordenadaYInicinal)
  ( - (+ COORDENADA-Y-INICIAL-PINO (/ ALTURA-PINO 2)) (/ ALTURA-DISCO  2))
 )


;desenha os 3 pinos e retorna uma cena que é usada para desenhar os discos
;none -> scene
(define (desenharPino)
     
    (place-image PINO (posn-x POSN-ORIGEM) (posn-y POSN-ORIGEM) 
                 (place-image PINO (posn-x POSN-MEIO) (posn-y POSN-MEIO)
                              (place-image PINO (posn-x POSN-DESTINO) (posn-y POSN-DESTINO) (empty-scene WIDTH HEIGHT))))
   
  
)
;move um disco do pino1 para o pino2
;world string string -> world
(define (moverDisco  world pino1 pino2)
  (cond 
    [(string=? pino1 "origem")
      (cond 
        [(string=? pino2 "meio") (tirarDisco(make-world (world-solucao world) (world-pinoOrigem world) (colocarDisco(world-pinoOrigem world) (world-pinoMeio world)) (world-pinoDestino world)) "origem")]
        [(string=? pino2 "destino") (tirarDisco(make-world (world-solucao world) (world-pinoOrigem world) (world-pinoMeio world)  (colocarDisco(world-pinoOrigem world) (world-pinoDestino world))) "origem")]
      ) 
    ]  
    [(string=? pino1 "meio")
      (cond
      [(string=? pino2 "origem") (tirarDisco(make-world (world-solucao world) (colocarDisco(world-pinoMeio world) (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world)) "meio")]
      [(string=? pino2 "destino") (tirarDisco(make-world (world-solucao world) (world-pinoOrigem world) (world-pinoMeio world)  (colocarDisco(world-pinoMeio world) (world-pinoDestino world))) "meio")]
      ) 
    ]
    [(string=? pino1 "destino")
      (cond
        [(string=? pino2 "origem") (tirarDisco(make-world (world-solucao world) (colocarDisco(world-pinoDestino world) (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world)) "destino")]
        [(string=? pino2 "meio") (tirarDisco(make-world  (world-solucao world) (world-pinoOrigem world) (colocarDisco(world-pinoDestino world) (world-pinoMeio world)) (world-pinoDestino world)) "destino")]
      )
    ] 
    ))

;coloca o disco da lista de origem na lista de Destino; 
;função helper de moverDisco do pino1 para o pino2
;listDeDiscos listaDeDiscos -> listaDeDiscos
(define (colocarDisco listaDiscoOrigem listaDiscoDestino)
  (cond 
    [(empty? listaDiscoOrigem) listaDiscoDestino]
    [(empty? (rest listaDiscoOrigem)) (append listaDiscoDestino (list (first listaDiscoOrigem)) )]
    [else (colocarDisco(rest listaDiscoOrigem) listaDiscoDestino)]
  )  
)

;retira os último disco da lista "listaDisco"
;listaDeDiscos -> listaDeDiscos
(define (tirarDiscoHelper  listaDisco)
  (cond
   [(empty? listaDisco) empty]
   [else (cond
           [(empty? (rest listaDisco)) empty]  
           [else (cons (first listaDisco) (tirarDiscoHelper  (rest listaDisco))) ]
 ) ]
 )
  )

;retira um disco do pino "pino" da lista de discos de world
;world -> world
(define (tirarDisco world pino )
  (cond
     [(string=? pino "origem") (make-world (world-solucao world) (tirarDiscoHelper  (world-pinoOrigem world)) (world-pinoMeio world) (world-pinoDestino world))]
     [(string=? pino "meio")(make-world  (world-solucao world) (world-pinoOrigem world) (tirarDiscoHelper  (world-pinoMeio world)) (world-pinoDestino world))]
     [(string=? pino "destino") (make-world (world-solucao world) (world-pinoOrigem world)  (world-pinoMeio world) (tirarDiscoHelper  (world-pinoDestino world)))]
  )
 
 )
 
;Desenha os discos no pino "pino"
 ;listaDeDiscos string number scene -> scene
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

;Recebe a coordenada (x, y) e retorna  um retangulo que representa o disco a ser desenhado
;posn -> rectangle
(define (getDisco posicao)
     (rectangle (posn-x posicao) (posn-y posicao) "solid" "red")
)


;------------------------------------------------------------------------------------------------

;formata uma lista contendo a solucao
;formato exemplo:
;  converte (list "origem->destino") para (list "origem" "destino")
;list -> list
(define (formatarSolucao listaSolucao)
  (cond 
     [(empty? listaSolucao) empty] 
     [else 
        (string-split (string-replace (first listaSolucao) "->" " "))
     ]
  )  
)

;resolve a torre de hanoi
 ;numero minimo de movimentos: (2^n) -1
;numero string string string -> string
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
;remove um elemento da lista de solucao, depois que o movimento do disco é executado
(define (removerMovimentoDalistaSolucao world)
   (make-world (rest (world-solucao world)) (world-pinoOrigem world) (world-pinoMeio world) (world-pinoDestino world))
 )
  
;funcao chamada quando o timer estoura
;world -> world
(define (tick world) 
  
    (cond  
       [(empty? (world-solucao world)) world]
         
       [else
           (removerMovimentoDalistaSolucao (moverDisco  world (getOrigem world) (getDestino world))) 
       ] 
     )
    
 )   

;cria os discos no pino Origem
;number number -> listOfDiscs
(define (criarConjuntoDiscos qtde tamanho-inicial)
 (cond 
  [(< qtde 1) empty]
  [else
    (cons (make-posn  tamanho-inicial ALTURA-DISCO) (criarConjuntoDiscos (sub1 qtde) (- tamanho-inicial DIFERENCA-TAMANHO-DISCO) )) 
  ]
 )  
)
;retorna o tamanho do maior disco. Ele é usado para calcular o tamanho dos demais discos
;none -> number
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

; world function function function -> world
(big-bang sample-world
          [to-draw render] 
          [on-tick tick 1/2]
          [stop-when stop]
          
)
 