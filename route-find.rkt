#lang racket/gui

(define network-names '("euston" "tottenham court road" "leicester square" "green park" "waterloo" "kennington" "elephant and castle" "bank" "king's cross st. pancras" "hollow way road"))
(define network-structure '(
    #|0: Euston|#(1 8)
    #|1: Tottenham|#(0 2)
    #|2: Leicester Square|#(1 3 4 8)
    #|3: Green Park|#(2)
    #|4: Waterloo|#(2 5)
    #|5: Kennington|#(4 6)
    #|6: Elephant and Castle|#(5 7)
    #|7: Bank|#(6 8)
    #|8: King's Cross St. Pancras|#(0 2 7 9)
    #|9: Holloway Road|#(8)
))

(define route-finder% (class object%
    (super-new)
    (init-field (v 10))
    (init-field (names network-names))
    (init-field (graph network-structure))
    (init-field (stringPath ""))
    (init-field (convertedPath '()))
    (init-field (returnPath '()))

    (define/private populate (lambda(x y)
        (for/list ([i x])y)
    ))

    (define/private getAllPaths (lambda (s d visited path)
        (set! visited (list-set visited s #t))
        (set! path (append path (list s)))
        (cond
            [(equal? s d) (set! returnPath (cons path returnPath))]
            [#t 
                (for ([i (list-ref graph s)])
                    (cond
                        [(equal? (list-ref visited i) #f) (getAllPaths i d visited path)]
                    )
                )
                (set! path (remove (last path) path))
                (set! visited (list-set visited s #f))
            ]
        )
    ))

    (define/public printReturnPaths (lambda ()
        (let ((newPath '()))
            (cond
                [(empty? returnPath) "There is no path between these stations."]
                [#t (for ([i (in-range (length returnPath))])
                    (for ([j (in-range (length (list-ref returnPath i)))])
                        (cond
                            [(not (equal? (list-ref (list-ref returnPath i) j) (last (list-ref returnPath i)))) (set! stringPath (string-append stringPath (list-ref names (list-ref (list-ref returnPath i) j)) " > "))]
                            [else (set! stringPath (string-append stringPath (list-ref names (list-ref (list-ref returnPath i) j))))]
                        )
                    )
                    (set! newPath (cons stringPath newPath))
                    (set! stringPath "")
                    
                )(set! returnPath '())newPath]
            )
        )
    ))

    (define/private isMember? (lambda (str strs) (ormap [lambda (s) (string=? s str)] strs)))

    (define/public run (lambda (s d)
        (let ((visited (populate v #f)) (path '()))
            (cond
                [(and (isMember? (string-downcase s) names)(isMember? (string-downcase d) names))
                    (getAllPaths (index-of names (string-downcase s)) (index-of names (string-downcase d)) visited path) (send this printReturnPaths)]
                [#t #f]
            )
        )
    ))
))

;(define runner (new route-finder%))
;(send runner run "Euston" "Tottenham Court Road")

(provide route-finder%)