#lang racket

(define v 4)

(define network '("Euston" "Hendon" "Tottenham" "Brent"))

(define graph '(
    (1 2 3)
    (3)
    (0 1)
    ()
))

(define fp '())


(define printAllPathsUtil (lambda (s d visited path graph)
    (set! visited (list-set visited s #t))
    (set! path (append path (list s)))
    (cond
        [(equal? s d) 
            ;(set! path (remove (list-ref path s) path))
            (set! fp (cons path fp))
        ]
        [#t 
            (for ([i (list-ref graph s)])
                
                (cond
                    [(equal? (list-ref visited i) #f)(printAllPathsUtil i d visited path graph)]
                )
            )
            (set path (remove (last path) path))
            (set! visited (list-set visited s #f))
        ]
    )
))
(define popList (lambda(x)
    (for/list ([i x])
        #f
    )
))


(define printAllPaths (lambda (s d)
    (let ((visited (popList v)) (path '()))
        (printAllPathsUtil s d visited path graph)
    )
))

(printAllPaths 2 3)
(for ([i fp])
    (for([j i])
        (print (list-ref network j))
    )
)

