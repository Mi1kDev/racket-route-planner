#lang racket

(define v 10)

(define network '("Euston" "Tottenham Court Road" "Leicester Square" "Green Park" "Waterloo" "Kennington" "Elephant and Castle" "Bank" "King's Cross St.Pancras" "Holloway Road"))

(define graph '(
    (1 2 3)
    (3)
    (0 1)
    ()
))

(define network-graph'(
    #|Euston|# (1 8)
    #|Tottenham Court Road|# (0 2)
    #|Leicester Square|# (1 3 4 8)
    #|Greenpark|# (2)
    #|Waterloo|# (2 5)
    #|Kennington|# (4 6)
    #|Elephant and Castle|# (5 7)
    #|Bank|# (6 8)
    #|King's Cross St. Pancras|# (0 2 9)
    #|Holloway Road|# (8)
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
        (printAllPathsUtil s d visited path network-graph)
    )
))

(printAllPaths (index-of network "Euston") (index-of network "King's Cross St.Pancras"))
(for ([i fp])
    (for([j i])
        (print (list-ref network j))
    )
    (println "")
)


