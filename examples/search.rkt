#lang racket/gui

(define root (new frame%
    [label "Search"]
))

(define input (new text-field%
    [parent root]
    [label ""]
    [init-value ""]
    [callback (lambda (o e) (search (send input get-value)))]
))

(define outputBox (new message%
    [parent root]
    [label ""]
    [auto-resize #t]
))

(define search (lambda (x)
    (cond
        [(equal? x "")]
        [(string-contains? "apple" (string-downcase x)) (send outputBox set-label "apple")]
        [#t (send outputBox set-label "")]
    )
))
(send root show #t)