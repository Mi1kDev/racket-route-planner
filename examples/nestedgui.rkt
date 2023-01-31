#lang racket/gui

(define root (new frame% [label "Window 1"]))

(define start-destination% (class horizontal-panel%
    (super-new)
    (define n (new message% [label "Message 1"] [parent this]))
    (define m (new message% [label "Message 2"] [parent this]))
    (define o (new message% [label "Message 3"] [parent this]))
))

(define screen1% (class vertical-panel%
    (super-new)
    (new button% [label "Button 1"] [parent this])
    (new start-destination% [parent this])
    (new button% [label "Button 2"] [parent this])
))

(new screen1%[parent root])

(send root show #t)