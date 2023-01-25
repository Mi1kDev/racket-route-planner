#lang racket/gui

(define root (new frame%
    [label "Journey Planner"]
))

(define saveScreen%
    (class vertical-panel%
        (super-new)
        (define messageBox (new text-field%
            [parent this]
            [label "Enter"]
            [init-value ""]
        ))
        (define fileOutput (new message%
            [parent this]
            [label ""]
            [auto-resize #t]
        ))
        (define save (new button%
            [parent this]
            [label "Save to Disk"]
            [callback (lambda (o e)(send this saveToDisk (send messageBox get-value) "SavedRoutes.txt"))]
        ))
        (define load (new button%
            [parent this]
            [label "Load from Disk"]
            [callback (lambda(o e) (send fileOutput set-label (send this readFromDisk "SavedRoutes.txt")))]
        ))
        (define/public saveToDisk (lambda (x y)
            (call-with-output-file y #:exists 'truncate (lambda (out)(display x out)))
        ))
        (define/public (readFromDisk x)
            (call-with-input-file "SavedRoutes.txt" (lambda (in) (port->string in)))
        )
    )
)

(define ss (new saveScreen%[parent root]))
(send root show #t)