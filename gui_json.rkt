#lang racket/gui
(require json)

(define root (new frame% [label "JSON SAVE"]))

(define x "aPPLE")

(define saveScreen%
    (class vertical-panel%
        (init-field (name ""))
        (init-field (dob ""))
        (init-field (filename ""))
        (init-field (data ""))
        (super-new)
        (define name-input (new text-field%
            [parent this]
            [label "Name"]
            [init-value ""]
            [callback (lambda(o e)(set! name (send name-input get-value)))]
        ))
        (define dob-input (new text-field%
            [parent this]
            [label "DOB"]
            [init-value ""]
            [callback (lambda(o e)(set! dob (send dob-input get-value)))]
        ))
        (define file-input (new text-field%
            [parent this]
            [label "Filename"]
            [init-value ""]
            [callback (lambda(o e)(set! filename (send file-input get-value)))]
        ))
        (define save (new button%
            [parent this]
            [label "Save to JSON"]
            [callback (lambda(o e)
                (set! data #hasheq((fname . (bytes->jsexpr name)) (dobb . (bytes->jsexpr dob))))
                (send this write-json-wrapper data filename)
            )]
        ))

        (define/public write-json-wrapper (lambda (jsexpr filename)
        (call-with-output-file filename (lambda (x) (write-json jsexpr x)) #:exists 'replace)))  
    )
)

(define ss (new saveScreen%[parent root]))
(send root show #t)