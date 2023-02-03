#lang racket
(require json)

(define x "hendon")
(define b #hasheq(
    (start_location . "hendon")
    (end_location . "tottenham")
    (ETA . 30)
    (route . ("g"))
    (optional_parameters . #hasheq((accessibility . #f)))
))

(define c (make-hasheq 
    `((start. , x)
        (end., x)
    )
))

(define d (hash 'start x))


;(display a)
;(hash-ref a "start_location")
#|
(with-output-to-string 
    (lambda () (write-json b))
)|#

(define write-json-wrapper (lambda (jsexpr filename)
    (call-with-output-file filename (lambda (x) (write-json jsexpr x)) #:exists 'replace)
))  

(define read-json-wrapper (lambda (filename)
    (call-with-input-file filename read-json)
))

(write-json-wrapper d "save.json")
;(read-json-wrapper "save.json")