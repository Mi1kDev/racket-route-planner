#lang racket/gui

(define screen (new frame%
    [label "Client"]
    [height 400]
    [width 300]
))

(define connectScreen (new vertical-panel%
    [parent screen]
))

(define ip (new text-field%
    [parent connectScreen]
    [label "IP Address"]
    [init-value ""]
    [horiz-margin 10]
))

(define port (new text-field%
    [parent connectScreen]
    [label "Port No."]
    [init-value ""]
    [horiz-margin 10]
))

(define connectButton (new button%
    [parent connectScreen]
    [label "Connect"]
    [callback (lambda (o e)
        (send screen delete-child connectScreen)
        (send screen add-child messageScreen
    
    ))]
))

(define messageScreen (new vertical-panel%
    [parent screen]
))

(define messageScreenTitle (new message%
    [parent messageScreen]
    [label "You are now messaging..."]
))

(define messageTextBox (new text-field%
    [parent messageScreen]
    [label "Message"]
    [min-height 200]
    [init-value ""]
))

(define sendMessage (new button%
    [parent messageScreen]
    [label "Send"]
))

(define backButton (new button%
    [parent messageScreen]
    [label "<-"]
    [callback (lambda (o e) (send screen delete-child messageScreen) (send screen add-child connectScreen))]
))

(send screen delete-child messageScreen)
(send screen show #t)
