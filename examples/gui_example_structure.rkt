#lang racket/gui

;first we create a frame
(define root (new frame% [label "This is a Window"]))

;we make our own custom class that extends from vertical-panel (I used vertical-panel just because it orders stuff nicely)
(define loginScreen% (class vertical-panel%
    (super-new)
    ;inside our new class we can actually create other widgets like buttons, text input etc.
    (define login (new text-field%
        ;we set their parent to "this" because "this" refers to our class which is a vertical-panel. It is essentially the same as parenting the text-field to any vertical-panel
        [parent this]
        [label "Username"]
    ))
    ;we create another widget and parent  it to this as well
    (define password (new text-field%
        [parent this]
        [label "Password"]
    ))
))

;if we wanted to make a completely new screen then we can group it another class like this so we don't have just a bunch of widgets

(define screen1 (new loginScreen%
    [parent root]
))

(send root show #t)