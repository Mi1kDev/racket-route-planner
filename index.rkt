#lang racket/gui

(require "route-find.rkt")
(require json)

(define fake-screen% (class vertical-panel%
    (super-new)
))

(define mainmenu% (class vertical-panel%
    (super-new)
    (init-field (route (new route-finder%)))
    (init-field (found-routes #f))
    (init-field (currentStart ""))
    (init-field (currentDest ""))
    (init-field (filters (make-vector 3 #f)))
    (init-field (accessiblity #f))
    (init-field (dummyData
        (list
            (list "12:20" "12:45" "13:30")
            (list "10 min" "23 min" "35 min")
            (list "Leicester Square - King's Cross - Holloway Road" "Waterloo - Leicester - Tottenham" "Bank - Elephant&Castle - Kennington")
        )
    ))
    (define/private accessiblityToggle (lambda (x)
        (cond
            [(not (equal? x accessiblity)) (set! accessiblity x)]
        )
    ))
    (define/private filterToggle (lambda (x pos)
        (cond
            [(not (equal? x (vector-ref filters pos)))(vector-set! filters pos x)]
        )
    ))
    (define/private findRoutes (lambda (s d)
        (cond
            [(not (and (equal? currentStart s) (equal? currentDest d))) (set! found-routes (send route run s d))]
        )
    ))
    (define/private displayMessage (lambda (stringName)
        (send error-bar set-label stringName)
    ))
    (define/public getRoutes (lambda()
        (let ((d (send sdbar getValue "des")) (s (send sdbar getValue "s")))
            (findRoutes s d)
            (cond
                [(and (equal? s currentStart) (equal? d currentDest))]
                [(and (not (equal? #f found-routes))) (send sr addChildren found-routes) (displayMessage "")]
                [#t (displayMessage "Something went wrong!")]
            )
            (set! currentStart s) 
            (set! currentDest d)
        )
    ))
    (define/private write-json-wrapper (lambda (jsexpr filename)
        (call-with-output-file filename (lambda (x) (write-json jsexpr x)) #:exists 'replace)
    ))
    (define/public save (lambda()
        (cond
            [(not (equal? found-routes #f))
                (let ((c (hash
                'start currentStart
                'end currentDest
                'routes found-routes
                'accessiblity accessiblity
                'filters  (hash 'bus (vector-ref filters 0) 'train (vector-ref filters 1) 'cab (vector-ref filters 2))
                )))
                    (write-json-wrapper c "save.json")
                )
            ]
        )
    ))
    (new button% [parent this] [label "Saved Routes"])
    (define timetable (new list-box%
        [label ""]
        [parent (new horizontal-panel% [parent this])]
        [choices (list)]
        [style (list 'single 'column-headers 'vertical-label 'variable-columns)]
        [min-height 100]
        [horiz-margin 50]
        [columns (list "Time" "Route Time" "Stops To Come")]
    ))
    (send/apply timetable set dummyData)
    (new button%[label "CHANGE DATA"][callback (lambda (o e)(send/apply timetable set (list (list "Apple"))))])
    (define start-destination-bar% (class horizontal-panel%
        (super-new)
        (define start (new text-field%[parent this][label ""][init-value ""]))
        (define destination (new text-field%[parent this][label ""][init-value ""]))
        (new message%[parent this][horiz-margin 50][label ""])
        (define arrival (new text-field%[parent this][label ""][init-value ""]))
        (define departure(new text-field%[parent this][label ""][init-value ""]))

        (define/public getValue (lambda (x)
            (cond
                [(equal? x "s") (send start get-value)]
                [(equal? x "a") (send arrival get-value)]
                [(equal? x "des") (send destination get-value)]
                [(equal? x "dep") (send departure get-value)]
            )
        ))
    ))
    (define sdbar (new start-destination-bar%[parent this][horiz-margin 50][vert-margin 10]))
    (define filters-buttons% (class horizontal-panel%
        (super-new)
        (define bus(new check-box%[parent this][label "Bus"][callback (lambda(o e)(filterToggle (send bus get-value) 0))]))
        (define train(new check-box%[parent this][label "Train"][callback (lambda(o e)(filterToggle (send train get-value) 1))]))
        (define cab(new check-box%[parent this][label "Cab"][callback (lambda(o e)(filterToggle (send cab get-value) 2))]))
        (new message%[parent this][horiz-margin 10][label ""])
        (define accessiblityBtn(new check-box%[parent this][label "Wheelchair Accessible"][callback (lambda (o e) (accessiblityToggle (send accessiblityBtn get-value)))]))
        (new message%[parent this][horiz-margin 10][label ""])
        (define save-search% (class vertical-panel%
            (super-new)
            (define searchButton (new button% [parent this][label "Search"][callback (lambda (o e) (search))]))
            (define saveButton (new button%[parent this][label "Save"][callback (lambda (o e) (createSave))]))
            (define/private createSave (lambda ()
                (send (send (send this get-parent) get-parent) save)
            ))
            (define/private search (lambda ()
                (send (send (send this get-parent) get-parent) getRoutes)
            ))
        ))
        (new save-search% [parent this])
    ))
    (define fb (new filters-buttons% [parent this][horiz-margin 50]))
    (define search-results% (class vertical-panel%
        (super-new)
        (define/public addChildren (lambda (results)
            (clearAll)
            (for ([i results])
                (new button% [label i] [parent this])
            )
        ))
        (define/private clearAll (lambda ()
            (for ([child (send this get-children)])
                (send this delete-child child)
            )
        ))
    ))
    (define sr (new search-results% [parent this][style (list 'border 'vscroll)][min-height 100][vert-margin 10][horiz-margin 50]))
    (define error-bar (new message% [parent this][label ""][auto-resize #t]))
    (new button% [label "Switch"][parent this][callback (lambda (o e)(send (send this get-parent) switchScreens fake-screen%))])
))

(define parent% (class frame%
    (super-new)
    (init-field (screen mainmenu%))
    (define/private clearScreen (lambda ()
        (for ([child (send this get-children)])
            (send this delete-child child)
        )
    ))
    (define/public switchScreens (lambda (newScreen)
        (cond
            [(equal? screen newScreen)]
            [#t (clearScreen) (set! screen newScreen) (new newScreen [parent this])]
        )
    ))
))

(define root (new parent%
    [label "Route Planner"]
    [width 400]
    [height 600]
    [stretchable-height #f][stretchable-width #f]
))

(define y (new mainmenu% [parent root][vert-margin 50]))
(send root show #t)