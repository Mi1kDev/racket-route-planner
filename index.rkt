#lang racket/gui

(require "route-find.rkt")
(require json)

(define mainmenu% (class vertical-panel%
    (super-new)
    (init-field (route (new route-finder%)))
    (init-field (found-routes #f))
    (init-field (currentStart ""))
    (init-field (currentDest ""))
    (init-field (filters (make-vector 3 #f)))
    (init-field (accessiblity #f))
    (init-field (arrive? ""))
    (init-field (timeToPass ""))
    (init-field (trainData
        (list
            (list "12:20" "12:45" "13:30")
            (list "10 min" "23 min" "35 min")
            (list "Leicester Square - King's Cross - Holloway Road" "Waterloo - Leicester - Tottenham" "Bank - Elephant&Castle - Kennington")
        )
    ))
    (init-field (busData
        (list
            (list "12:30" "12:35" "1:00")
            (list "10 min" "15 min" "40 min")
            (list "192" "196" "183")
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
            (cond
                [(equal? (string-downcase s) (string-downcase d))]
                [#t (findRoutes s d)
                    (cond
                        [(and (equal? s currentStart) (equal? d currentDest))]
                        [(and (not (equal? #f found-routes))) (send sr addChildren found-routes) (displayMessage "")]
                        [#t (displayMessage "Something went wrong!")]
                    )
                    (set! currentStart s) 
                    (set! currentDest d)]
            )
        )
    ))
    (define/private arriveDepartureHandle (lambda (type otherField)
        (cond
            [(equal? type "a") (set! arrive? #t)
                (cond
                    [(not (equal? (send otherField get-value) "")) (send otherField set-value "")]
                )
            ]
            [(equal? type "d") (set! arrive? #f)
                (cond
                    [(not (equal? (send otherField get-value) "")) (send otherField set-value "")]
                )
            ]
        )
    ))
    (define/private switchDisplayedData (lambda (currentData)
        (cond
            [(equal? currentData "See Bus Times")(send/apply timetable set busData)(send data-button set-label "See Train Times")]
            [(equal? currentData "See Train Times")(send/apply timetable set trainData)(send data-button set-label "See Bus Times")]
        )
    ))
    (new button% [parent this] [label "Saved Routes"][callback (lambda (o e) (send (send this get-parent) switchScreens savedRoutesPage% '()))])
    (define timetable (new list-box%
        [label ""]
        [parent (new horizontal-panel% [parent this])]
        [choices (list)]
        [style (list 'single 'column-headers 'vertical-label 'variable-columns)]
        [min-height 100]
        [horiz-margin 50]
        [columns (list "Time" "Route Time" "Stops To Come")]
    ))
    (send/apply timetable set trainData)
    (define data-button (new button%[label "See Bus Times"][parent this][callback (lambda (o e)(switchDisplayedData (send data-button get-label)))]))
    (define start-destination-bar% (class horizontal-panel%
        (super-new)
        (define start (new text-field%[parent this][label ""][init-value ""]))
        (define destination (new text-field%[parent this][label ""][init-value ""]))
        (new message%[parent this][horiz-margin 50][label ""])
        (define arrival (new text-field%[parent this][label "Arrive By:"][init-value ""][callback (lambda (o e)(arriveDepartureHandle "a" departure))]))
        (define departure(new text-field%[parent this][label "Depart At:"][init-value ""][callback (lambda(o e)(arriveDepartureHandle "d" arrival))]))
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
            (define/private search (lambda ()
                (send (send (send this get-parent) get-parent) getRoutes)
            ))
        ))
        (new save-search% [parent this])
    ))
    (define fb (new filters-buttons% [parent this][horiz-margin 50]))
    (define sort-button-bar% (class horizontal-panel%
        (super-new)
        (new button% [parent this][label "Sort Asc."][callback (lambda (o e) (send sr sortResults >))])
        (new button% [parent this][label "Sort Dsc."][callback (lambda (o e) (send sr sortResults <))])
    ))
    (new sort-button-bar% [parent this][horiz-margin 50])
    (define search-results% (class vertical-panel%
        (super-new)
        (define/private convertDistanceToTime (lambda (distance)
            (/ 20.5 distance)
        ))
        (define/private convertRouteStringToRoute (lambda (routeStr)
            (string-split routeStr " > ")
        ))
        (define/private buildPage (lambda(routeStr distance)
            (send (send (send this get-parent) get-parent) switchScreens routeInfoScreen% (hash 
                            'start currentStart
                            'dest currentDest
                            'route (convertRouteStringToRoute routeStr) 
                            'time (convertDistanceToTime distance)
                            'save #t))
        ))
        (define/public addChildren (lambda (results)
            (clearAll)
            (for/list ([i results])
                (cond 
                    [(> (string-length (first i)) 40) (new button% [label (string-append (string-titlecase (substring (first i) 0 39)) "... "(number->string(/ (round (* 100 (convertDistanceToTime (second i))))100)))] [parent this][callback (lambda (o e)(buildPage (first i) (second i)))])]
                    [#t (new button% [label (string-append (string-titlecase (first i)) (number->string(/ (round (* 100 (convertDistanceToTime (second i))))100)))] [parent this][callback (lambda (o e)(buildPage (first i) (second i)))])
                    ]
                )
            )
        )) 
        (define/public sortResults (lambda (proc)
            (cond
                [(equal? found-routes #f)]
                [(<= (length found-routes) 1)]
                [#t (clearAll)
                    (set! found-routes (sort found-routes proc #:key (lambda (x) (second x))))
                    (addChildren found-routes)
                ]
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
))

(define routeInfoScreen% (class vertical-panel%
    (super-new)
    (init-field (start ""))
    (init-field (destination ""))
    (init-field (route '()))
    (init-field (save #f))
    (init-field (timeTaken 0))
    (init-field (timePassedIn ""))
    (println save)
    (define hz% (class horizontal-panel%
        (super-new)
        (new message%[parent this][horiz-margin 50][label (string-upcase (string-append start "->" destination))])
    ))
    (new hz% [parent this])
    (define routeInformation% (class vertical-panel%
        (super-new)
        (for ([i route])
            (new message%[parent this][label (string-titlecase i)][auto-resize #t][min-width 350])
        )
    ))
    (new routeInformation%[parent this][style (list 'border 'vscroll)][min-height 30][horiz-margin 50])
    (define hzp% (class horizontal-panel%
        (super-new)
        (new message%[parent this][horiz-margin 50][label (string-append (number->string timeTaken) "hrs")])
    ))
    (new hzp% [parent this])
    (cond
        [(equal? save #t) (new button%[parent this][label "Save"][min-width 300][callback (lambda (o e)(send (send this get-parent) saveRoute start destination route timeTaken "save.json"))])]
    )
    (define hzpanel(new horizontal-panel%[parent this]))
    (new button%[parent hzpanel][label "Back"][horiz-margin 50][callback (lambda (o e)(send (send this get-parent) popScreen))])
)) 

(define savedRoutesPage% (class vertical-panel%
    (init-field (routeInfo '()))
    (super-new)
    (define/private loadData (lambda ()
        (let ((data (send (send this get-parent) loadRoutes "save.json")))
            (cond
                [(empty? data) (new message% [parent this][label "You have no Saved Routes"])]
                [#t (set! routeInfo data)]
            )
        ) 
    ))
    (define/private createRoutePage (lambda (start dest route timeTaken)
        (send (send this get-parent) switchScreens routeInfoScreen% (hash
            'start start
            'dest dest
            'route route
            'time timeTaken
            'save #f
        ))
    ))
    (define/private isInData? (lambda (search data)
        (ormap (lambda (d) (equal? search d)) data)
    ))
    (define/private deleteSavedRoute (lambda (data pArg)
        (cond
            [(isInData? data routeInfo) 
                (set! routeInfo(remove data routeInfo)) (clear pArg) (buildRoutes pArg routeInfo)
                (send (send this get-parent) saveRoutes routeInfo "save.json")
            ]
        )
    ))
    (define/private buildRoutes (lambda (parentArg routes)
        (cond
            [(empty? routeInfo)]
            [#t (for ([i routes])
                (define n (new horizontal-panel%[parent parentArg][alignment (list 'center 'center)]))
                (new button%[parent n][label (string-append (hash-ref i 'start) "->" (hash-ref i 'dest) " " (number->string(/ (round ( * 100 (hash-ref i 'time))) 100)))][callback (lambda (o e) (createRoutePage (hash-ref i 'start) (hash-ref i 'dest) (hash-ref i 'route) (hash-ref i 'time)))])
                (new button%[parent n][label "Delete"][callback (lambda (o e)(deleteSavedRoute i parentArg))])
            )]
        )
    ))
    (define/private clear (lambda (clearPanel)
        (for ([child (send clearPanel get-children)])
            (send clearPanel delete-child child)
        )
    ))
    (define/private sortRoutes (lambda (parentArg proc)
        (cond
            [(<= (length routeInfo) 1)]
            [#t (clear parentArg)
                (set! routeInfo (sort routeInfo proc #:key (lambda (x)(hash-ref x 'time))))
                (buildRoutes parentArg routeInfo)
            ]
        )
    ))
    (define/private buildButtons (lambda (hzPanel pArg)
        (new button%[parent hzPanel][label "Sort Asc."][callback (lambda (o e)(sortRoutes pArg <))])
        (new button%[parent hzPanel][label "Sort Dsc."][callback (lambda (o e)(sortRoutes pArg >))])
    ))
    (define/private search (lambda (textValue resultPanel)
        (let ((filteredResults (filter (lambda (x) (or (string-contains? (hash-ref x 'dest) (string-downcase textValue))(string-contains? (hash-ref x 'start) (string-downcase textValue)))) routeInfo)))
            (clear resultPanel)
            (buildRoutes resultPanel filteredResults)
        )
    ))
    
    (loadData)
    (cond
        [(empty? routeInfo)]
        [#t 
            (define searchBar (new text-field% [parent this][label "Search"][horiz-margin 50][callback (lambda (o e) (search (send searchBar get-value) vp))]))
            (define vp (new vertical-panel% [parent this][style (list 'border 'vscroll)][horiz-margin 50]))
            (buildButtons (new horizontal-panel%[parent this][horiz-margin 50]) vp)
            (buildRoutes vp routeInfo)
        ]
    )
    (new button%[parent this][label "Back"][callback (lambda (o e)(send (send this get-parent) popScreen))])
))

(define parent% (class frame%
    (super-new)
    (init-field (screen mainmenu%))
    (init-field (prevScreen ""))
    (define/private clearScreen (lambda ()
        (for ([child (send this get-children)])
            (send this delete-child child)
        )
    ))
    (define/private write-json-wrapper (lambda (jsexpr filename)
        (call-with-output-file filename (lambda (x) (write-json jsexpr x)) #:exists 'replace)
    ))
    (define/private read-json-wrapper (lambda (filename)
        (call-with-input-file filename read-json)
    ))
    (define/public loadRoutes (lambda (filename)
        (cond
            [(file-exists? filename) (read-json-wrapper filename)]
            [#t '()]
        )
    ))
    (define/public saveRoutes (lambda (routeInformation filename)
        (cond
            [(file-exists? filename) (write-json-wrapper routeInformation filename)]
        )
    ))

    (define/public saveRoute (lambda(s d savedRoute approxTime filename)
        (cond
            [(file-exists? filename)
                (let ((data (read-json-wrapper filename)))
                    (set! data (append data 
                        (list
                            (hash
                                'start s
                                'dest d
                                'route savedRoute
                                'time approxTime
                            )
                        )
                    ))
                    (write-json-wrapper data filename)
                )
            ]
            [else (let ((data (list 
                (hash
                    'start s
                    'dest d
                    'route savedRoute
                    'time approxTime
                )
            )))
                (write-json-wrapper data filename)
            )]
        )
    ))
    (define/public popScreen (lambda()
        (cond
            [(equal? prevScreen "")]
            [(equal? prevScreen screen)]
            [(equal? prevScreen mainmenu%) (clearScreen) (set! screen prevScreen) (new prevScreen[parent this][vert-margin 50])]
            [(and (equal? prevScreen savedRoutesPage%) (equal? screen routeInfoScreen%))(clearScreen) (set! screen prevScreen) (new prevScreen[parent this]) (set! prevScreen mainmenu%)]
        )
    ))
    (define/public switchScreens (lambda (newScreen args)
        (cond
            [(equal? screen newScreen)]
            [(equal? newScreen routeInfoScreen%) (clearScreen) 
                (set! prevScreen screen)
                (set! screen newScreen) 
                (new newScreen 
                    [parent this]
                    [start (hash-ref args 'start)]
                    [destination (hash-ref args 'dest)]
                    [route (hash-ref args 'route)]
                    [timeTaken (hash-ref args 'time)]
                    [save (hash-ref args 'save)]
                    )]
            [#t (clearScreen)(set! prevScreen screen)(set! screen newScreen) (new newScreen [parent this])]
        )
    ))
))


(define root (new parent%
    [label "Route Planner"]
    [min-width 400]
    [min-height 600]
    [stretchable-height #t][stretchable-width #t]
))

(define y (new mainmenu% [parent root][vert-margin 50]))
(send root show #t)