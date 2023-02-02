#lang racket/gui
(define mainpageframe(new frame%
                        [label "Route Planner"][width 600][height 1800][stretchable-height #f][stretchable-width #f]
))
(define save-search%(class vertical-panel%
                        (super-new)
                        (define searchbutton(new button%[parent this][label "Search"]))
                        (define savebutton(new button%[parent this][label "Save"]))
))
(define transport-accesibility-searchandsave%(class horizontal-panel%
                                                (super-new)
                                                (define bus(new check-box%[parent this][label "Bus"]))
                                                (define train(new check-box%[parent this][label "Train"]))
                                                (define taxi(new check-box%[parent this][label "Taxi"]))
                                                (new message%[parent this][horiz-margin 10][label ""])
                                                (define accesibility(new check-box%[parent this][label "Disable access"]))
                                                (new message%[parent this][horiz-margin 10][label ""])
                                                (new save-search%[parent this])
))
(define start-destination-arrival-departure%(class horizontal-panel%
                                              (super-new)
                                              (define start(new text-field%[parent this][label #f][init-value "Start"]))
                                              (define destination(new text-field%[parent this][label #f][init-value "Destination"]))
                                              (new message%[parent this][horiz-margin 50][label ""])
                                              (define arrival(new text-field%[parent this][label #f][init-value "Arrival"]))
                                              (define departure(new text-field%[parent this][label #f][init-value "Departure"]))
))
(define firstscreen%(class vertical-panel%
                        (super-new)
                        (init-field (dummyData 
                                        (list (list "12:20" "12:45" "13:30")
                                            (list "10 min" "23 min" "35 min")
                                            (list "Leicester Square - King's Cross - Holloway Road" "Waterloo - Leicester - Tottenham" "Bank - Elephant&Castle - Kennington"))))
                        (new button%[parent this][label "Saved Routes"])
                        (define lbox(new list-box%[label #f]
                                                [parent (new horizontal-panel%[parent this])]
                                                [choices (list)]
                                                [style (list 'single 'column-headers 'vertical-label 'variable-columns)]
   	 	                                        [horiz-margin 50][min-height 100]
                                                [columns (list "Time" "Route Time" "Stops to come")]))
                        (send/apply lbox set dummyData)
                        (new start-destination-arrival-departure% [parent this][horiz-margin 50][vert-margin 10])
                        (new transport-accesibility-searchandsave%[parent this][horiz-margin 50])
                        (new message%[parent this][vert-margin 50][label ""])
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)