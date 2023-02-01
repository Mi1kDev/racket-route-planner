#lang racket/gui
(define mainpageframe(new frame%
                        [label "Route Planner"][width 600][height 1800][stretchable-height #f][stretchable-width #f]
))
(define search-result-panel%(class vertical-panel%
                                (super-new)
                                (new button%[parent this][label "hello"][min-width 400])
                                (new button%[parent this][label "this"][min-width 400])
                                (new button%[parent this][label "is "][min-width 400])
                                (new button%[parent this][label "for"][min-width 400])
                                (new button%[parent this][label "the"][min-width 400])
                                (new button%[parent this][label "result"][min-width 400])
))
(define transport-accesibility-save%(class horizontal-panel%
                                              (super-new)
                                              (define bus(new check-box%[parent this][label "Bus"]))
                                              (define train(new check-box%[parent this][label "Train"]))
                                              (define taxi(new check-box%[parent this][label "Taxi"]))
                                              (new message%[parent this][horiz-margin 10][label ""])
                                              (define accesibility(new check-box%[parent this][label "Disable access"]))
                                              (new message%[parent this][horiz-margin 10][label ""])
                                              (define savebutton(new button%[parent this][label "Save"]))
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
                                    [style (list 'single 'column-headers 'vertical-label 'variable-columns )]
   	 	                            [horiz-margin 50][min-height 100]
                                    [columns (list "Time" "Route Time" "Stops to come")]))
                      (send/apply lbox set dummyData)
                      (new start-destination-arrival-departure% [parent this][horiz-margin 50][vert-margin 10])
                      (new transport-accesibility-save%[parent this][horiz-margin 50])
                      (new search-result-panel%[parent this][style (list 'border 'vscroll)][min-height 100][vert-margin 10][horiz-margin 50])
                      (new message%[parent this][label ""])
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)