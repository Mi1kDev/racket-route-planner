#lang racket/gui
#|After main-page comment, please read the comments from the bottom. Any empty messages are a contribution to
maintain the aesthetics of the page|#
#|Main page frame is created as a new class specifying the width and height and making sure is strechable|#
(define mainpageframe(new frame%
                        [label "Route Planner"][width 400][height 600][stretchable-height #t][stretchable-width #t]
))
#|A new class is created as a vertical-panel subclass, in here we place the save and search buttons|#
(define save-search%(class vertical-panel%
                        (super-new)
                        (define searchbutton(new button%[parent this][label "Search"]))
                        (define savebutton(new button%[parent this][label "Save"]))
))
#|Another class created as an horizontal-panel subclass, including the bus, train, taxi and acesibility check box,
the subclass save-search was called to be present inside the horizontal panel|#
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
#|This class as an horizontal-panel subclass, will include the start, destination, arrival and departure text-fields|#
(define start-destination-arrival-departure%(class horizontal-panel%
                                              (super-new)
                                              (define start(new text-field%[parent this][label #f][init-value "Start"]))
                                              (define destination(new text-field%[parent this][label #f][init-value "Destination"]))
                                              (new message%[parent this][horiz-margin 50][label ""])
                                              (define arrival(new text-field%[parent this][label #f][init-value "Arrival"]))
                                              (define departure(new text-field%[parent this][label #f][init-value "Departure"]))
))
#|The main screen is define as a vertical-panel subclass to maintain all the other subclasses in the page organised
with an horizontal margin of 50 in all the elements, a new button to check the saved routes is created followed by 
creating a list with data that will be afterwards included inside a list-box that will have a set up margin
with different styles incorporated to represent the columns and the data in the columns in the way desired.
Followed by this, the other subclasses are called|#
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