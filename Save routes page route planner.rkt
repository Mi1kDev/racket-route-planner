#lang racket/gui
(define mainpageframe(new frame%
                        [label "Route Planner"][width 400][height 600][stretchable-height #t][stretchable-width #t]
))
(define saved-start-destinations%(class group-box-panel%
                                    (super-new)
                                    (new button%[parent this][label "saved address 1"][min-width 350])
                                    (new button%[parent this][label "saved address 2"][min-width 350])
))
(define firstscreen%(class vertical-panel%
                      (super-new)
                      (new text-field%[parent this][label #f][init-value "Search"][horiz-margin 50])
                      (new button%[parent this][label "Sort"][horiz-margin 50])
                      (new saved-start-destinations%[parent this][label "Start Location -> Destination"][horiz-margin 50])
                      
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)