#lang racket/gui
#|After main-page comment, please read the comments from the bottom. Any empty messages are a contribution to
maintain the aesthetics of the page|#
#|Main page frame is created as a new class specifying the width and height and making sure is strechable|#
(define mainpageframe(new frame%
                        [label "Route Planner"][width 400][height 600][stretchable-height #t][stretchable-width #t]
))
#|The saved-start-destinations% will be defined as a group-box-panel% subclass to be able to show the saved routes
as buttons so the desired one can be chosen, the purpose of the group-box-panel% is to make sure that the subclass
satisfies the aesthetics desired|#
(define saved-start-destinations%(class group-box-panel%
                                    (super-new)
                                    (new button%[parent this][label "saved address 1"][min-width 350])
                                    (new button%[parent this][label "saved address 2"][min-width 350])
))
#|The screen is defined as a vertical panel subclass, this will create a new subclass that will be defined above, a text-field
and a button|#
(define firstscreen%(class vertical-panel%
                      (super-new)
                      (new text-field%[parent this][label #f][init-value "Search"][horiz-margin 50])
                      (new button%[parent this][label "Sort"][horiz-margin 50])
                      (new saved-start-destinations%[parent this][label "Start Location -> Destination"][horiz-margin 50])
                      
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)