#lang racket/gui
#|After main-page comment, please read the comments from the bottom. Any empty messages are a contribution to
maintain the aesthetics of the page|#
#|Main page frame is created as a new class specifying the width and height and making sure is strechable|#
(define mainpageframe(new frame%
                        [label "Route Planner"][width 400][height 600][stretchable-height #t][stretchable-width #t]
))
#|route-plan is defined as a vertical panel subclass, to show the chosen route trajectory|#
(define route-plan%(class vertical-panel%
                        (super-new)
                        (new message%[parent this][label "this"][min-width 350])
                        (new message%[parent this][label "is "][min-width 350])
                        (new message%[parent this][label "for"][min-width 350])
                        (new message%[parent this][label "the"][min-width 350])
                        (new message%[parent this][label "route"][min-width 350])
))
#|Searchinput-time is defined as a horizontal panel subclass, to show the "Route Chosen" message and the "Route time" message|#
(define searchinput-time%(class horizontal-panel%
                            (super-new)
                            (new message%[parent this][label"route chosen"])
                            (new message%[parent this][label ""][horiz-margin 100])
                            (new message%[parent this][label "route time"])
))
#|The screen is defined as a subclass of a vertical panel to maintain it organised, it will only include 
the creation of two new subclasses. Style or alignment will be incorporated to represent the subclasses 
in the way desired.|#
(define firstscreen%(class vertical-panel%
                      (super-new)
                      (new searchinput-time%[parent this][horiz-margin 50][alignment (list 'center 'top)])
                      (new route-plan%[parent this][style (list 'border 'vscroll)][min-height 100][vert-margin 10][horiz-margin 50])
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)