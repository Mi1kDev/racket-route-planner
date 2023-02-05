#lang racket/gui
(define mainpageframe(new frame%
                        [label "Route Planner"][width 400][height 600][stretchable-height #t][stretchable-width #t]
))
(define route-plan%(class vertical-panel%
                        (super-new)
                        (new message%[parent this][label "this"][min-width 350])
                        (new message%[parent this][label "is "][min-width 350])
                        (new message%[parent this][label "for"][min-width 350])
                        (new message%[parent this][label "the"][min-width 350])
                        (new message%[parent this][label "route"][min-width 350])
))
(define searchinput-time%(class horizontal-panel%
                            (super-new)
                            (new message%[parent this][label"route chosen"])
                            (new message%[parent this][label ""][horiz-margin 100])
                            (new message%[parent this][label "route time"])
))
(define firstscreen%(class vertical-panel%
                      (super-new)
                      (new searchinput-time%[parent this][horiz-margin 50][alignment (list 'center 'top)])
                      (new route-plan%[parent this][style (list 'border 'vscroll)][min-height 100][vert-margin 10][horiz-margin 50])
))
(new firstscreen%[parent mainpageframe][vert-margin 50])
(send mainpageframe show #t)