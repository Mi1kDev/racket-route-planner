#|Group members:    M00829986 MORGAN, Teon   -   M00774667 RINCON RODRIGUES, Alam
Route Planner App with GUI - Transport for London(TfL)|#
;lol
#lang racket/gui
;We import some of the required modules for the functionality of the app
;That being the route-find module which has a reference to the route-finder class
(require "route-find.rkt")
;The next being Racket's json module which is used to save and load json data.
(require json)

#|The main screen is define as a vertical-panel subclass to maintain all the other subclasses in the page organised
with an horizontal margin of 50 in all the elements, a new button to check the saved routes is created followed by 
creating a list with data that will be afterwards included inside a list-box that will have a set up margin
with different styles incorporated to represent the columns and the data in the columns in the way desired.
Followed by this, the other subclasses are called|#
(define mainmenu% (class vertical-panel%
    (super-new)
    ;Certain variables that may need to be referenced by various functions are initialized as fields. This includes 
    ;an instance of the route-finder class
    (init-field (route (new route-finder%)))
    ;a variable to store all the returned routes from the route-finder
    (init-field (found-routes #f))
    ;variables to store both the start and destination points as determined by the user
    (init-field (currentStart ""))
    (init-field (currentDest ""))
    ;a vector to store the states of some of the toggles used in the program. Initializing them as false to represent 
    ;the state of the user interface.
    (init-field (filters (make-vector 3 #f)))
    ;a variable to reflect the state of the accessibility toggle in the UI
    (init-field (accessiblity #f))
    ;a variable to determine whether the user has set a time to arrive at the destination by or to depart their location by
    (init-field (arrive? ""))
    ;a variable to hold the actual time the user wants to either arrive at or depart by
    (init-field (timeToPass ""))
    ;Train and Bus Data are both used to display information regarding the respective mode of transport
    ;In a full implementation this information should be based off of the user's geographical location but as of now it 
    ;simply displays placeholder information.
    (init-field (trainData
        (list
            (list "12:20" "12:45" "13:30")
            (list "10 min" "23 min" "35 min")
            (list "Leicester Square - King's Cross - Holloway Road" "Waterloo - Leicester - Tottenham" "Bank - Elephant&Castle - Kennington"))
    ))
    (init-field (busData
        (list
            (list "12:30" "12:35" "1:00")
            (list "10 min" "15 min" "40 min")
            (list "192" "196" "183"))
    ))
    ;the private accessibilityToggle and filterToggle are called by their respective UI checkboxes to reflect the state of 
    ;the checkbox within the actual variables
    (define/private accessiblityToggle (lambda (x)
        (cond
            [(not (equal? x accessiblity)) (set! accessiblity x)])
    ))
    (define/private filterToggle (lambda (x pos)
        (cond
            [(not (equal? x (vector-ref filters pos)))(vector-set! filters pos x)])
    ))
    ;A helper function which calls on the route-finder class to get all the routes between a start and end point and return 
    ;it to the found-routes variable
    (define/private findRoutes (lambda (s d)
        (cond
            ;The function would only need to be run if the currentStart and currentDestination are not the same as the passed 
            ;in s and d variables. This simply prevents unnecessary calls of the function
            [(not (and (equal? currentStart s) (equal? currentDest d))) (set! found-routes (send route run s d))])
    ))
    ;If for some reason an error occurs, then this function used to notify the user that something went wrong.
    (define/private displayMessage (lambda (errString)
        (send error-bar set-label errString)
    ))
    ;handles the allowed inputs for the findRoutes function as well as does any necessary error handling
    (define/public getRoutes (lambda()
        ;the function gets and stores the data from the text-field elements into variables for further use
        (let ((d (send sdbar getValue "des")) (s (send sdbar getValue "s")))
            (cond
                ;if the start and endpoint are the same then there is no need to get any routes
                [(equal? (string-downcase s) (string-downcase d))]
                ;calls the helper findRoutes function
                [#t (findRoutes s d)
                    ;We perform any necessary error handling. If the found routes are of valid form then we update the user interface 
                    ;to display the results.
                    ;If there is an issue with the form of the found routes it would mean something went wrong during execution and we display 
                    ;this information to the user.
                    (cond
                        [(and (equal? s currentStart) (equal? d currentDest))]
                        [(and (not (equal? #f found-routes))) (send sr addChildren found-routes) (displayMessage "")]
                        [#t (displayMessage "Something went wrong!")])
                    ;Update the currentStart and currentDest values to reflect the new currentStart and new currentDest
                    (set! currentStart s) 
                    (set! currentDest d)]))
    ))
    ;This function finds whether the user has entered into the departBy input field or ArriveAt input field. Only one can 
    ;be active at a time so if the user switches then the other field is cleared of all data
    (define/private arriveDepartureHandle (lambda (type otherField)
        (cond
            [(equal? type "a") (set! arrive? #t)
                (cond
                    [(not (equal? (send otherField get-value) "")) (send otherField set-value "")])]
            [(equal? type "d") (set! arrive? #f)
                (cond
                    [(not (equal? (send otherField get-value) "")) (send otherField set-value "")])])
    ))
    ;Changes the information displayed on the user interface from one preset to the other
    (define/private switchDisplayedData (lambda (currentData)
        (cond
            [(equal? currentData "See Bus Times")(send/apply timetable set busData)(send data-button set-label "See Train Times")]
            [(equal? currentData "See Train Times")(send/apply timetable set trainData)(send data-button set-label "See Bus Times")])
    ))
    ;This button allows us to view our saved routes by switching to the savedRoutes Screen
    (new button% [parent this] [label "Saved Routes"][callback (lambda (o e) (send (send this get-parent) switchScreens savedRoutesPage% '()))])
    ;timetable stores either our train or bus presets of information
    (define timetable (new list-box%
        [label ""]
        [parent (new horizontal-panel% [parent this])]
        [choices (list)]
        [style (list 'single 'column-headers 'vertical-label 'variable-columns)]
        [min-height 100]
        [horiz-margin 50]
        [columns (list "Time" "Route Time" "Stops To Come")]
    ))
    ;we start off by displaying the train data to the user first
    (send/apply timetable set trainData)
    ;by pressing the data-button can then switch to the other data preset
    (define data-button (new button%[label "See Bus Times"][parent this][callback (lambda (o e)(switchDisplayedData (send data-button get-label)))]))
    #|This class as an horizontal-panel subclass, will include the start, destination, arrival and departure text-fields|#
    (define start-destination-bar% (class horizontal-panel%
        (super-new)
        (define start (new text-field%[parent this][label "Start:"][init-value ""]))
        (define destination (new text-field%[parent this][label "Dest:"][init-value ""]))
        (new message%[parent this][horiz-margin 50][label ""])
        (define arrival (new text-field%[parent this][label "Arrive By:"][init-value ""][callback (lambda (o e)(arriveDepartureHandle "a" departure))]))
        (define departure(new text-field%[parent this][label "Depart At:"][init-value ""][callback (lambda(o e)(arriveDepartureHandle "d" arrival))]))
        ;helper function used to get the values of the various text-fields even from outside of the class
        (define/public getValue (lambda (x)
            (cond
                [(equal? x "s") (send start get-value)]
                [(equal? x "a") (send arrival get-value)]
                [(equal? x "des") (send destination get-value)]
                [(equal? x "dep") (send departure get-value)])))
    ))
    ;create an instantiation of the defined sub-class
    (define sdbar (new start-destination-bar%[parent this][horiz-margin 50][vert-margin 10]))
    #|Another class created as an horizontal-panel subclass, including the bus, train, cab and acesibility check box,
    the subclass save-search was called to be present inside the horizontal panel|#
    (define filters-buttons% (class horizontal-panel%
        (super-new)
        ;The various checkboxes (bus, train, cab, accessibility) have callbacks to update their respective fields with their 
        ;changed value. This makes using these values for calculations easier
        (define bus(new check-box%[parent this][label "Bus"][callback (lambda(o e)(filterToggle (send bus get-value) 0))]))
        (define train(new check-box%[parent this][label "Train"][callback (lambda(o e)(filterToggle (send train get-value) 1))]))
        (define cab(new check-box%[parent this][label "Cab"][callback (lambda(o e)(filterToggle (send cab get-value) 2))]))
        (new message%[parent this][horiz-margin 10][label ""])
        (define accessiblityBtn(new check-box%[parent this][label "Wheelchair Accessible"][callback (lambda (o e) (accessiblityToggle (send accessiblityBtn get-value)))]))
        (new message%[parent this][horiz-margin 10][label ""])
        ;The save-search is a sub-class of a vertical-panel and was originally supposed to hold two buttons, but the save button has since moved
        (define save-search% (class vertical-panel%
            (super-new)
            ;we create a search button which when called will call the getRoutes function defined above. As the button and 
            ;said function are not in the same scope, we have to get the parents of these classes until we can call the function
            (define searchButton (new button% [parent this][label "Search"][callback (lambda (o e) (search))]))
            (define/private search (lambda ()
                (send (send (send this get-parent) get-parent) getRoutes)))))
        (new save-search% [parent this])
    ))
    ;we create an instance of the filters-button class
    (define fb (new filters-buttons% [parent this][horiz-margin 50]))
    ;the sorting buttons are made children of a horizontal-panel which arranges them side by side
    (define sort-button-bar% (class horizontal-panel%
        (super-new)
        ;the sort buttons as the name implies call a function to sort any found routes in their respective orders
        (new button% [parent this][label "Sort Asc."][callback (lambda (o e) (send sr sortResults >))])
        (new button% [parent this][label "Sort Dsc."][callback (lambda (o e) (send sr sortResults <))])
    ))
    ;we create an instance of the sort-button-bar class
    (new sort-button-bar% [parent this][horiz-margin 50])
    ;the search results class will contain all of our found routes. We make it separate so we can easily make changes to the way in which routes are displayed
    (define search-results% (class vertical-panel%
        (super-new)
        ;helper function to convert the appeneded totall distance of a route to a time. Simply divides by the average speed of a train to return a time. 
        (define/private convertDistanceToTime (lambda (distance)
            (/ 20.5 distance)))
        ;helper function to convert route strings into a list of strings
        (define/private convertRouteStringToRoute (lambda (routeStr)
            (string-split routeStr " > ")))
        ;function to set the variables of the routeInformation page that the user will be taken to.
        ;sends the frame a call for the switchScreens function, passing in the parameters necessary to make each routeInformation screen unique
        (define/private buildPage (lambda(routeStr distance lines)
            (send (send (send this get-parent) get-parent) switchScreens routeInfoScreen% (hash 
                            'start currentStart
                            'dest currentDest
                            'route (convertRouteStringToRoute routeStr) 
                            'time (convertDistanceToTime distance)
                            'save #t
                            'lines lines
                            ))
        ))
        (define/public addChildren (lambda (results)
            ;clears all children of the panel
            (clearAll)
            ;loops through a given input (typically found routes)
            (for/list ([i results])
                (cond 
                    ;conditions are used for some formatting. If the length of a route string is above a certain threshold then we make limit it to a certain length
                    ;each button will follow the format "A>B>C Approx. {time}"
                    ;every button has a callback which allows them to switch to a new routeInformation screen
                    [(> (string-length (first i)) 40) (new button% [label (string-append (string-titlecase (substring (first i) 0 39)) "... "(number->string(/ (round (* 100 (convertDistanceToTime (second i))))100)))] [parent this][callback (lambda (o e)(buildPage (first i) (second i) (third i)))])]
                    [#t (new button% [label (string-append (string-titlecase (first i)) (number->string(/ (round (* 100 (convertDistanceToTime (second i))))100)))] [parent this][callback (lambda (o e)(buildPage (first i) (second i) (third i)))])
                    ]
                )
            )
        )) 
        ;sorts the found routes based off of their total distance and whatever procedure is passed ub
        (define/public sortResults (lambda (proc)
            (cond
                [(equal? found-routes #f)]
                [(<= (length found-routes) 1)]
                [#t (clearAll)
                    (set! found-routes (sort found-routes proc #:key (lambda (x) (second x))))
                    (addChildren found-routes)])))
        ;simply clears all children in the class
        (define/private clearAll (lambda ()
            (for ([child (send this get-children)])
                (send this delete-child child))))
    ))
    ;we create an instance of the search results class
    (define sr (new search-results% [parent this][style (list 'border 'vscroll)][min-height 100][vert-margin 10][horiz-margin 50]))
    (define error-bar (new message% [parent this][label ""][auto-resize #t]))
))

#|The screen is defined as a subclass of a vertical panel to maintain it organised, it will only include 
the creation of two new subclasses. Style or alignment will be incorporated to represent the subclasses 
in the way desired.|#
(define routeInfoScreen% (class vertical-panel%
    (super-new)
    ;initializees certain variables and their default values
    (init-field (start ""))
    (init-field (destination ""))
    (init-field (route '()))
    (init-field (save #f))
    (init-field (timeTaken 0))
    (init-field (timePassedIn ""))
    (init-field (lines '()))

    ;a horizontal class to display the start and destination and arrange them
    (define hz% (class horizontal-panel%
        (super-new)
        (new message%[parent this][horiz-margin 50][label (string-upcase (string-append start "->" destination))][font (send the-font-list find-or-create-font 8 "Arial" 'default 'normal 'bold)])
    ))
    (new hz% [parent this])

    ;A vertical panel to store each station within the route and display it as a message
    (define routeInformation% (class vertical-panel%
        (super-new)
        (define/private getColor (lambda (station)
                (cond 
                    [(equal? station "piccadily") "blue"]
                    [(equal? station "northern") "black"]
                ) 
        ))
        
        (for ([i (in-range (length route))])
            ;conditions to determine whether we need to change lines and displays when to change lines
            (cond
                [(and ( < i (-(length route)1)) (equal? (list-ref lines i) (list-ref lines (+ 1 i))))]
                [(and ( < i (-(length route)1)) (not (equal? (list-ref lines i) (list-ref lines (+ 1 i)))))
                    (cond
                        [(and (< i (- (length route) 1)) (equal?  (list-ref route (+ 1 i)) (last route))) (new message%[parent this][label (string-append "Change to " (string-upcase (list-ref lines (+ 1 i))) " Line")][auto-resize #t][color (getColor (list-ref lines (+ 1 i)))][font (send the-font-list find-or-create-font 8 "Arial" 'default 'normal 'bold)])]
                        [(and (< i (- (length route) 2)) (equal? (list-ref lines (+ 1 i)) (list-ref lines (+ 2 i)))) (new message%[parent this][label (string-append "Change to " (string-upcase (list-ref lines (+ 1 i))) " Line")][auto-resize #t][color (getColor (list-ref lines (+ 1 i)))][font (send the-font-list find-or-create-font 8 "Arial" 'default 'normal 'bold)])]
                    )
                ]
            )
            ;build and display message widgets for each given route
            (new message%[parent this][label (string-titlecase (list-ref route i))][auto-resize #t][min-width 350])
        )
    ))
    (new routeInformation%[parent this][style (list 'border 'vscroll)][min-height 30][horiz-margin 50])

    ;A class to hold information regarding the time needed to travel this route
    (define hzp% (class horizontal-panel%
        (super-new)
        (new message%[parent this][horiz-margin 50][label (string-append (number->string (/(round(* 100 timeTaken))100)) "hrs")][font (send the-font-list find-or-create-font 8 "Arial" 'default 'normal 'bold)])
    ))
    (new hzp% [parent this])
    ;If when building this screen, a save tag is passed in then we should display a save button
    (cond
        ;The save button calls a function of the frame that being saveRoute and provides it with the necessary data for this particular route to be saved in a json file
        [(equal? save #t) (new button%[parent this][label "Save"][min-width 300][callback (lambda (o e)(send (send this get-parent) saveRoute start destination route timeTaken lines "save.json"))])]
    )
    (define hzpanel(new horizontal-panel%[parent this]))
    ;we create a back button which calls a function of the frame which keeps track of what screen the user is currently viewing 
    ;and returns them to the appropriate previous screen
    (new button%[parent hzpanel][label "Back"][horiz-margin 50][callback (lambda (o e)(send (send this get-parent) popScreen))])
)) 

;this class represents the saved routes screen. It displays all saved routes and allows for routes to be deleted or searched through.
(define savedRoutesPage% (class vertical-panel%
    (init-field (routeInfo '()))
    (super-new)
    ;This function calls on the frame to load data from a json file. If any data is returned then we set routeInfo field to 
    ;be this data. If not we simply display a message that there are no saved routes.
    (define/private loadData (lambda ()
        (let ((data (send (send this get-parent) loadRoutes "save.json")))
            (cond
                [(empty? data) (new message% [parent this][label "You have no Saved Routes"])]
                [#t (set! routeInfo data)]))
    ))
    ;This function allows us to switch screens to a route information screen, this version of the route information screen 
    ;should not allow us to save however as the route is already saved.
    ;As such we pass false into the save tag
    (define/private createRoutePage (lambda (start dest route timeTaken lines)
        (send (send this get-parent) switchScreens routeInfoScreen% (hash
            'start start
            'dest dest
            'route route
            'time timeTaken
            'save #f
            'lines lines
        ))
    ))
    ;helper function to check if a search parameter exists in a list
    (define/private isInData? (lambda (search data)
        (ormap (lambda (d) (equal? search d)) data)
    ))
    ;removes a given route from route info and rewrites the json file to exclude the route, effectively deleting it.
    (define/private deleteSavedRoute (lambda (data pArg)
        (cond
            ;if the data we want to delete is a saved route then we remove it from route info and rebuild the page without 
            ;it then update the json file to reflect the change
            [(isInData? data routeInfo) 
                (set! routeInfo(remove data routeInfo)) (clear pArg) (buildRoutes pArg routeInfo)
                (send (send this get-parent) saveRoutes routeInfo "save.json")])
    ))
    ;takes a parent widget (a vertical panel) and a list of routes, loops through the list of routes and creates both a button 
    ;to view the route information page and a button to delete the route
    (define/private buildRoutes (lambda (parentArg routes)
        (cond
            [(empty? routeInfo)]
            [#t (for ([i routes])
                (define n (new horizontal-panel%[parent parentArg][alignment (list 'center 'center)]))
                (new button%[parent n][label (string-append (hash-ref i 'start) "->" (hash-ref i 'dest) " " (number->string(/ (round ( * 100 (hash-ref i 'time))) 100)))][callback (lambda (o e) (createRoutePage (hash-ref i 'start) (hash-ref i 'dest) (hash-ref i 'route) (hash-ref i 'time) (hash-ref i 'lines)))])
                (new button%[parent n][label "Delete"][callback (lambda (o e)(deleteSavedRoute i parentArg))])
            )]
        )
    ))
    ;helper function which clears all the elements of a given widget
    (define/private clear (lambda (clearPanel)
        (for ([child (send clearPanel get-children)])
            (send clearPanel delete-child child))
    ))
    ;sorts routes based off of their time taken to travel and updates routeInfo to reflect the change. The page is then rebuilt to display the new order
    (define/private sortRoutes (lambda (parentArg proc)
        (cond
            [(<= (length routeInfo) 1)]
            [#t (clear parentArg)
                (set! routeInfo (sort routeInfo proc #:key (lambda (x)(hash-ref x 'time))))
                (buildRoutes parentArg routeInfo)])
    ))
    ;a function to create the sort buttons. These are only needed if the user does in fact have some routes saved
    (define/private buildButtons (lambda (hzPanel pArg)
        (new button%[parent hzPanel][label "Sort Asc."][callback (lambda (o e)(sortRoutes pArg <))])
        (new button%[parent hzPanel][label "Sort Dsc."][callback (lambda (o e)(sortRoutes pArg >))])
    ))
    ;filters and displays routes in which a given search parameter exists in either the start value or destination value of the route
    ;updates the vertical-panel to reflect the change
    (define/private search (lambda (textValue resultPanel)
        (let ((filteredResults (filter (lambda (x) (or (string-contains? (string-downcase (hash-ref x 'dest)) (string-downcase textValue))
                                                       (string-contains? (string-downcase (hash-ref x 'start)) (string-downcase textValue)))) routeInfo)))
            (clear resultPanel)
            (buildRoutes resultPanel filteredResults))
    ))
    ;loads the necessary data from a json file
    (loadData)
    (cond
        [(empty? routeInfo)]
        ;if we have saved routes then we create a search bar, a panel to display the saved routes as well as the buttons to 
        ;sort them. Then we display all the saved routes inside our panel
        [#t (define searchBar (new text-field% [parent this][label "Search"][horiz-margin 50][callback (lambda (o e) (search (send searchBar get-value) vp))]))
            (define vp (new vertical-panel% [parent this][style (list 'border 'vscroll)][horiz-margin 50]))
            (buildButtons (new horizontal-panel%[parent this][horiz-margin 50]) vp)
            (buildRoutes vp routeInfo)]
    )
    ;we create a back button to move us to our previous screen should we need to go back
    (new button%[parent this][label "Back"][callback (lambda (o e)(send (send this get-parent) popScreen))])
))

;the parent class is a sub-class of the frame class. With extra functions and variables for saving and loading data as well 
;as managing the various screens.
(define parent% (class frame%
    (super-new)
    ;we store both the current screen and the previous screen to determine which screen to go to when going back from another screen
    (init-field (screen mainmenu%))
    (init-field (prevScreen ""))
    ;helper function to remove all child widgets
    (define/private clearScreen (lambda ()
        (for ([child (send this get-children)])
            (send this delete-child child))
    ))
    ;helper function to store an object in a json file
    (define/private write-json-wrapper (lambda (jsexpr filename)
        (call-with-output-file filename (lambda (x) (write-json jsexpr x)) #:exists 'replace)
    ))
    ;helper function to read an object from a json file
    (define/private read-json-wrapper (lambda (filename)
        (call-with-input-file filename read-json)
    ))
    ;returns routes saved in a json file provided the file exists
    (define/public loadRoutes (lambda (filename)
        (cond
            [(file-exists? filename) (read-json-wrapper filename)]
            [#t '()])
    ))
    ;saves a list of routes to a json file. This is called when deleting a route
    (define/public saveRoutes (lambda (routeInformation filename)
        (cond
            [(file-exists? filename) (write-json-wrapper routeInformation filename)])
    ))
    ;saves a singular route at a time
    (define/public saveRoute (lambda(s d savedRoute approxTime lines filename)
        (cond
            ;if the file exists already then reads the contents of the file and appends the new route to be saved to those 
            ;contents before rewriting the file. Effectively merging the previous data with the new
            [(file-exists? filename)
                (let ((data (read-json-wrapper filename)))
                    (set! data (append data 
                        (list
                            (hash
                                'start s
                                'dest d
                                'route savedRoute
                                'time approxTime
                                'lines lines
                            )
                        )
                    ))
                    (write-json-wrapper data filename)
                )
            ];if the file does not exist then we are creating the file for the first time and do not need to ready any data from the file. We simply write our data to the file.
            [else (let ((data (list 
                (hash
                    'start s
                    'dest d
                    'route savedRoute
                    'time approxTime
                    'lines lines
                )
            )))
                (write-json-wrapper data filename)
            )]
        )
    ))
    ;switches the scren back to a previous screen
    ;certain conditions are used to prevent the screens from looping on each other
    (define/public popScreen (lambda()
        (cond
            [(equal? prevScreen "")]
            [(equal? prevScreen screen)]
            [(equal? prevScreen mainmenu%) (clearScreen) (set! screen prevScreen) (new prevScreen[parent this][vert-margin 50])]
            [(and (equal? prevScreen savedRoutesPage%) (equal? screen routeInfoScreen%))
                (clearScreen) (set! screen prevScreen) (new prevScreen[parent this]) (set! prevScreen mainmenu%)])
    ))
    ;switches from the current screen to a new screen. The new screen is a provided class. Some screens such as routeInfoScreen have specific 
    (define/public switchScreens (lambda (newScreen args)
        (cond
            ;if the screen we are to switch to is the same as the currentScreen then we do not need to switchScreens
            [(equal? screen newScreen)]
            ;if the new screen is a route information screen then we clear the screen. Set our previous screen to be out 
            ;current screen and set our current screen to be the new screen. We then instantiate the route information screen 
            ;with the arguments passed into the function.
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
                    [lines (hash-ref args 'lines)]
                    )]
            ;if the screen does not need special arguments to build then we set the previous screen to our current screen and our current screen to the new screen and then instantiate whatever this new screen is
            [#t (clearScreen)(set! prevScreen screen)(set! screen newScreen) (new newScreen [parent this])]
        )
    ))
))

;we create an instance of our parent class
(define root (new parent%
    [label "Route Planner"]
    [min-width 400]
    [min-height 600]
    [stretchable-height #t][stretchable-width #t]
))
;we parent the mainmenu to this instance as it is the first screen displayed
(define y (new mainmenu% [parent root][vert-margin 50]))
;finally we tell the frame to show by sending it a message
(send root show #t)