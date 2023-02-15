#lang racket/gui

;A list of the names of the various stations to easily map an index to the station name
(define network-names '("euston" "tottenham court road" "leicester square" "green park" "waterloo" "kennington" "elephant and castle" "bank" "king's cross st. pancras" "holloway road"))
;A list of lists representing the layout of the network. Each index represents a station and the list at that index represents the connected stations
(define network-structure '(
    #|0: Euston|#(1 8)
    #|1: Tottenham|#(0 2)
    #|2: Leicester Square|#(1 3 4 8)
    #|3: Green Park|#(2)
    #|4: Waterloo|#(2 5)
    #|5: Kennington|#(4 6)
    #|6: Elephant and Castle|#(5 7)
    #|7: Bank|#(6 8)
    #|8: King's Cross St. Pancras|#(0 2 7 9)
    #|9: Holloway Road|#(8)
))

(define stationLines '("northern" "northern" "piccadily" "piccadily" "northern" "northern" "northern" "northern" "piccadily" "piccadily"))
;Similar to the network structure but displays the distance from a station to its connected stations
(define network-distances '(
    (2.4 3.0)
    (2.4 2.0)
    (2.0 1.5 2.5 4)
    (1.5)
    (2.5 1.5)
    (1.5 2.0)
    (1.5 2.35)
    (2.35 1.6)
    (3.0 4 1.6 1.0)
    (1.0)
))

(define route-finder% (class object%
    (super-new)
    ;indicates number of points or nodes in the network
    (init-field (v 10))

    (init-field (names network-names))
    (init-field (graph network-structure))
    (init-field (distances network-distances))
    (init-field (trainLines stationLines))

    ;keeps track of the totalDistance travelled by each route
    (init-field (totalDistance 0))

    ;stores the formatted string of a given route
    (init-field (stringPath ""))

    ;is a list of these formatted paths
    (init-field (convertedPath '()))
    
    ;a list of the returned routes as indices
    (init-field (returnPath '()))

    ;helper function to quickly create and return a list populated with y for x times
    (define/private populate (lambda(x y)
        (for/list ([i x])y)
    ))

    ;returns all routes from a start point to an end point using depth first search
    (define/private getAllPaths (lambda (s d visited path)
        ;we set whatever point we are currently at as being visited to prevent a cyclic loop
        (set! visited (list-set visited s #t))
        ;add the current point to the path
        (set! path (append path (list s)))
        (cond
            ;if the start point and end are the same then we have reached our destination and can save our path so far to the returnPath for future use
            [(equal? s d) (set! returnPath (cons path returnPath))]
            [#t 
                ;if we are not at the end yet then we loop through the connected stations
                (for ([i (list-ref graph s)])
                    ;if a station has not yet been visited then we call this function again using this station as our new start point
                    (cond
                        [(equal? (list-ref visited i) #f) (getAllPaths i d visited path)]
                    )
                )
                ;if we loop through the station and every connected station has been visited then we remove this from the path and update it to be not visited
                ;at this point it means there are no more stations to check and that we cannot get from our start point to our destination
                (set! path (remove (last path) path))
                (set! visited (list-set visited s #f))
            ]
        )
    ))

    ;this function calculates the total distance travelled by each route as well as converting the route from simply indexes to actual formatted strings
    (define/public printReturnPaths (lambda ()
        ;a let function is used as some variables will be repeatedly used
        (let ((newPath '()) (lines '()) (finalPath '()))
            (cond
                ;if there is nothing in the returnPath then we return this string. In this case all stations are connected so this should never occur.
                [(empty? returnPath) "There is no path between these stations."]
                ;if the returnPath has elements in it then we loop through the list and format into an appropriate string
                [#t (for ([i (in-range (length returnPath))])
                    (for ([j (in-range (length (list-ref returnPath i)))])
                        (cond
                            [(not (equal? (list-ref (list-ref returnPath i) j) (last (list-ref returnPath i)))) (set! stringPath (string-append stringPath (list-ref names (list-ref (list-ref returnPath i) j)) " > "))]
                            [else (set! stringPath (string-append stringPath (list-ref names (list-ref (list-ref returnPath i) j))))]
                        )
                        (set! lines (cons (list-ref trainLines (list-ref (list-ref returnPath i) j)) lines))
                        
                        ;we total the distances of the station to the next in the route
                        (cond
                            [(< j (- (length (list-ref returnPath i)) 1)) (set! totalDistance  (+ totalDistance (list-ref (list-ref distances (list-ref (list-ref returnPath i) j)) (index-of (list-ref graph (list-ref (list-ref returnPath i) j))(list-ref (list-ref returnPath i) (+ 1 j))))))]
                        )
                    )
                    ;we append the formatted string and the distance travelled as both are needed values
                    ;this is done after looping through one individual route
                    (set! newPath (list(list stringPath totalDistance (reverse lines))))
                    ;we append this newPath to a list which will contain a list of  lists of route strings and total distances
                    (set! finalPath (append finalPath newPath))
                    
                    ;we then reset the total distance as well as the formatted string to be used in the next route
                    (set! lines '())
                    (set! totalDistance 0)
                    (set! stringPath "")
                    
                )
                ;once we have completed the loop we set the return path to empty so if we need another route calculation done, those results will not be appended to previous results
                ;finally we return the finalPath which is our list of lists of route strings and total distances
                (set! returnPath '()) finalPath]
            )
        )
    ))

    ;helper function to check whether an element exists in a list
    (define/private isMember? (lambda (str strs) (ormap [lambda (s) (string=? s str)] strs)))

    ;starts the route finding process, does the necessary data conversions and returns useable data
    (define/public run (lambda (s d)
        ;we assign a list visited to be filled with false as we are hereby instantiating the visited list to be used in the getAllPaths function
        (let ((visited (populate v #f)) (path '()))
            (cond
                ;if the provided inputs are both members of the list of stations then we getAllPaths or get all the routes between the start and destination then we convert it to strings and distances and return the data
                [(and (isMember? (string-downcase s) names)(isMember? (string-downcase d) names))
                    (getAllPaths (index-of names (string-downcase s)) (index-of names (string-downcase d)) visited path) (send this printReturnPaths)]
                [#t #f]
            )
        )
    ))
))
;we provide the class so it can be imported in other racket programs for use
(provide route-finder%)