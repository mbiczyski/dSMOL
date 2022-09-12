#lang racket

(require racket/struct)
(require "../default.rkt")

;(provide environmentobject%)
(provide environmentobject%)

;environmentobject% <class>
(define environmentobject%
  (class object%    
    (super-new)
    
    (init-field id ;id <field>
                [type "default"]
                [samplingtime 0.01] ;samplingtime <field>
                [init-currenttime 0] ;init-currenttime <field>
                [init-state '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]) ;init-state <field>
    (field [currenttime init-currenttime]) ;currenttime <field>

    
    ; State
    ;--------------------------------
    (struct struct-state (x y z) #:mutable #:transparent) ;struct-state <struct>
    (define position (struct-state (list-ref init-state 0) (list-ref init-state 1) (list-ref init-state 2))) ;position <struct-state>
    (define velocity (struct-state (list-ref init-state 3) (list-ref init-state 4) (list-ref init-state 5))) ;velocity <struct-state>
    (define acceleration (struct-state (list-ref init-state 6) (list-ref init-state 7) (list-ref init-state 8))) ;acceleration <struct-state>
    (define angle (struct-state (list-ref init-state 9) (list-ref init-state 10) (list-ref init-state 11))) ;angle <struct-state>
    (define angvelocity (struct-state (list-ref init-state 12) (list-ref init-state 13) (list-ref init-state 14))) ;angvelocity <struct-state>
    (define angacceleration (struct-state (list-ref init-state 15) (list-ref init-state 16) (list-ref init-state 17))) ;angacceleration <struct-state>

    ;DCM <method>
    (define/public (DCM lst #:direction mode)
      (let*
          ([phi (get-state #:param 'angle #:axis 'x)]
           [theta (get-state #:param 'angle #:axis 'y)]
           [psi (get-state #:param 'angle #:axis 'z)]
           [cphi (cos phi)] [sphi (sin phi)]
           [ct (cos theta)] [st (sin theta)]
           [cpsi (cos psi)] [spsi (sin psi)])
        (case mode
          [(quote body-to-earth) (list
                                  (+ (* (first lst) (* ct cpsi))
                                     (* (second lst) (- (* sphi st cpsi) (* cphi spsi)))
                                     (* (third lst) (+ (* cphi st cpsi) (* sphi spsi))))
                                  (+ (* (first lst) (* ct spsi))
                                     (* (second lst) (+ (* sphi st spsi) (* cphi cpsi)))
                                     (* (third lst) (- (* cphi st spsi) (* sphi cpsi))))
                                  (+ (* (first lst) (- st))
                                     (* (second lst) (* (third lst) (* sphi ct)))
                                     (* (third lst) (* cphi ct))))]
          [(quote earth-to-body) (list
                                  (+ (* (first lst) (* ct cpsi))
                                     (* (second lst) (* ct spsi))
                                     (* (third lst) (- st)))
                                  (+ (* (first lst) (- (* sphi st cpsi) (* cphi spsi)))
                                     (* (second lst) (+ (* sphi st spsi) (* cphi cpsi)))
                                     (* (third lst) (* sphi ct)))
                                  (+ (* (first lst) (+ (* cphi st cpsi) (* sphi spsi)))
                                     (* (second lst) (- (* cphi st spsi) (* sphi cpsi)))
                                     (* (third lst) (* cphi ct))))]
          [else (error "Error! Wrong keyword:" mode)])))

    ; Control
    ;--------------------------------
    ;control-init <method>
    (define/public (control-init)
      (error "Error! Initialisation routine not specified."))

    ; Structure
    ;--------------------------------    
    ;zone <struct-zone>
    (define zones empty)

    ;add-structure-zone <method>
    (define/public (add-structure-zone lst [offset '(0 0 0)])
      (if (list? lst)
          (if (andmap list? lst)
              (set! zones
                    (append zones (list
                                   (map
                                    (lambda (point) (map + offset point))
                                      lst))))
              (error "Error! Not a proper set of points: " lst))
          (error "Error! Not a list: " lst)))
    
    ;get-structure-zone <method>
    (define/public (get-structure-zone lst)
      (map
       (lambda (points)
         (let ([strc-points (DCM points #:direction 'body-to-earth)]
               [posn-points (get-state #:param 'position)])
           (list (+ (first strc-points) (first posn-points))
                 (+ (second strc-points) (second posn-points))
                 (+ (third strc-points) (third posn-points)))))
       lst))
           
    ;get-structure-full <method>
    (define/public (get-structure-full)
      (map
       (lambda (set) (get-structure-zone set))
       zones))

    ; Data Management
    ;--------------------------------
    ;get-state <method>
    (define/public (get-state #:param [param 'all] #:axis [axis 'all])
      (let ([param-struct (case param
                            [(quote position) position]
                            [(quote velocity) velocity]
                            [(quote acceleration) acceleration]
                            [(quote angle) angle]
                            [(quote angvel) angvelocity]
                            [(quote angaccel) angacceleration]
                            [(quote all) (list position velocity acceleration angle angvelocity angacceleration)]
                            [else (error "Error! Wrong keyword:" param)])])
        (if (equal? param (quote all))
            (flatten (map (get-axis axis) param-struct))
            ((get-axis axis) param-struct))))

    ;set-state <method>
    (define/public (set-state value #:param [param 'position] #:axis [axis 'all] . value-list)
      (let ([param-struct (case param
                            [(quote position) position]
                            [(quote velocity) velocity]
                            [(quote acceleration) acceleration]
                            [(quote angle) angle]
                            [(quote angvel) angvelocity]
                            [(quote angaccel) angacceleration]
                            [else (error "Error! Wrong keyword:" param)])])
        (case axis
          [(quote x) (set-struct-state-x! param-struct value)]
          [(quote y) (set-struct-state-y! param-struct value)]
          [(quote z) (set-struct-state-z! param-struct value)]
          [(quote all) (if (list? value)
                           (begin
                             (set-struct-state-x! param-struct (first value))
                             (set-struct-state-y! param-struct (second value))
                             (set-struct-state-z! param-struct (third value)))
                           (begin
                             (set-struct-state-x! param-struct value)
                             (set-struct-state-y! param-struct (first value-list))
                             (set-struct-state-z! param-struct (second value-list))))]
          [else (error "Error! Wrong keyword:" axis)])))
    
    ;read-state <method>
    (define/public (read-state #:name [name-string name-state] #:line [target 1])
      (let*
          ([file-string (string-append name-string "_" id ".csv")]
           [line (list-ref (file->lines file-string) (sub1 target))]
           [data (map string->number (string-split line ","))])
        (set-state (list-ref data 0) (list-ref data 1) (list-ref data 2) #:param 'position)
        (set-state (list-ref data 3) (list-ref data 4) (list-ref data 5) #:param 'velocity)
        (set-state (list-ref data 6) (list-ref data 7) (list-ref data 8) #:param 'acceleration)
        (set-state (list-ref data 9) (list-ref data 10) (list-ref data 11) #:param 'angle)
        (set-state (list-ref data 12) (list-ref data 13) (list-ref data 14) #:param 'angvel)
        (set-state (list-ref data 15) (list-ref data 16) (list-ref data 17) #:param 'angaccel)))
    
    ;write-data <method>
    (define/public (write-data lst #:name [name-string name-data] #:exists [mode 'truncate])
      (let
          ([file-string (string-append name-string "_" id ".csv")])
        (begin
          (display-lines-to-file (drop-right lst 1) file-string #:separator #"," #:exists mode)
          (display-to-file (last lst) file-string #:exists 'append)
          (display-to-file "\n" file-string #:exists 'append))))
    
    ;write-state <method>
    (define/public (write-state #:name [name-string name-state] #:exists [mode 'truncate])
      (write-data (get-state #:param 'all #:axis 'all) #:name name-string #:exists mode))
    
    ;write-archive <method>
    (define/public (write-archive #:name [name-string name-archive] #:exists [mode 'append])
      (write-data
       (append (list currenttime) (get-state #:param 'all #:axis 'all)) #:name name-string #:exists mode))
    
    ;write-structure <method>
    (define/public (write-structure #:name [name-string name-structure] #:exists [mode 'truncate])
      (let ([file-string (string-append name-string "_" id ".csv")])
        (display-to-file "" file-string #:exists mode)
        (map (lambda (set)
               (map (lambda (zone)
                      (write-data zone #:name name-string #:exists 'append))
                    set)
               (display-to-file "NaN,NaN,NaN\n" file-string #:exists 'append))
             (get-structure-full))))
    
    ;display-state <method>
    (define/public (display-state)
      (display "State: ")
      (displayln (get-state #:param 'all #:axis 'all)))
    
    ;display-structure <method>

    ; Supporting functions
    ;--------------------------------
    (define/private (get-axis axis)
      (lambda (param-struct)
        (case axis
          [(quote x) (struct-state-x param-struct)]
          [(quote y) (struct-state-y param-struct)]
          [(quote z) (struct-state-z param-struct)]
          [(quote all) (struct->list param-struct)]
          [else (error "Error! Wrong keyword:" axis)])))
    
    ))