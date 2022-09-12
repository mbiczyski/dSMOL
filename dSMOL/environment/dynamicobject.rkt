#lang racket

;(require environmentobject%)
(require "environmentobject.rkt"
         "../default.rkt")

;(provide dynamicobject%)
(provide dynamicobject%)

;dynamicobject% -|> environmentobject% <class>
(define dynamicobject%
  (class environmentobject%
    (super-new)

    (inherit write-data
             write-archive)
    (inherit-field id
                   type
                   samplingtime
                   currenttime)

    ;simulate-control <method>
    (define/public (simulate-control . arg)
       (control-actuator
        (control-autopilot
         (control-collision
          (control-coordination
           (control-mission
            (unless (empty? arg) (flatten arg))))))))

    ;simulate-dynamics <method>
    (define/public (simulate-motion . arg)
      (control-dynamics
            (unless (empty? arg) (flatten arg)))
      (set! currenttime (+ currenttime samplingtime))
      (write-archive))

    ; Control
    ;--------------------------------
    ;control-mission  <method>
    (define/public (control-mission . arg) (error "Error! Mission Control not initiated."))
    
    ;control-coordination <method>
    (define/public (control-coordination . arg) (error "Error! Coordination Control not initiated."))

    ;control-collision <method>
    (define/public (control-collision . arg) (error "Error! Collision Avoidance not initiated."))

    ;control-autopilot <method>
    (define/public (control-autopilot . arg) (error "Error! Autopilot not initiated."))

    ;control-actuator <method>
    (define/public (control-actuator . arg) (error "Error! Actuator not initiated."))

    ;control-dynamics <method>
    (define/public (control-dynamics . arg)
      (control-motion (control-aerodynamics
                       (unless (empty? arg) (flatten arg)))))

    ;control-aerodynamics <method>
    (define/public (control-aerodynamics . arg) (error "Error! Aerodynamics not initiated."))

    ;control-motion <method>
    (define/public (control-motion . arg) (error "Error! Equations of Motion not initiated."))


    ; Structure
    ;--------------------------------
    (init-field [cg '(0 0 0)] ;cg <field>
                [mass 1] ;mass <field>
                [inertia '((1 0 0) (0 1 0) (0 0 1))]) ;inertia <field>

    ; DataManagement
    ;--------------------------------
    ;write-control <method>
    (define/public (write-control lst #:name [name-string name-control] #:exists [mode 'truncate])
      (unless (empty? lst)
        (if (list? (first lst))
            (begin
              (write-data (first lst) #:name name-string #:exists mode)
              (map (lambda (line) (write-data line #:name name-string #:exists 'append)) (drop lst 1)))
            (write-data lst #:name name-string #:exists mode))))

    ;read-control <method>
    (define/public (read-control #:name [name-string name-control])
      (let*
          ([file-string (string-append name-string "_" id ".csv")]
           [lines (file->lines file-string)]
           [data-proc (lambda (line) (map string->number (string-split line ",")))])
        (map data-proc lines)))
    
    ;write-parameters <method>
    (define/public (write-parameters #:name [name-string name-parameters] #:exists [mode 'truncate])
      (write-data (flatten (list id type samplingtime mass cg)) #:name name-string #:exists mode)
      (map (lambda (in) (write-data in #:name name-string #:exists 'append)) inertia))

    ))