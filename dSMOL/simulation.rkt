#lang racket

(require racket/struct)
(require pict3d)

;(require environmentcontrol)
(require "environment/environmentcontrol.rkt")

(provide simulate-once
         simulate-all
         add-motion-constraint
         add-hit-constraint
         add-area-constraint
         set-simulation-area
         set-timer
         stop-timer)

(define constraints empty) ;constraints <list>

; SimulationControl
;--------------------------------
;simulate-all <method>
(define (simulate-all value
                      #:mode [mode 'time]
                      #:display [disp 'time])
  (let*
      ([init-proc (lambda (object)
                    (send object control-init))]
       [objects (append drones threats obstacles)]
       [iter (case mode
               [(quote time) (/ value (get-field samplingtime (first objects)))]
               [(quote iterations) value]
               [else (error "Error! Wrong keyword:" mode)])])
    (begin
      (map init-proc objects)
      (when (or
             (equal? disp (quote time))
             (equal? disp (quote full)))
        (printf "Time: ~a\n" (get-field currenttime (first objects))))
      (define errval #t)
      (for ([ii (in-range iter)])
        (set! errval (if (equal? disp (quote full))
                         (simulate-once #:display 'yes)
                         (simulate-once #:display 'no)))
        #:break (not errval)
        (when (or
               (equal? disp (quote time))
               (equal? disp (quote full)))
          (printf "Time: ~a\n" (get-field currenttime (first objects)))))
      (if errval
          #t
          #f))))

;DODAC SYMLUACJE DLA ROZNYCH SAMPLING TIME
;simulate-once <method>
(define (simulate-once #:display [disp 'no])
  (let
      ([sim-dyn-proc (lambda (object control)
                       ;PRZERZUCIC DO TRAITOW JAK SIE ZROBI BEHAVIOURS
                       (send object simulate-motion control)
                       (send object write-structure #:exists 'truncate))]
       [sim-con-proc (lambda (object) (send object simulate-control))]
       [objects (append drones threats obstacles)]
       [controllables (append drones threats)])
    (map sim-dyn-proc controllables (map sim-con-proc controllables))
    (map sim-dyn-proc obstacles (make-list (length obstacles) empty))
    (define errval (apply + (map (lambda (constr) (check-constraint constr #:display disp)) constraints)))
    (if (zero? errval)
        #t
        (begin
          (when (equal? disp (quote yes)) (displayln "Stop! Constraint broken."))
          #f))))


; Constraints
;--------------------------------
;struct-motionconstraint <struct>
(struct struct-motionconstraint (type param axis minlimit maxlimit action)  #:mutable #:transparent)

;add-motion-constraint <method>
(define (add-motion-constraint type limits
                               #:param param
                               #:axis axis
                               #:action [action 'skip])
  (set! constraints
        (append constraints
                (if
                 (member param (list (quote position) (quote velocity) (quote acceleration)
                                     (quote angle) (quote angvel) (quote angaccel)))
                 (if
                  (member action (list (quote limit) (quote skip) (quote stop) (quote error)))
                  (case axis
                    [(quote x y z)
                     (list (struct-motionconstraint type param axis (first limits) (second limits) action))]
                    [(quote all) (list
                                  (struct-motionconstraint type param 'x (first limits) (second limits) action)
                                  (struct-motionconstraint type param 'y (first limits) (second limits) action)
                                  (struct-motionconstraint type param 'z (first limits) (second limits) action))]
                    [else (error "Error! Wrong keyword:" axis)])
                  (error "Error! Wrong keyword:" action))
                 (error "Error! Wrong keyword:" param)))))

;struct-hitconstraint <struct>
(struct struct-hitconstraint (action) #:mutable #:transparent)

;add-hit-constraint <method>
(define (add-hit-constraint #:action [action 'stop])
  (set! constraints
        (append constraints
                (if
                 (member action (list (quote skip) (quote stop) (quote error)))
                 (list (struct-hitconstraint action))
                 (error "Error! Wrong keyword:" action)))))

;struct-areaconstraint <struct>
(struct struct-areaconstraint (action) #:mutable #:transparent)

;struct-area <struct>
(struct struct-area (from-x to-x from-y to-y from-z to-z)  #:mutable #:transparent)

;simulationarea <struct-area>
(define simulationarea (struct-area 0 100
                                    0 100
                                    -100 0))

;set-simulation-area <method>
(define (set-simulation-area #:width [east '(0 100)]
                             #:height [up '(0 100)]
                             #:depth [north '(-100 0)])
  (set-struct-area-from-x! simulationarea (first east))
  (set-struct-area-to-x! simulationarea (second east))
  (set-struct-area-from-y! simulationarea (first north))
  (set-struct-area-to-y! simulationarea (second north))
  (set-struct-area-from-z! simulationarea (first up))
  (set-struct-area-to-z! simulationarea (second up)))

;add-area-constraint <method>
(define (add-area-constraint #:action [action 'stop])
  (set! constraints
        (append constraints
                (if
                 (member action (list (quote skip) (quote stop) (quote error)))
                 (list (struct-areaconstraint action))
                 (error "Error! Wrong keyword:" action)))))

;check-constraint <method>
(define (check-constraint constraint #:display [disp 'no])
  (let ([objects (append drones threats obstacles)])
    (cond
      [(struct-motionconstraint? constraint)
       (let* ([data (struct->list constraint)]
              [type (first data)]
              [param (second data)]
              [axis (third data)]
              [minlimit (fourth data)]
              [maxlimit (fifth data)]
              [action (sixth data)])
         (apply +
                (map
                 (lambda (object)
                   (let ([obj-state (send object get-state #:param param #:axis axis)]
                         [obj-id (get-field id object)])
                     (if (or (< obj-state minlimit)
                             (> obj-state maxlimit))
                         (case action
                           [(quote limit) (begin
                                            (cond
                                              [(< obj-state minlimit)
                                               (send object set-state minlimit #:param param #:axis axis)]
                                              [(> obj-state maxlimit)
                                               (send object set-state maxlimit #:param param #:axis axis)])
                                            (when (equal? disp (quote yes))
                                              (printf "Motion constraint - limit! Object: ~a; Parameter: ~a; Axis: ~a\n" obj-id param axis))
                                            0)]
                           [(quote skip) (begin
                                           (when (equal? disp (quote yes))
                                             (printf "Motion constraint - skip! Object: ~a; Parameter: ~a; Axis: ~a\n" obj-id param axis))
                                           0)]
                           [(quote stop) (begin
                                           (when (equal? disp (quote yes))
                                             (printf "Motion constraint - stop! Object: ~a; Parameter: ~a; Axis: ~a\n" obj-id param axis))
                                           1)]
                           [(quote error) (begin
                                            (error "Motion constraint - error! Object: ~a; Parameter: ~a; Axis: ~a\n" obj-id param axis)
                                            1)]
                           [else (error "Error! Wrong action in constraint: " action)])
                         0)))
                 (get-type type))))]
      ;ABSOLUTELY BAD PERFORMANCE
      [(struct-hitconstraint? constraint)
       (let ([action (struct-hitconstraint-action constraint)])
         (apply +
                (for*/list ([test1 objects]
                            [test2 objects])
                  (define errval
                    (if (not (equal? test1 test2))
                        (let* ([position (send test1 get-state #:param 'position)]
                               [origin (pos (first position) (second position) (third position))])
                          (apply +
                                 (for/list ([zone (send test1 get-structure-full)])
                                   (apply +
                                          (for/list ([point zone])
                                            (let* ([vert (pos (first point) (second point) (third point))])
                                              (apply +
                                                      (for*/list ([thr-zone (send test2 get-structure-full)]
                                                                  [thr-point1 thr-zone]
                                                                  [thr-point2 thr-zone]
                                                                  [thr-point3 thr-zone])
                                                        (if (not (or
                                                                  (equal? thr-point1 thr-point2)
                                                                  (equal? thr-point1 thr-point3)
                                                                  (equal? thr-point2 thr-point3)))
                                                            (let* ([thr-vert1 (pos (first thr-point1) (second thr-point1) (third thr-point1))]
                                                                   [thr-vert2 (pos (first thr-point2) (second thr-point2) (third thr-point2))]
                                                                   [thr-vert3 (pos (first thr-point3) (second thr-point3) (third thr-point3))]
                                                                   [thr-tri (triangle thr-vert1 thr-vert2 thr-vert3)]
                                                                   [intersect (trace thr-tri origin vert)])
                                                              (if (not intersect)
                                                                  0
                                                                  (let ([dist-fig (pos-dist origin vert)]
                                                                        [dist-thr (pos-dist origin intersect)])
                                                                    (if (> dist-fig dist-thr) 1 0))))
                                                            0)))))))))
                        0))
                  (if (not (zero? errval))
                      (case action
                        [(quote skip) (begin
                                        (when (equal? disp (quote yes))
                                          (printf "Hit constraint - skip! Objects: ~a; ~a\n" (get-field id test1) (get-field id test2)))
                                        0)]
                        [(quote stop) (begin
                                        (when (equal? disp (quote yes))
                                          (printf "Hit constraint - stop! Objects: ~a; ~a\n" (get-field id test1) (get-field id test2)))
                                        1)]
                        [(quote error) (begin
                                         (error "Hit constraint - error! Objects: ~a; ~a\n" (get-field id test1) (get-field id test2))
                                         1)]
                        [else (error "Error! Wrong action in constraint: " action)])
                      0))))]
      [(struct-areaconstraint? constraint)
       (let ([action (struct-areaconstraint-action constraint)])
         (apply +
                (map
                 (lambda (object)
                   (let* ([posn (send object get-state #:param 'position #:axis 'all)]
                          [posn-x (first posn)]
                          [posn-y (second posn)]
                          [posn-z (third posn)]
                          [arg-x (if (or
                                      (< posn-x (struct-area-from-x simulationarea))
                                      (> posn-x (struct-area-to-x simulationarea)))
                                     1 0)]
                          [arg-y (if (or
                                      (< posn-y (struct-area-from-y simulationarea))
                                      (> posn-y (struct-area-to-y simulationarea)))
                                     1 0)]
                          [arg-z (if (or
                                      (< posn-z (struct-area-from-z simulationarea))
                                      (> posn-z (struct-area-to-z simulationarea)))
                                     1 0)]
                          [arg-sum (+ arg-x arg-y arg-z)]
                          [obj-id (get-field id object)])
                     (if (zero? arg-sum)
                         0
                         (case action
                           [(quote skip) (begin
                                           (when (equal? disp (quote yes))
                                             (printf "Area constraint - skip! Object: ~a; Position: ~a\n" obj-id posn))
                                           0)]
                           [(quote stop) (begin
                                           (when (equal? disp (quote yes))
                                             (printf "Area constraint - stop! Object: ~a; Position: ~a\n" obj-id posn))
                                           1)]
                           [(quote error) (begin
                                            (error "Area constraint - error! Object: ~a; Position: ~a\n" obj-id posn)
                                            1)]
                           [else (error "Error! Wrong action in constraint: " action)]))))
                 objects)))]
      [else (error "Error! Wrong constraint type: " constraint)])))



; Supporting functions
;--------------------------------
(define elapsed-time 0)

(define (set-timer)
  (set! elapsed-time (current-seconds)))

(define (stop-timer)
  (let*
      ([time-used (- (current-seconds) elapsed-time)]
       [time-date (seconds->date time-used)]
       [days (- (date-day time-date) 1)]
       [hours (date-hour time-date)]
       [minutes (date-minute time-date)]
       [seconds (date-second time-date)])
    (printf "Time passed: ~a days, ~a hours, ~a minutes, ~a seconds.\n"
            days hours minutes seconds)))
            
