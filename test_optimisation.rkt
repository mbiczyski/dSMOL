#lang racket

(require "dSMOL/main.rkt")
(require "test_traits.rkt")
(require racket/trait)

(define tcrash 1)
(define v1 3)
(define v2 5)

(define-optimisation-parameter h1 1 0 (list -90 90))
(define-optimisation-parameter h2 2 0 (list -90 90))

(define p1 (list
            (- 50 (* tcrash v1 (sin (degrees->radians 45))))
            (- 50 (* tcrash v1 (cos (degrees->radians 45))))
            -0.1))
(define p2 (list
            (- 50 (* tcrash v2 (sin (degrees->radians 45))))
            (+ 50 (* tcrash v2 (cos (degrees->radians 45))))
            -0.1))
(define alt 1.5)

(define mission-1
  (trait
   (define/override (control-mission . arg)
     (let*
         ([set-speed v1]
          [set-alt alt]
          [set-heading h1])
       (list set-speed set-alt set-heading)))))

(define mission-2
  (trait
   (define/override (control-mission . arg)
     (let*
         ([set-speed v2]
          [set-alt alt]
          [set-heading h2])
       (list set-speed set-alt set-heading)))))

;DEFINE CONSTRAINTS
(add-motion-constraint "drone" (list (degrees->radians -90) (degrees->radians 90))
                       #:param 'angle
                       #:axis 'all
                       #:action 'limit)

(set-simulation-area #:width '(0 100)
                     #:height '(-10 0)
                     #:depth '(0 100))

(add-area-constraint #:action 'stop)
(add-hit-constraint #:action 'stop)

;DEFINE STRUCTURE
(define structure-iris (list '(0.2 -0.2 -0.05)
                             '(0.2 0.2 -0.05)
                             '(-0.2 0.2 -0.05)
                             '(-0.2 -0.2 -0.05)
                             '(0.2 -0.2 0.05)
                             '(0.2 0.2 0.05)
                             '(-0.2 0.2 0.05)
                             '(-0.2 -0.2 0.05)))

(define structure-car (list '(2.2 -0.95 -1.5)
                            '(2.2 0.95 -1.5)
                            '(-2.2 0.95 -1.5)
                            '(-2.2 -0.95 -1.5)
                            '(2.2 -0.95 0)
                            '(2.2 0.95 0)
                            '(-2.2 0.95 0)
                            '(-2.2 -0.95 0)))

;[0.016, 0, 0; 0, 0.016, 0; 0, 0, 0.031]
(define inertia-iris (list '(0.016 0 0)
                           '(0 0.016 0)
                           '(0 0 0.031)))

;ADD OBJECTS TO ENVIRONMENT
(add-drone "dro1"
           #:type "drone"
           #:sampling 0.01
           #:position p1
           #:dynamics dynamics-trait
           #:actuator actuator-trait
           #:autopilot control-trait
           #:mission mission-1
           #:structure structure-iris
           #:inertia inertia-iris)

(add-drone "dro2"
           #:type "drone"
           #:sampling 0.01
           #:position p2
           #:dynamics dynamics-trait
           #:actuator actuator-trait
           #:autopilot control-trait
           #:mission mission-2
           #:structure structure-iris
           #:inertia inertia-iris)

(add-obstacle "obst1"
           #:type "obstacle"
           #:position '(55 50 0)
           #:angle (list 0 0 (degrees->radians 180))
           #:velocity (list 5 0 0)
           #:structure structure-car)

;SETUP OPTIMISATION

;BEGIN OPTIMISATION
(optimise (method-simulated-annealing 100 0.88 1) (fitness-miss-distance "drone" "obstacle") 3 #:minmax 'max)

;CLEAR AFTER OPTIMISATION

; DONE dodac current parameters do fitness.csv
; DONE zrobic wykresy w matlabie
; DONE dodac reset do optimum
; DONE naprawic dynamic model
; (sampling time wywalic do innego pliku)
; (dodac metody do zmiany domyslnych nazw plikow)
; DONE boundaries przy optymalizacji dorzucic do definicji parametru
; DONE sensors
; DONE naprawic optymalizacje
; sprawdzic fitness function