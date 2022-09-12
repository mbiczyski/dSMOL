#lang racket

(require "dSMOL/main.rkt")
(require "test_traits.rkt")
(require racket/trait)

(define-optimisation-parameter c 1 0.5 (list 0 1))
(define-optimisation-parameter d 2 0.5 (list 0 c))
(define-optimisation-parameter r 3 1 (list 0.25 2))

(define mission-ally
  (trait
   (define/override (control-mission . arg)
     (list (list 20 0 -1.5)
           (list c d 0.01 0.017 r)))))

(define mission-enemy
  (trait
   (define/override (control-mission . arg)
       (list 5 1.5 -90))))

;DEFINE CONSTRAINTS
(add-motion-constraint "ally" (list (degrees->radians -35) (degrees->radians 35)) #:param 'angle #:axis 'y #:action 'limit)
(add-motion-constraint "ally" (list (degrees->radians -35) (degrees->radians 35)) #:param 'angle #:axis 'x #:action 'limit)
(add-motion-constraint "ally" (list (degrees->radians -250) (degrees->radians 250)) #:param 'angvel #:axis 'all #:action 'limit)
(add-motion-constraint "ally" (list -10 10) #:param 'velocity #:axis 'all #:action 'limit)

(set-simulation-area #:width '(-25 25) #:depth '(-25 25) #:height '(-10 0))

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

;ADD OBJECTS TO ENVIRONMENT
(add-drone "leader"
           #:type "ally"
           #:sampling 0.01
           #:position (list -7 0 -1.5)
           #:velocity (list 6 0 0)
           #:mission mission-ally
           #:coordination coordination-trait
           #:collision collision-trait
           #:autopilot force-control-trait
           #:actuator actuator-trait
           #:dynamics dynamics-trait
           #:structure structure-iris)

(add-drone "left-follow"
           #:type "ally"
           #:sampling 0.01
           #:position (list -9 -2 -1.5)
           #:velocity (list 6 0 0)
           #:mission mission-ally
           #:coordination coordination-trait
           #:collision collision-trait
           #:autopilot force-control-trait
           #:actuator actuator-trait
           #:dynamics dynamics-trait
           #:structure structure-iris)

(add-drone "right-follow"
           #:type "ally"
           #:sampling 0.01
           #:position (list -9 2 -1.5)
           #:velocity (list 6 0 0)
           #:mission mission-ally
           #:coordination coordination-trait
           #:collision collision-trait
           #:autopilot force-control-trait
           #:actuator actuator-trait
           #:dynamics dynamics-trait
           #:structure structure-iris)

(add-drone "back-follow"
           #:type "ally"
           #:sampling 0.01
           #:position (list -11 0 -1.5)
           #:velocity (list 6 0 0)
           #:mission mission-ally
           #:coordination coordination-trait
           #:collision collision-trait
           #:autopilot force-control-trait
           #:actuator actuator-trait
           #:dynamics dynamics-trait
           #:structure structure-iris)

(add-threat "rogue-one"
            #:type "enemy"
            #:position (list 0 7.5 -1.5)
            #:angle (list 0 0 (degrees->radians -90))
            #:velocity (list 5 0 0)
            #:mission mission-enemy
            #:autopilot control-trait
            #:actuator actuator-trait
            #:dynamics dynamics-trait
            #:structure structure-iris)

;BEGIN OPTIMISATION
(optimise (method-simulated-annealing 10 0.88 1) (fitness-miss-distance "ally" "enemy") 2.5 #:minmax 'max)