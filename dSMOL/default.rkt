#lang racket

(require racket/trait)


(provide name-state
         name-data
         name-control
         name-parameters
         name-archive
         name-structure
         name-motors
         name-fitness
         default-init-drone-trait
         default-init-threat-trait
         default-mission-trait
         default-coordination-trait
         default-collision-trait
         default-autopilot-trait
         default-actuator-trait
         default-dynamics-trait
         default-aerodynamics-trait
         default-motion-trait)

; Default File Names
;--------------------------------
(define name-state "state")
(define name-data "data")
(define name-control "control")
(define name-parameters "parameters")
(define name-archive "history")
(define name-structure "structure")
(define name-motors "motors")
(define name-fitness "fitness")

; Default Traits
;--------------------------------
(define default-init-drone-trait
  (trait
   (inherit write-state
            write-motors
            write-archive
            write-structure
            set-state)
   (inherit-field id
                  currenttime
                  samplingtime
                  init-currenttime
                  init-state)
   (define/override (control-init . arg)
          (set! currenttime init-currenttime)
     (set-state (list-ref init-state 0) (list-ref init-state 1) (list-ref init-state 2) #:param 'position)
     (set-state (list-ref init-state 3) (list-ref init-state 4) (list-ref init-state 5) #:param 'velocity)
     (set-state (list-ref init-state 6) (list-ref init-state 7) (list-ref init-state 8) #:param 'acceleration)
     (set-state (list-ref init-state 9) (list-ref init-state 10) (list-ref init-state 11) #:param 'angle)
     (set-state (list-ref init-state 12) (list-ref init-state 13) (list-ref init-state 14) #:param 'angvel)
     (set-state (list-ref init-state 15) (list-ref init-state 16) (list-ref init-state 17) #:param 'angaccel)
     (write-state #:exists 'truncate)
     (write-motors #:exists 'truncate)
     (write-archive #:exists 'truncate)
     (write-structure #:exists 'truncate)
     (let
         ([filename (string-append "multi_model_state_" id ".mat")])
       (when (file-exists? filename) (delete-file filename))))))

(define default-init-threat-trait
  (trait
   (inherit write-state
            write-archive
            write-structure
            set-state)
   (inherit-field id
                  currenttime
                  samplingtime
                  init-currenttime
                  init-state)
   (define/override (control-init . arg)
          (set! currenttime init-currenttime)
     (set-state (list-ref init-state 0) (list-ref init-state 1) (list-ref init-state 2) #:param 'position)
     (set-state (list-ref init-state 3) (list-ref init-state 4) (list-ref init-state 5) #:param 'velocity)
     (set-state (list-ref init-state 6) (list-ref init-state 7) (list-ref init-state 8) #:param 'acceleration)
     (set-state (list-ref init-state 9) (list-ref init-state 10) (list-ref init-state 11) #:param 'angle)
     (set-state (list-ref init-state 12) (list-ref init-state 13) (list-ref init-state 14) #:param 'angvel)
     (set-state (list-ref init-state 15) (list-ref init-state 16) (list-ref init-state 17) #:param 'angaccel)
     (write-state #:exists 'truncate)
     (write-archive #:exists 'truncate)
     (write-structure #:exists 'truncate)
     (let
         ([filename (string-append "multi_model_state_" id ".mat")])
       (when (file-exists? filename) (delete-file filename))))))

(define default-mission-trait
  (trait
   (define/override (control-mission . arg)
     (flatten arg))))

(define default-coordination-trait
  (trait
   (define/override (control-coordination . arg)
     (flatten arg))))

(define default-collision-trait
  (trait
   (define/override (control-collision . arg)
     (flatten arg))))

(define default-autopilot-trait
  (trait
   (define/override (control-autopilot . arg)
     (flatten arg))))

(define default-actuator-trait
  (trait
   (define/override (control-actuator . arg)
     (flatten arg))))

(define default-dynamics-trait
  (trait
   (define/override (control-dynamics . arg)
     (flatten arg))))

(define default-aerodynamics-trait
  (trait
   (define/override (control-aerodynamics . arg)
     (flatten arg))))

(define default-motion-trait
  (trait
   (define/override (control-motion . arg)
     (flatten arg))))