#lang racket

;(require environmentobject%)
(require "environmentobject.rkt")

;(provide kinematicthreat%)
(provide kinematicthreat%)

;kinematicthreat% -|> environmentobject% <class>
(define kinematicthreat%
  (class environmentobject%
    (super-new)

    (inherit get-state
             set-state
             write-archive
             display-state
             DCM)
    (inherit-field samplingtime
                   currenttime)
    
    ;simulate <method>
    (define/public (simulate-motion . arg)
      (let
          ([linposn (get-state #:param 'position)]
           [linvel (get-state #:param 'velocity)]
           [linaccel (get-state #:param 'acceleration)]
           [angposn (get-state #:param 'angle)]
           [angvel (get-state #:param 'angvel)]
           [angaccel (get-state #:param 'angaccel)]
           [t samplingtime])
        (define vel-fun
          (lambda (v a) (+ v (* a t))))
        (define pos-fun
          (lambda (p v a) (+ p (* v t) (* a t t 0.5))))
        (set-state #:param 'position (map pos-fun linposn
                                          (DCM linvel #:direction 'body-to-earth)
                                          (DCM linaccel  #:direction 'body-to-earth)))
        (set-state #:param 'angle (map pos-fun angposn
                                       (DCM angvel  #:direction 'body-to-earth)
                                       (DCM angaccel #:direction 'body-to-earth)))
        (set-state #:param 'velocity (map vel-fun linvel linaccel))
        (set-state #:param 'angvel (map vel-fun angvel angaccel))
        (set! currenttime (+ currenttime samplingtime))
        (write-archive)))

    ;control-behaviour <method>
    
    ))