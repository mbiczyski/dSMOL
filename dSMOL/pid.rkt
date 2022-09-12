#lang racket

;(provide pid%)
(provide add-pid)

;pid% <class>
(define pid%
  (class object%
    (super-new)
    
    (init-field kp ;kp <field>
                ti ;ti <field>
                td ;td <field>
                dt ;dt <field>
                [tb 0] ;tb <field>
                [nis 10] ;nis <field>
                [minlim -inf.0] ;minlim <field>
                [maxlim +inf.0] ;maxlim <field>
                [init-I 0]
                [init-D 0])

    (define h (/ dt nis))
    (define ui init-I)
    (define ud init-D)
    (define up 0)
    (define u (+ ui ud up))
    
    (define prev-err 0)
    
    (define/private (set-ui err)
      (set! ui (+ ui
                  (if (or
                       (<= u minlim)
                       (>= u maxlim))
                      0
                      (* kp h err (/ ti))))))
    (define/private (set-ud err)
      (set! ud
            (if (zero? tb)
                (* kp td (- err prev-err) (/ h))
                (+ (* kp td (/ tb) (- err prev-err)) (* ud (- 1 (/ dt tb)))))))
    (define/private (set-up err)
      (set! up (* kp err)))
    (define/private (set-u)
      (set! u (+ ui ud up)))
    (define/private (set-prev-err err)
      (set! prev-err err))

    ;get-control <method>
    (define/public (get-control err)
      (begin
        (for ([ii (in-range nis)])
          (set-ui err))
        (set-ud err)
        (set-up err)
        (set-u)
        (set-prev-err err)
        (cond
          [(<= u minlim) minlim]
          [(>= u maxlim) maxlim]
          [else u])))))

;add-pid  <method>
(define (add-pid kprop tint tder time
                 #:NIS [repeats 10]
                 #:Tb [treal 0]
                 #:limits [lim '(-inf.0 +inf.0)]
                 #:init [init '(0 0)])
  (new pid%
       [kp kprop]
       [ti tint]
       [td tder]
       [dt time]
       [tb treal]
       [nis repeats]
       [minlim (first lim)]
       [maxlim (second lim)]
       [init-I (first init)]
       [init-D (second init)]))