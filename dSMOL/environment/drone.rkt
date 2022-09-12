#lang racket

;(require dynamicobject%)
(require "dynamicobject.rkt")
(require "../default.rkt")
(require racket/struct)

;(provide drone%)
(provide drone%)

;drone% -|> dynamicobject% <class>
(define drone%
  (class dynamicobject%
    (super-new)

    (inherit write-data)
    (inherit-field id)

    (init-field [init-motors '(1000 1000 1000 1000 1000 1000 1000 1000)])

    ; Motors
    ;--------------------------------
    (struct motor-state (m1 m2 m3 m4 m5 m6 m7 m8) #:mutable #:transparent) ;struct-motors <struct>
    (define motors (motor-state (list-ref init-motors 0)
                                (list-ref init-motors 1)
                                (list-ref init-motors 2)
                                (list-ref init-motors 3)
                                (list-ref init-motors 4)
                                (list-ref init-motors 5)
                                (list-ref init-motors 6)
                                (list-ref init-motors 7)))

    ; DataManagement
    ;--------------------------------
    ;get-motors <method>
    (define/public (get-motors)
      (struct->list motors))

    ;set-motors <method>
    (define/public (set-motors . args)
      (let ([lst (flatten args)])
        (for ([num (in-naturals)]
              [mot (in-list lst)])
          ((hash-ref motors-key num) motors mot))))
    
    ;read-motors <method>
    (define/public (read-motors #:name [name-string name-motors] #:line [target 1])
      (let*
          ([file-string (string-append name-string  "_" id ".csv")]
           [line (list-ref (file->lines file-string) (sub1 target))]
           [data (map string->number (string-split line ","))])
        (set-motors data)))
    
    ;write-motors <method>
    (define/public (write-motors #:name [name-string name-motors] #:exists [mode 'truncate])
      (write-data (struct->list motors) #:name name-string #:exists mode))
    
    ;display-motors <method>
    (define/public (display-motors)
      (display "Motors: ")
      (displayln (struct->list motors)))

    ; Supporting functions
    ;--------------------------------
    (define motors-key (hash 0 set-motor-state-m1!
                             1 set-motor-state-m2!
                             2 set-motor-state-m3!
                             3 set-motor-state-m4!
                             4 set-motor-state-m5!
                             5 set-motor-state-m6!
                             6 set-motor-state-m7!
                             7 set-motor-state-m8!))

    ))