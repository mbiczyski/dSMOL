#lang racket

(require "environment/environmentcontrol.rkt")
(require "simulation.rkt")
(require "optimisation.rkt")
(require math/distributions)

(provide method-simulated-annealing
         fitness-miss-distance)

(define (method-simulated-annealing initT alpha precision)
  (lambda (fitproc maxtime minmax)
    (do ([T initT (* alpha T)])
      ((< T precision) (get-optimal-parameters))
      (set-current-parameters #:source 'current #:target 'temporary)
      (let ([tempfit (fitproc)]
            [optfit (get-optimal-fitness)]
            [oper (case minmax
                    [(quote min) <]
                    [(quote max) >]
                    [else (error "Error! Wrong keyword:" minmax)])])
        (do ([temp1 (get-neighbour T initT) (get-neighbour T initT)])
          ((simulate-all maxtime #:mode 'time #:display 'none)
           (let* ([currfit (fitproc)]
                  [diff (- currfit tempfit)]
                  [prob (case minmax
                          [(quote min) (exp (- (/ diff T)))]
                          [(quote max) (exp (/ diff T))]
                          [else (error "Error! Wrong keyword:" minmax)])]
                  [rand (random)])
             (set-current-fitness currfit)
             (if (oper currfit optfit)
                 (begin
                   (set-optimal-parameters)
                   (set-optimal-fitness currfit)
                   (set-optimal-paths))
                 (unless (oper currfit tempfit)
                   (unless (> prob rand)
                     (set-current-parameters #:source 'optimal #:target 'current))))))
          (printf "Constraint broken! Current parameters: ~a;\n" (get-current-parameters))
          (set-current-parameters #:source 'temporary #:target 'current))
        (stop-timer)
        (printf "Current parameters: ~a; Current fitness: ~a; Temperature: ~a\n"
                (get-current-parameters) (get-current-fitness) T)
        (write-fitness)))))

(define (fitness-miss-distance type1 type2)
  (lambda ()
    (let*
        ([dist-proc
          (lambda (lst1 lst2)
            (sqrt (+ (sqr (- (list-ref lst1 0) (list-ref lst2 0)))
                     (sqr (- (list-ref lst1 1) (list-ref lst2 1)))
                     (sqr (- (list-ref lst1 2) (list-ref lst2 2))))))]
         [dist-list-proc
          (lambda (id1 id2)
            (map dist-proc (get-position-list id1) (get-position-list id2)))]
         [id-proc (lambda (type)
                    (map (lambda (obj) (get-field id obj)) (get-type type)))])
      (apply min
             (for*/list ([id1 (id-proc type1)]
                         [id2 (id-proc type2)])
               (apply min (dist-list-proc id1 id2)))))))

(define (get-neighbour T Tinit)
  (let* ([len (length (get-current-parameters))]
         [changepos (random len)]
         [lb (first (list-ref (get-current-bounds) changepos))]
         [ub (last (list-ref (get-current-bounds) changepos))]
         [db (- ub lb)]
         ;[scale (* (/ T Tinit) (/ db 4))]
         [scale T]
         [uptdproc (lambda (val) (sample (truncated-dist (cauchy-dist val scale) lb ub)))]
         [proclist (list-set (make-list len identity) changepos uptdproc)])
    (update-current-parameters proclist #:source 'current)))