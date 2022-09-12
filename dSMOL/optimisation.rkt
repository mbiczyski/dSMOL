#lang racket

(require "default.rkt")
(require "simulation.rkt")

(provide define-optimisation-parameter
         set-current-parameters
         get-current-parameters
         get-current-bounds
         update-current-parameters
         set-current-fitness
         get-current-fitness
         write-fitness
         optimise
         set-optimal
         set-optimal-paths
         set-optimal-parameters
         get-optimal-parameters
         set-optimal-fitness
         get-optimal-fitness)

;currparameters <list>
(define currparameters empty)

;currbounds <list>
(define currbounds empty)

;optparameters <list>
(define optparameters empty)

;tempparameters <list>
(define tempparameters empty)

;tempparameters <list>
(define currfitness +inf.0)

;optfitness <number>
(define optfitness +inf.0)

; Parametrisation
;--------------------------------
;set-optimisation-parameter <macro>
(define-syntax-rule (define-optimisation-parameter id num init bounds)
  (define-syntax id
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [id (identifier? (syntax id)) (syntax (get-val (sub1 num) init bounds))]
         [(set! id e) (syntax (put-val! (sub1 num) e bounds))])))))

;set-current-parameters <method>
(define (set-current-parameters #:source [src 'optimal] #:target [tgt 'current] . lst)
  (let ([srcparam
         (case src
           [(quote optimal) optparameters]
           [(quote current) currparameters]
           [(quote temporary) tempparameters]
           [(quote external) lst]
           [else (error "Error! Wrong keyword:" src)])])
    (case tgt
           [(quote optimal) (set! optparameters srcparam)]
           [(quote current) (set! currparameters srcparam)]
           [(quote temporary) (set! tempparameters srcparam)]
           [else (error "Error! Wrong keyword:" tgt)])))

;get-current-parameters <method>
(define (get-current-parameters #:source [src 'current])
  (let ([srcparam
         (case src
           [(quote current) currparameters]
           [(quote temporary) tempparameters]
           [else (error "Error! Wrong keyword:" src)])])
  srcparam))

;get-current-bounds <method>
(define (get-current-bounds)
  currbounds)

;update-current-parameters <method>
(define (update-current-parameters procs #:source [src 'optimal])
  (let ([srcparam
         (case src
           [(quote optimal) optparameters]
           [(quote current) currparameters]
           [(quote temporary) tempparameters]
           [else (error "Error! Wrong keyword:" src)])])
    (set! currparameters
          (for/list ([ii (in-range (length srcparam))])
            ((list-ref procs ii) (list-ref srcparam ii))))))

; Fitness Function
;--------------------------------
;set-current-fitness <method>
(define (set-current-fitness val)
  (set! currfitness val))

;get-current-fitness <method>
(define (get-current-fitness)
  currfitness)

;write-fitness <method>
(define (write-fitness #:exists [mode 'append])
  (let
      ([file-string (string-append name-fitness ".csv")]
       [data-lst (append
                  (list (get-optimal-fitness))
                  (list (get-current-fitness))
                  (get-current-parameters))])
    (display-lines-to-file (drop-right data-lst 1) file-string #:separator #"," #:exists mode)
    (display-to-file (last data-lst) file-string #:exists 'append)
    (display-to-file "\n" file-string #:exists 'append)))

; Optimisation
;--------------------------------
;optimise <method>
(define (optimise met fit maxtime #:minmax [minmax 'min])
  (begin
    (set-timer)
    (printf "Optimisation started!\n")
    (simulate-all maxtime #:mode 'time #:display 'none)
    (set-current-fitness (fit))
    (set-optimal-parameters)
    (set-optimal-fitness (get-current-fitness))
    (set-optimal-paths)
    (write-fitness #:exists 'truncate)
    (stop-timer)
    (printf "Initial parameters: ~a; Initial fitness: ~a\n" (get-current-parameters) (get-current-fitness))
    (met fit maxtime minmax)
    (stop-timer)
    (printf "Optimal parameters: ~a; Optimal fitness: ~a\n" (get-optimal-parameters) (get-optimal-fitness))
    ))

;set-optimal <method>
(define (set-optimal proc #:minmax [minmax 'min])
  (let ([old optfitness]
        [new (proc)]
        [oper (case minmax
                [(quote min) <]
                [(quote max) >]
                [else (error "Error! Wrong keyword:" minmax)])])
    (when (oper new old) (begin
                        (set-optimal-parameters)
                        (set-optimal-fitness new)
                        (set-optimal-paths)))))

;set-optimal-paths <method>
(define (set-optimal-paths)
  (let* ([find-proc (lambda (path)
                      (regexp-match-exact?
                       (regexp (string-append name-archive "_.*\\.csv"))
                       (file-name-from-path path)))]
         [change-proc (lambda (path)
                        (build-path
                         (if (path-only path) (path-only path) (current-directory))
                         (string-append "optimal_"
                                        (path->string (file-name-from-path path)))))]
         [src-list
          (find-files find-proc)]
         [dest-list (map change-proc src-list)])

    (map
     (lambda (src dest) (copy-file src dest #t))
     src-list
     dest-list)))

;get-optimal-paths <method>

;set-optimal-parameters <method>
(define (set-optimal-parameters)
  (set! optparameters currparameters))

;get-optimal-parameters <method>
(define (get-optimal-parameters)
  optparameters)

;set-optimal-fitness <method>
(define (set-optimal-fitness val)
  (set! optfitness val))

;get-optimal-fitness <method>
(define (get-optimal-fitness)
  optfitness)

; Supporting functions
;--------------------------------
(define get-val
  (lambda (n i b)
    (if (> (length currparameters) n)
        (begin
          (when (empty? (list-ref currparameters n))
            (set! currparameters (list-set currparameters n i)))
          (set! currbounds (list-set currbounds n b))
          (list-ref currparameters n))
        (begin
          (set! currparameters
                (append currparameters (make-list (- n (length currparameters)) empty) (list i)))
          (set! currbounds
                (append currbounds (make-list (- n (length currbounds)) empty) (list b)))
          (list-ref currparameters n)))))

(define put-val!
  (lambda (n v b)
    (if (> (length currparameters) n)
        (begin
          (set! currparameters (list-set currparameters n v))
          (set! currbounds (list-set currbounds n b)))
        (begin 
          (set! currparameters
                (append currparameters (make-list (- n (length currparameters)) empty) (list v)))
          (set! currbounds
                (append currbounds (make-list (- n (length currbounds)) empty) (list b)))))))

