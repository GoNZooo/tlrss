#lang racket/base

(require (for-syntax racket/base
                     racket/bool
                     racket/syntax
                     syntax/parse))

(provide human-time)

(define-syntax (human-time stx)
  (define (parse-time time)
    (define (get-hours t)
      (let ([r (regexp-match #px"(\\d*)[Hh]" time)])
        (if (false? r)
            0
            (string->number (list-ref r 1)))))
    (define (get-minutes t)
      (let ([r (regexp-match #px"(\\d*)[Mm]" time)])
        (if (false? r)
            0
            (string->number (list-ref r 1)))))
    (define (get-seconds t)
      (let ([r (regexp-match #px"(\\d*)[Ss]" time)])
        (if (false? r)
            0
            (string->number (list-ref r 1)))))

    (+ (* (get-hours time) 3600)
       (* (get-minutes time) 60)
       (get-seconds time)))
    
  (syntax-parse
   stx
   [(ht time:id)
    (with-syntax ([time-in-seconds
                   (parse-time (symbol->string (syntax->datum #'time)))])
      #'time-in-seconds)]))

(module+ main
  (human-time 24h))