#lang at-exp racket/base

(require racket/contract

         gonz/human-time

         "db.rkt"
         "xml-parse.rkt"
         "configuration.rkt"

         scribble/srcdoc)

(require/doc racket/base
             scribble/manual
             (for-label racket))

(define (seen?/title title)
  (db/get/seen/title title))
(provide/doc
   (proc-doc/names
     seen?/title
     (-> string? boolean?)
     (x)
     @{Fuck.}))

(define (seen?/guid guid)
  (db/get/seen/guid guid))
