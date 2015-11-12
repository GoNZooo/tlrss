#lang racket/base

(require scribble/srcdoc

         gonz/human-time

         "db.rkt"
         "xml-parse.rkt"
         "configuration.rkt")

(define (seen?/title title)
  (db/get/seen/title title))

(define (seen?/guid guid)
  (db/get/seen/guid guid))
