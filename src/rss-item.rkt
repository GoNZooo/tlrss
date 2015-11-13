#lang racket/base

(provide item-title
         item-category
         item-guid
         item-comments
         item-added
         item-link)

(define (item-title item-pair)
  (hash-ref (cdr item-pair)
            'item))

(define (item-category item-pair)
  (hash-ref (cdr item-pair)
            'category))

(define (item-guid item-pair)
  (hash-ref (cdr item-pair)
            'guid))

(define (item-comments item-pair)
  (hash-ref (cdr item-pair)
            'comments))

(define (item-added item-pair)
  (hash-ref (cdr item-pair)
            'added))

(define (item-link item-pair)
  (hash-ref (cdr item-pair)
            'link))
