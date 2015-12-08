#lang typed/racket/base

(provide item-title
         item-category
         item-guid
         item-comments
         item-added
         item-link
         (struct-out item))

(struct item ([guid : Integer]
              [title : String]
              [category : String]
              [comments : String]
              [added : String]
              [link : String])
  #:transparent)
