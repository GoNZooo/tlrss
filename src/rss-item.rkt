#lang typed/racket/base

(provide item-title
         item-category
         item-guid
         item-comments
         item-added
         item-link
         (struct-out item)
         (struct-out torrent))

(struct torrent ([title : String]
                 [category : String]
                 [guid : Integer]
                 [comments : String]
                 [added : String]
                 [link : String])
        #:transparent)

(struct item ([guid : Integer] [data : torrent])
        #:transparent)

(: item-title (-> item String))
(define (item-title i)
  (torrent-title (item-data i)))

(: item-category (-> item String))
(define (item-category i)
  (torrent-category (item-data i)))

(: item-comments (-> item String))
(define (item-comments i)
  (torrent-comments (item-data i)))

(: item-added (-> item String))
(define (item-added i)
  (torrent-added (item-data i)))

(: item-link (-> item String))
(define (item-link i)
  (torrent-link (item-data i)))
