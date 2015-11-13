#lang racket/base

(require racket/match
         xml
         net/url

         gonz/html-tree
         gonz/with-matches

         "configuration.rkt")

(provide rss-items)

(define (fetch-rss rss-url)
  (define-values
    (response headers input-port)
    (http-sendrecv/url (string->url rss-url)
                       #:headers
                       '("Accept: application/xml; charset=UTF-8"
                         "Accept-Encoding: UTF-8")))
  (read-xml input-port))

(define (rss->items rss-data)
  (find* (xml->xexpr (document-element rss-data))
         'item))

(define (rss-item-components item)
  (match item
    [`(item (,component ...))
      (filter (lambda (c)
                (and (not (string? c))
                     (not (null? c))))
              component)]))

(define (item-components->item-hash item-components [output-hash #hash()])
  (define (extract-cdata str)
    (with-matches #px"<!\\[CDATA\\[\\s*(.*)\\s*\\]\\]>" str (m 1)))

  (if (null? item-components)
    (hash (hash-ref output-hash 'guid) output-hash)
    (match (car item-components)
      [`(title () ,(cdata _ _ title))
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'title
                                              (extract-cdata title)))]
      [`(pubDate () ,pub-date)
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'added
                                              pub-date))]
      [`(category () ,category)
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'category
                                              category))]
      [`(guid () ,guid)
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'guid
                                              guid))]
      [`(comments () ,(cdata _ _ comments))
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'comments
                                              (extract-cdata comments)))]
      [`(link () ,(cdata _ _ link))
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'link
                                              (extract-cdata link)))]
      [_
        (item-components->item-hash (cdr item-components)
                                    output-hash)])))

(define (rss-items #:rss-url [rss-url user-rss-url])
  (map (compose1 item-components->item-hash rss-item-components)
       (rss->items (fetch-rss rss-url))))

(module+ main
  (require racket/pretty)

  (pretty-print
    (rss-items)
    ))
