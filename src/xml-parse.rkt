#lang racket/base

(require racket/match
         xml
         net/url

         gonz/html-tree
         gonz/with-matches

         "configuration.rkt"
         "rss-item.rkt")

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
  (define (extract-guid guid-url)
    (with-matches #px"https://www.torrentleech.org/torrent/(\\d*)"
                  guid-url
                  (m 1)))

  (if (null? item-components)
    (cons (hash-ref output-hash 'guid) output-hash)
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
      [`(guid () ,guid-url)
        (item-components->item-hash (cdr item-components)
                                    (hash-set output-hash
                                              'guid
                                              (string->number
                                                (extract-guid guid-url))))]
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

(define (make-rss-item/struct i)
  (define data (cdr i))
  (item (car i)
        (torrent (hash-ref data
                           'title)
                 (hash-ref data
                           'category)
                 (hash-ref data
                           'guid)
                 (hash-ref data
                           'comments)
                 (hash-ref data
                           'added)
                 (hash-ref data
                           'link))))

(module+ main
  (require racket/pretty)

  (pretty-print
    (map make-rss-item/struct (rss-items))
    ))
