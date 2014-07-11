#lang racket/base

(require xml
         xml/path
         racket/list
         racket/contract
         net/url)

(provide get-items
         get-rss-data
         (struct-out rss-item))

(struct rss-item (title link)
        #:transparent)

(define/contract (get-items xml-data)
  (xexpr? . -> . (listof rss-item?))

  (define/contract (get-titles)
    (-> (or/c cdata? (listof cdata?)))
    
    (se-path*/list '(item title)
                   xml-data))
  
  (define/contract (get-links)
    (-> (or/c cdata? (listof cdata?)))
    
    (se-path*/list '(item link)
                   xml-data))

  (define/contract (combine-data titles links [output '()])
    (((listof cdata?) (listof cdata?))
     ((or/c list? (listof rss-item?)))
     . ->* . (or/c (and list?
                        null?)
                   (listof rss-item?)))
    
    (define/contract (trim-left cdata)
      (string? . -> . string?)
      
      (substring cdata 9))
    
    (define/contract (trim-right cdata)
      (string? . -> . string?)
      
      (substring cdata
                 0
                 (- (string-length cdata)
                    3)))
    
    (if (null? titles)
        output
        (combine-data (rest titles)
                      (rest links)
                      (cons
                       (rss-item
                        (trim-right (trim-left (cdata-string (first titles))))
                        (trim-right (trim-left (cdata-string (first links)))))
                       output))))

  (combine-data (get-titles) (get-links)))

(define/contract (get-rss-data rss-url)
  (string? . -> . xexpr?)
    
  (define/contract (generate-xexpr)
    (-> xexpr?)
    
    (xml->xexpr
     ((eliminate-whitespace '(item))
      (document-element (call/input-url (string->url rss-url)
                                        get-pure-port
                                        read-xml)))))
  (generate-xexpr))