#lang racket

(require xml
         xml/path
         net/url)

(provide get-items
         (struct-out rss-item))

(struct rss-item (title link)
        #:transparent)

(define (get-items xml-data)
  (define (get-titles)
    (se-path*/list '(item title)
                   xml-data))
  (define (get-links)
    (se-path*/list '(item link)
                   xml-data))

  (define (combine-data titles links [output '()])
    (define (trim-left cdata)
      (substring cdata 9))
    (define (trim-right cdata)
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
