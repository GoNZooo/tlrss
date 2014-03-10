#lang racket

(require net/url
         xml
         racket/date

         "xml-parse.rkt"
         "configuration.rkt")

(provide url->file)

(define (url->file url [base-path ""] [filename ""])
  (define (get-filename)
     (if (equal? filename "")
         (last (string-split url "/"))
         filename))
  
  ; Open the input-port from the URL, copy
  ; everything to the output port for the file
  (copy-port (get-pure-port (string->url url))
             (open-output-file (string-append base-path
                                              (get-filename))
                               #:exists 'replace)))

(define (get-rss-data)
  (define (generate-xexpr)
    (define (url->port)
      (get-pure-port (string->url user-rss-url)))
    
    (xml->xexpr
     ((eliminate-whitespace '(item))
      (document-element (read-xml (url->port))))))

  (generate-xexpr))

(define (match-rss-item i downloaded)
  (define (downloaded?)
    (not (false? (member (rss-item-link i) downloaded))))
  (define (match-downloads set)
    (cond
     [(null? set) #f]
     [(downloaded?)
      #f]
     [(regexp-match (cdr (first set))
                    (rss-item-title i))
      #t]
     [else
      (match-downloads (rest set))]))

  (match-downloads downloads))

(define (fetch-match-loop [downloaded '()])
  (define (compose-current-time)
    (define (leading-zero n)
      (if (< n 10)
          (string-append "0" (number->string n))
          n))
    
    (let ([cd (current-date)])
      (format "~a-~a-~a ~a:~a:~a"
              (date-year cd)
              (leading-zero (date-month cd))
              (leading-zero (date-day cd))
              (leading-zero (date-hour cd))
              (leading-zero (date-minute cd))
              (leading-zero (date-second cd)))))
  
  (begin
    (with-handlers ([exn:fail:network?
                     (lambda (e)
                       (printf "~a - Failed to connect~n"
                               (compose-current-time)))])
      (begin
        (for-each
         (lambda (rss-entry)
           (begin
             (when (match-rss-item rss-entry downloaded)
               (begin
                 (printf "~a - Matched: ~a~n"
                         (compose-current-time)
                         (rss-item-title rss-entry))
                 (url->file (rss-item-link rss-entry)
                            user-base-path))
               (set! downloaded
                     (cons (rss-item-link rss-entry)
                           downloaded)))))
         (get-items (get-rss-data)))
        (printf "~a - Fetched rss.~n"
                (compose-current-time))))
    (sleep 310)
    (fetch-match-loop downloaded)))

(module+ main
  (fetch-match-loop))