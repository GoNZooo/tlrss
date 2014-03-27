#lang racket

(require net/url
         xml
         racket/date

         "xml-parse.rkt"
         "configuration.rkt")

(define (fetch-match-loop [downloaded '()])
  ;; The main function that fetches RSS items
  ;; and loops through them to find matches
  
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
    ;; Function that fetches RSS and generates
    ;; an x-expression from it.
    
    (define (generate-xexpr)
      (define (url->port)
        (get-pure-port (string->url user-rss-url)))
      
      (xml->xexpr
       ((eliminate-whitespace '(item))
        (document-element (read-xml (url->port))))))

    (generate-xexpr))
  
  (define (match-rss-item? i)
    ;; Function that matches an item against
    ;; user-specified downloads to look for.
    
    (define (match-downloads set)
      (define (downloaded?)
        (not (false? (member (rss-item-link i) downloaded))))
      
      (cond
       ;; If the set is empty, we return false.
       [(null? set) #f]
       ;; If the link is already in downloaded links
       ;; we return false.
       [(downloaded?)
        #f]
       ;; If the regexp we have in 'downloads' matches
       ;; the name of the current rss-item we have a match.
       [(regexp-match (cdr (first set))
                      (rss-item-title i))
        #t]
       ;; Otherwise we move on in the download-set.
       [else
        (match-downloads (rest set))]))

    ;; Note that 'downloads' refers to the
    ;; user-defined variable in 'configuration.rkt'.
    (match-downloads downloads))
  
  (define (compose-current-time)
    ;; Formats current time for log/output.
    
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
             (when (match-rss-item? rss-entry)
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
    (collect-garbage)
    (sleep 310)

    ;; TODO: Log time of addition to downloaded
    ;; At each iteration, loop through and remove
    ;; the ones that are too old.
    (fetch-match-loop downloaded)))

(module+ main
  (fetch-match-loop))