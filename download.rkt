#lang racket/base

(require net/url
         xml
         racket/date
         racket/list
         racket/string
         racket/port
         racket/bool
         racket/contract

         "macros/human-time.rkt"
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

    (define (copy-to-file/close)

      (let ([ip (get-pure-port (string->url url))]
            [op (open-output-file (build-path base-path
                                              (get-filename))
                                  #:exists 'replace)])

        ;; copy-port doesn't return, so we can put the
        ;; closes under without them becoming unreachable
        (copy-port ip op)
        (close-input-port ip)
        (close-output-port op)))
    
    (copy-to-file/close))
  
  (define/contract (match-rss-item? i)
    (rss-item? . -> . boolean?)
    
    (define/contract (match-downloads set)
      ((listof regexp?) . -> . boolean?)

      (define (downloaded? [dl-set downloaded])
        (cond
         [(null? dl-set) #f]
         [(equal? (car (first dl-set))
                  (rss-item-link i))
          #t]
         [else
          (downloaded? (rest dl-set))]))
      
      (cond
       ;; If the set is empty, we return false.
       [(null? set) #f]
       ;; If the link is already in downloaded links
       ;; we return false.
       [(downloaded?)
        #f]
       ;; If the regexp we have in 'downloads' matches
       ;; the name of the current rss-item we have a match.
       [(regexp-match (car set)
                      (rss-item-title i))
        #t]
       ;; Otherwise we move on in the download-set.
       [else
        (match-downloads (cdr set))]))

    ;; Note that 'downloads' refers to the
    ;; user-defined variable in 'configuration.rkt'.
    (match-downloads user-downloads))
  
  (define (compose-current-time)
    ;; Formats current time for log/output.
    
    (define/contract (leading-zero n)
      (number? . -> . string?)
      
      (if (< n 10)
          (string-append "0" (number->string n))
          (number->string n)))
    
    (let ([cd (current-date)])
      (format "~a-~a-~a ~a:~a:~a"
              (date-year cd)
              (leading-zero (date-month cd))
              (leading-zero (date-day cd))
              (leading-zero (date-hour cd))
              (leading-zero (date-minute cd))
              (leading-zero (date-second cd)))))

  (define/contract (cleanse set [output '()])
    ((list?) (list?) . ->* . list?)

    (define/contract (too-old?)
      (-> boolean?)
      
      (define/contract (time-diff)
        (-> number?)
        
        (- (current-seconds)
           (cdr (first set))))

      (> (time-diff)
         (human-time 24h)))
    
    (cond
     [(null? set) output]
     [(too-old?)
      (cleanse (rest set)
               output)]
     [else
      (cleanse (rest set)
               (cons (first set)
                     output))]))

  (define (check-rss-item rss-entry)
    ;; Checks an RSS item for matches

    (define (print-match)
      (printf "~a - Matched: ~a~n"
              (compose-current-time)
              (rss-item-title rss-entry)))

    (define (add-item-to-downloaded)
      (set! downloaded
            (cons (cons (rss-item-link rss-entry) (current-seconds))
                  downloaded)))
    
    (when (match-rss-item? rss-entry)
      (print-match)
      (url->file (rss-item-link rss-entry)
                 user-base-path)
      (add-item-to-downloaded)))
  
  (with-handlers ([exn:fail:network?
                   (lambda (e)
                     (printf "~a - Failed to connect~n"
                             (compose-current-time)))]
                  [exn:fail:read?
                   (lambda (e)
                     (printf "~a - Failed to read XML~n"
                             (compose-current-time)))])
    (for-each check-rss-item
              (get-items (get-rss-data user-rss-url)))

    (printf "~a - Fetched rss.~n"
            (compose-current-time)))
  
  (collect-garbage)
  (sleep (human-time 5m10s))

  (fetch-match-loop (cleanse downloaded)))

(module+ main
  (fetch-match-loop))
