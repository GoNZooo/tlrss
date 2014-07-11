#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         base-path
         rss-url
         downloads)

(define-syntax-rule (module-begin e ...)
  (#%module-begin e ...))

(define-for-syntax (def-provide-symbol stx sym value)
  (with-syntax ([def-symbol (format-id stx "~a" sym)]
                [rvalue (syntax->datum value)])
    #'(begin
        (define def-symbol rvalue)
        (provide def-symbol))))

(define-syntax (base-path stx)

  (define-syntax-class pathstr
    (pattern p
             #:fail-unless
             (or (path-string? (syntax->datum #'p))
                 (path? (syntax->datum #'p)))
             "Expected path-string or built path"
             #:fail-unless
             (directory-exists?
              (expand-user-path
               (build-path
                (syntax->datum #'p))))
             "Directory must exist"))
  
  (syntax-parse stx
   [(_ spath:pathstr)
    (with-syntax ([built-path
                   (expand-user-path
                    (build-path
                     (syntax->datum #'spath)))])
      (def-provide-symbol stx 'user-base-path #'built-path))]))

(define-syntax (rss-url stx)  
  (syntax-parse stx
    [(_ surl:str)
     (def-provide-symbol stx 'user-rss-url #'surl)]))

(define-syntax (downloads stx)
  (define-syntax-class rxstr
    (pattern s #:fail-unless (string? (syntax->datum #'s)) "String needed"))
  
  (syntax-parse stx
    [(_ sregex:rxstr ...)
     (with-syntax ([def-symbol (format-id stx "~a" 'user-downloads)]
                   [compiled (map pregexp (syntax->datum #'(sregex ...)))])
       #'(begin
           (define def-symbol (quote compiled))
           (provide def-symbol)))]))

(module+ main
  (base-path "~")
  (rss-url "HejHej")
  (downloads
   "The.Big.Bang.Theory.S07E\\d\\d.720p.*"
   "Hells.Kitchen.US.S12E\\d\\d.*x264.*"
   "Game.of.Thrones.S\\d\\dE\\d\\d.720p.*")
  (printf "User base path: ~a~nRSS URL: ~a~nDownloads: ~a~n"
          user-base-path
          user-rss-url
          user-downloads))
