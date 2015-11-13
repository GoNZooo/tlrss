#lang typed/racket/base

(require/typed db
               [#:opaque Connection connection?]
               [#:opaque Connection-Pool connection-pool?]
               [#:opaque Statement statement?]
               [query-maybe-row (-> Connection
                                    (U Statement String)
                                    Any
                                    (U False (Vectorof Any)))]
               [virtual-connection (-> (U (-> Connection)
                                          Connection-Pool)
                                       Connection)]
               [sqlite3-connect
                 (->* (#:database (U Path-String
                                     'memory
                                     'temporary))
                      (#:mode (U 'read-only
                                 'read/write
                                 'create)
                       #:busy-retry-limit (U Exact-Nonnegative-Integer
                                             +inf.0)
                       #:busy-retry-delay Nonnegative-Exact-Rational
                       #:use-place Boolean)
                      Connection)]
               [connection-pool
                 (->* ((U Connection
                          (-> Connection)))
                      (#:max-connections (U Positive-Integer
                                            +inf.0)
                       #:max-idle-connections (U Positive-Integer
                                                 +inf.0))
                      (U (-> Connection)
                         Connection-Pool))]
               [call-with-transaction
                 (->* (Connection (-> Any))
                      (#:isolation (U 'serializable
                                      'repeatable-read
                                      'read-commited
                                      'read-uncommited)
                       #:option Any)
                      Any)])

(require "db-credentials.rkt"
         "rss-item.rkt")

(provide db-conn
         db/get/seen/title
         db/get/seen/guid)

(: db-conn Connection)
(define db-conn
  (virtual-connection
    (connection-pool
      (lambda ()
        (sqlite3-connect #:database sqlite3-db/path
                         #:mode 'read/write)))))

(define-type (Maybe a) (U False a))

(define-type Item-Vector (Vector Integer
                                 String
                                 String
                                 String
                                 String
                                 String))

(: row->item (-> Item-Vector
                 item))
(define (row->item row)
  (item (vector-ref row 0)
        (vector-ref row 1)
        (vector-ref row 2)
        (vector-ref row 3)
        (vector-ref row 4)
        (vector-ref row 5)))

(: db/get/seen/title (-> String (Maybe item)))
(define (db/get/seen/title title)
  (define result
    (call-with-transaction
      db-conn
      (lambda ()
        (query-maybe-row db-conn
                         "SELECT * FROM seen WHERE title = $1"
                         title))))

  (and result
       (row->item (cast result
                        Item-Vector))))

(: db/get/seen/guid (-> Integer (Maybe item)))
(define (db/get/seen/guid guid)
  (define result
    (call-with-transaction
      db-conn
      (lambda ()
        (query-maybe-row db-conn
                         "SELECT * FROM seen WHERE guid = $1"
                         guid))))

  (and result
       (row->item (cast result
                        Item-Vector))))
