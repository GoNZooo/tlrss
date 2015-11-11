#lang racket/base

(require db
         
         "db-credentials.rkt")

(provide db-conn)
(define db-conn
  (virtual-connection
    (connection-pool
      (lambda ()
        (sqlite3-connect #:database sqlite3-db/path
                         #:mode 'read/write)))))

(define (db/get/seen/title title)
  (call-with-transaction db-conn
    (lambda ()
      (query-maybe-row db-conn
                       "SELECT * FROM seen WHERE title = $1"
                       title))))

(define (db/get/seen/guid guid)
  (call-with-transaction db-conn
    (lambda ()
      (query-maybe-row db-conn
                       "SELECT * FROM seen WHERE guid = $1"
                       guid))))
