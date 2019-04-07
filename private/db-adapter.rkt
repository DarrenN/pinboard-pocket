#lang racket/base

(require db
         racket/path
         threading
         "parameters.rkt"
         "path-utils.rkt")

(provide get-connection
         initialize-db)

;//////////////////////////////////////////////////////////////////////////////
; PRIVATE

; Ensure we have a path to the DB file based on parameters
(define (path-to-db)
  (define db-path-base (get-config 'pinboard-db-path))
  (define db-filename (get-config 'pinboard-db-filename))
  (ensure-directory (build-path db-path-base db-filename)))

; Return a connection to db-path, or create a db if it doesn't exist
(define (create-connection db-path)
  (sqlite3-connect #:database db-path
                   #:mode 'create))

; Setup the SQLite DB with tables and indexes
(define (create-table conn)
  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS entries
    (id integer primary key,
     hash text not null,
     timestamp text not null,
     href text not null,
     tags text not null,
     description text,
     shared int not null,
     toread int not null,
     extended text null,
     meta text null);")

  (query-exec
   conn
   "CREATE UNIQUE INDEX idx_hash ON entries (hash);")

  (query-exec
   conn
   "CREATE INDEX idx_timestamp ON entries (timestamp);"))

(define (ensure-db conn)
  (unless (table-exists? conn "entries")
    (create-table conn)))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (initialize-db)
  (~> (path-to-db)
      create-connection
      ensure-db))

(define (get-connection)
  (~> (path-to-db)
      create-connection))
