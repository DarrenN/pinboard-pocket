#lang racket/base

(module+ main
  (require db
           "private/db-adapter.rkt"
           "private/logger.rkt"
           "private/parameters.rkt")

  (define stop-logger (get-config 'stop-logger))
  (define logger (get-config 'logger))

  ; Stop the logger and terminate program
  (define (cleanup-and-crash)
    (stop-logger)
    (exit 1))

  ; Create a JSON Logger we can add additional fields to
  (define *json-logger* (create-json-logger logger #:fields '(module "main")))

  #|
  Notes:
  ------

  1. Check to see if SQL DB is setup, if not, set it up (see below)
  2. Query DB for most recent entry and get timstamp, convert to epoch
  3. Query pinboard for last update `/posts/update`, convert to epoch
  4. If update > most recent entry query pinboard for recent `/posts/recent`
  5. Attempt to INSERT entries into DB. Discard failures (id exists)
  6. Any entires successfully INSERTED should be sent to Pocket via Batch API

  DB Setup:
  ---------
  1. Create a SQLite DB
  2. Make a call to Pinboard for /posts/all?tag=readlater
  3. Load entries into DB

  DB Schema
  ---------
  id (text) [unique] [index] -> pinboard hash
  timestamp (text) [index]
  href (text)
  tags (text)
  description (text)
  shared (int) 0 (false) / 1 (true)
  toread (int) 0 (false) / 1 (true)
  extended (text)
  meta (text)

  Sample Pinboard data
  --------------------

  {
  "description": "The Languages Which Almost Became CSS - Eager Blog",
  "extended": "",
  "hash": "80ce91da8e2b3d581c94c8dfd683a935",
  "href": "https://eager.io/blog/the-languages-which-almost-were-css/",
  "meta": "983c6a2121147536d044b3a9d3250ba9",
  "shared": "yes",
  "tags": "css development readlater",
  "time": "2016-06-28T18:49:13Z",
  "toread": "yes"
  }

  |#

  ; Ensure SQLite is availble
  (unless (sqlite3-available?)
    (*json-logger* 'msg "SQLite3 is not available on this system!")
    (log-json-fatal *json-logger*)
    (cleanup-and-crash))

  ; Ensure SQLite DB is setup and ready tp go
  (initialize-db)

  (stop-logger))

(module+ test
  (require rackunit))
