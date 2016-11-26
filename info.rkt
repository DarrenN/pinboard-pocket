#lang info

(define collection "pinboard-pocket")
(define version "0.0.1")
(define deps
  '("base"
    "date"
    "match"
    "json"
    "https://github.com/DarrenN/racket-request.git#master"))

(define build-deps
  '("net-doc"
    "rackunit-lib"
    "rackunit-doc"
    "racket-doc"))