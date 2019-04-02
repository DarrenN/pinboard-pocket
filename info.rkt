#lang info
(define clean '("compiled" "private/compiled"))
(define collection "pinboard-pocket")
(define version "0.2.0")
(define pkg-authors '(Darren_N))

(define deps
  '("base"
    "dotenv"
    "gregor"
    "threading"
    "https://github.com/DarrenN/racket-request.git#master"))

(define build-deps
  '("net-doc"
    "rackunit-lib"
    "rackunit-doc"
    "racket-doc"))
