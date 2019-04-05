#lang racket/base

(module+ main
  (require "private/logger.rkt"
           "private/parameters.rkt")

  (define stop-logger (get-config 'stop-logger))
  (define logger (get-config 'logger))

  ; Create a JSON Logger we can add additional fields to
  (define *json-logger* (create-json-logger logger #:fields '(module "main")))

  ; add additioal key/val fields
  (*json-logger* 'msg "START"
                 'slurm '("a" "b"))

  ; values have to be valid jsexpr?
  (*json-logger* 'flarm (hasheq 'foo 1))

  (log-json-info *json-logger*)
  (log-json-warn *json-logger*)

  (stop-logger))

(module+ test
  (require rackunit))
