#lang racket/base

(require racket/date
         racket/hash
         racket/logging
         racket/match
         racket/string
         json
         "path-utils.rkt"
         (for-syntax racket/base
                     racket/syntax))

(provide create-json-logger
         log-json-debug
         log-json-error
         log-json-fatal
         log-json-info
         log-json-none
         log-json-warn
         log-json-warning
         setup-logger)

#|

Write log messages to file
==========================

- Create a logger ex: (define-logger app) which will provide you
with log-app-error, log-app-info, etc.

- Create a log receiver at the level you want to capture, ex:
(define app-receiver (make-log-receiver app-logger 'info)), which
will "listen" to log-app-info events.

- Create log writer thread, storing the returned function so you can
kill the logger if needed, ex:
(define stop-logger (create-writer app-receiver "app.log"))

|#

;//////////////////////////////////////////////////////////////////////////////
; PRIVATE

; Mutable struct that implements prop:procedure allowing us to
; update the data field in place. This lets us bind more fields
; before logging with log-json.
(struct log-fields (logger level data)
  #:transparent
  #:mutable
  #:guard (λ (logger level data type-name)
            (cond
              [(not (logger? logger))
               (error type-name
                      "not a logger: ~e"
                      logger)]
              [(not (log-level/c level))
               (error type-name
                      "not a valid log-level: ~e"
                      level)]
              [(not (hash-eq? data))
               (error type-name
                      "not a hash-eq?: ~e"
                      data)]
              [else (values logger level data)]))
  #:property prop:procedure
  (λ (self . kvs)
    (let ([data (log-fields-data self)]
          [new-data (apply hasheq kvs)])
      (set-log-fields-data!
       self
       (hash-union
        data new-data
        #:combine/key (λ (k v1 v2) v2))))))

; Bind receiver to a lambda that accepts a fileport to write to
; NOTE: We use JSON formatted logs
(define (handle-log-message receiver)
  (λ (out)
    (match (sync receiver)
      [(vector event-level event-message event-value name)
       (displayln
        (jsexpr->string
         (hash-union
          event-value
          (hasheq 'timestamp (date->string (current-date) #t)
                  'level ((compose string-upcase symbol->string)
                          event-level)
                  'logname (symbol->string name))
          #:combine/key (lambda (k v1 v2) v2)))
        out)])))

; Start a thread to write logger messages to a file, returns a
; lambda which will shutdown the thread
(define (create-writer log-receiver dir filename)
  (define log-cust (make-custodian))
  (parameterize ([current-custodian log-cust]
                 [date-display-format 'iso-8601])
    (define (loop)
      (call-with-output-file*
        (build-path (ensure-directory dir) filename)
        (handle-log-message log-receiver)
        #:exists 'append)
      (loop))
    (thread loop))
  (lambda ()
    (sleep 1)
    (custodian-shutdown-all log-cust)))

;; Macro: build a json logger for specific levels
(define-syntax (define/json-logger stx)
  (syntax-case stx ()
    [(_ n body ...)
     (with-syntax ([id (format-id #'n "log-json-~a" (syntax-e #'n))]
                   [level (if (equal? (syntax-e #'n) 'warn)
                              'warning (syntax-e #'n))])
       #'(define (id log-fs)
           (define-values (logger fields)
             (values (log-fields-logger log-fs)
                     (log-fields-data log-fs)))
           (log-message logger (syntax-e #'level) "" fields)))]))


;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (setup-logger topic dir filename)
  (define logger (make-logger topic))
  (define log-receiver (make-log-receiver logger 'info))
  (values logger (create-writer log-receiver dir filename)))

; Creates a log-fields struct loaded with initial values
(define (create-json-logger logger
                            #:level [level 'info]
                            #:fields [fields (hasheq)])
  (let ([fs (if (hash? fields) fields (apply hasheq fields))])
    (log-fields logger level fs)))

; Convenience functions to log at specific levels
(define/json-logger debug)
(define/json-logger error)
(define/json-logger fatal)
(define/json-logger info)
(define/json-logger none)
(define/json-logger warn)
(define/json-logger warning)
