#lang racket/base

(require racket/contract
         racket/contract/region
         racket/date
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

Write structured log messages (JSON) to file
============================================

First, setup a logger? and log-writer with setup-logger, ex:

(define-values (my-logger stop-logger)
  (setup-logger 'my-app-log        ; name of logger
                (getenv "LOG_DIR") ; name/path of log dir
                "myapp.log"        ; filename for log file
                ))

my-logger is your custom logger
stop-logger will terminate the logger and log-writer

Next, setup a json-logger which you will use to send logs, ex:

(define *json-logger*
  (create-json-logger my-logger                 ; logger to send logs to
                      #:fields '(module "main") ; options default log fields
                      ))

You can bind other jsexpr? fields to the json-logger, ex:

; add additioal key/val fields
(*json-logger* 'msg "START"
               'slurm '("a" "b"))

; remember, values have to be valid jsexpr?
(*json-logger* 'flarm (hasheq 'foo 1))

When you are ready to commit the log to the file, you can use the different
level procs, ex:

; Send at level INFO
(log-json-info *json-logger*)

; Send at level WARNING
(log-json-warn *json-logger*)

And the output log line should resemble:

{
 "flarm": {
           "foo": 1
           },
 "level": "WARNING",
 "msg": "START",
 "timestamp": "2019-04-06T12:01:13",
 "logname": "my-app-log",
 "slurm": [
           "a",
           "b"
           ],
 "module": "main"
 }

|#

;//////////////////////////////////////////////////////////////////////////////
; PRIVATE

; Mutable struct that implements prop:procedure allowing us to
; update the data field in place. This lets us bind more fields
; before logging with log-json.
(define-struct/contract log-fields
  ([logger logger?] [level log-level/c] [data hash-eq?])
  #:transparent
  #:mutable
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

; Macro: build a json logger for specific levels
; Aliases log-json-warn to log-json-warning
(define-syntax (define/json-logger stx)
  (syntax-case stx ()
    [(_ n body ...)
     (with-syntax ([id (format-id #'n "log-json-~a" (syntax-e #'n))]
                   [level (if (equal? (syntax-e #'n) 'warn)
                              'warning (syntax-e #'n))])
       #'(define/contract (id log-fs)
           (-> log-fields? void)
           (let ([logger (log-fields-logger log-fs)]
                 [fields (log-fields-data log-fs)])
             (log-message logger (syntax-e #'level) "" fields))))]))


;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

; Creates a logger? and log-receiver? that write JSON strings to dir/filename.
; If the dir doesn't exist we'll try to create it.
; Returns a function used to stop and shutdown the log-receiver and writer.
(define/contract (setup-logger topic dir filename)
  (-> symbol? (or/c string? path?) string? (values logger? procedure?))
  (define logger (make-logger topic))
  (define log-receiver (make-log-receiver logger 'info))
  (values logger (create-writer log-receiver dir filename)))

; Creates a log-fields struct loaded with initial values
(define/contract (create-json-logger logger
                                     #:level [level 'info]
                                     #:fields [fields (hasheq)])
  (->* (logger?) (#:level symbol? #:fields (or/c list? hash?)) log-fields?)
  (let ([fs (if (hash? fields) fields (apply hasheq fields))])
    (log-fields logger level fs)))

; Convenience functions to log at specific levels
; ex: (log-json-info json-logger-struct)

(define/json-logger debug)
(define/json-logger error)
(define/json-logger fatal)
(define/json-logger info)
(define/json-logger none)
(define/json-logger warn)
(define/json-logger warning)
