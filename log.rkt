#lang racket
(require racket/date
         racket/match)

(provide create-writer)

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

; Bind receiver to a lambda that accepts a fileport to write to
(define (handle-log-message receiver)
  (λ (out)
    (match (sync receiver)
      [(vector event-level event-message event-value name)
       (displayln
        (format "[~a] ~a - ~a"
                (date->string (current-date) #t)
                ((compose string-upcase symbol->string) event-level)
                event-message)
        out)])))

; Start a thread to write logger messages to a file, returns a
; lambda which will shutdown the thread
(define (create-writer log-receiver filename)
  (define log-cust (make-custodian))
  (parameterize ([current-custodian log-cust]
                 [date-display-format 'iso-8601])
    (define (loop)
      (call-with-output-file*
       filename
       (handle-log-message log-receiver)
       #:exists 'append)
      (loop))
    (thread loop))
  (lambda ()
    (sleep 1)
    (custodian-shutdown-all log-cust)))
