#lang racket/base

(require dotenv
         racket/list
         racket/string
         "logger.rkt")

(provide config
         get-config)

;; Dynamically mess around with the .env path so we can load files from
;; /private in the repl and still get our settings
(define path-frags (string-split (path->string (current-directory)) "/"))
(define env-path (if (equal? (last path-frags) "private")
                     (build-path ".." ".env")
                     (build-path (current-directory) ".env")))

(dotenv-load! (list (path->string env-path))) ;; pulls in API key

(define log-dir (getenv "LOG_DIR"))

(define-values (pinboard-logger stop-logger)
  (setup-logger 'pinpocket
                (if log-dir
                    log-dir
                    (find-system-path 'temp-dir))
                "pinpocket.log"))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define config
  (make-parameter
   (hasheq 'pinboard-api-token (getenv "PINBOARD_API_TOKEN")
           'pocket-access-token (getenv "POCKET_ACCESS_TOKEN")
           'pocket-consumer-key (getenv "POCKET_CONSUMER_KEY")
           'log-dir log-dir
           'logger pinboard-logger
           'stop-logger stop-logger)))

(define (get-config key)
  (hash-ref (config) key))
