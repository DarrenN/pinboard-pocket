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

(define tmp-path (find-system-path 'temp-dir))
(define log-dir (getenv "LOG_DIR"))
(define db-path (getenv "PINBOARD_DB_PATH"))
(define db-filename (getenv "PINBOARD_DB_FILENAME"))

; Setup logger (uses JSON structured logs)
(define-values (pinboard-logger stop-logger)
  (setup-logger 'pinpocket
                (if log-dir log-dir tmp-path)
                "pinpocket.log"))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define config
  (make-parameter
   (hasheq 'pinboard-api-token (getenv "PINBOARD_API_TOKEN")
           'pinboard-db-path (if db-path db-path tmp-path)
           'pinboard-db-filename (if db-filename db-filename "pinboard.db")
           'pocket-access-token (getenv "POCKET_ACCESS_TOKEN")
           'pocket-consumer-key (getenv "POCKET_CONSUMER_KEY")
           'log-dir (if log-dir log-dir tmp-path)
           'logger pinboard-logger
           'stop-logger stop-logger)))

(define (get-config key)
  (hash-ref (config) key))
