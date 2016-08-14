#lang racket
(require "log.rkt"
         "pinboard.rkt"
         "pocket.rkt")

;; Setup logging
(define-logger pipeline)
(define pipeline-receiver (make-log-receiver pipeline-logger 'info))
(create-writer pipeline-receiver "pinboard-pocket.log")

;; Setup API credentials (use getenv irl)
(pinboard-api-token! (getenv "PINBOARD_API_TOKEN"))
(pocket-access-token! (getenv "POCKET_ACCESS_TOKEN"))
(pocket-consumer-key! (getenv "POCKET_CONSUMER_KEY"))

;; Real simple: get recent Pinboard bookmarks with a readlater tag
;; and add them to Pocket one-by-one (couldn't get batch add working)
(module+ main 
  (with-handlers ([exn:fail:user?
                   (λ (exn) (log-pipeline-error (exn-message exn)))])
    (define pinboard_posts (hash-ref (get-recent-pinboard) 'posts))
    (define pocket-queries
      (for/list ([post pinboard_posts])
        (hasheq 'action "add"
                'url (hash-ref post 'href)
                'title (hash-ref post 'description)
                'tags (string-replace (hash-ref post 'tags) " " ","))))
    (define pocket-response
      (add-to-pocket (hash 'actions pocket-queries)))

    ; Any status other than 1 is a failure of some sort
    (if (equal? (hash-ref pocket-response 'status) 1)
        (log-pipeline-info "Pocket import success")
        (log-pipeline-error (format "Pocket import failue - status ~a"
                                    (hash-ref pocket-response 'status))))))

