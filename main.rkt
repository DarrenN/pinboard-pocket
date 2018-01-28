#lang racket
(require gregor
         "log.rkt"
         "pinboard.rkt"
         "pocket-api.rkt")

;; Setup logging
(define-logger pipeline)
(define pipeline-receiver (make-log-receiver pipeline-logger 'info))
(define log-writer (create-writer pipeline-receiver "pinboard-pocket.log"))

;; Real simple: get recent Pinboard bookmarks with a readlater tag
;; and add them to Pocket one-by-one (couldn't get batch add working)
(module+ main
  (with-handlers ([exn:fail:user?
                   (λ (exn) (log-pipeline-error (exn-message exn)))])

    (define pinboard-posts (hash-ref (get-recent-pinboard) 'posts))
    (define pocket-urls (fetch-pocket-urls))
    (log-pipeline-info "Got some data")

    (define pocket-queries
      (filter
       (λ (x) (not (void? x)))
       (for/list ([post pinboard-posts])
         (define url (hash-ref post 'href))
         (when (not (hash-has-key? pocket-urls url))
           (hasheq 'action "add"
                   'url (hash-ref post 'href)
                   'title (hash-ref post 'description)
                   'tags (string-replace (hash-ref post 'tags) " " ","))))))

    (define (send-pocket-queries queries)
        (define pocket-response
          (add-pocket-urls (hash 'actions pocket-queries)))

        ; Any status other than 1 is a failure of some sort
        (if (equal? (hash-ref pocket-response 'status) 1)
            (log-pipeline-info "Pocket import success")
            (log-pipeline-error (format "Pocket import failue - status ~a"
                                        (hash-ref pocket-response 'status))))

      ;; close logger
      (log-writer))

    (if (empty? pocket-queries)
        (begin
          (log-pipeline-info "Nothing to send to Pocket")
          (println (format "~a Nothing to send to Pocket"
                           (datetime->iso8601 (now/utc))))
          (log-writer))
        (send-pocket-queries pocket-queries))))
