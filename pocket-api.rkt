#lang racket
(require request
         net/url
         json)

(provide add-pocket-urls
         fetch-pocket-urls)

(define HOST "https://getpocket.com/v3")
(define PATH_OAUTH_REQUEST "/oauth/request")
(define PATH_OAUTH_AUTHORIZE "/oauth/request")
(define PATH_RETRIEVE "/get.php")
(define PATH_ADD "/add.php")
(define PATH_MODIFY "/send.php")

(define pocket-access-token (make-parameter ""))
(define pocket-consumer-key (make-parameter ""))

(define (auth-payload h)
  ;; Use ENV vars
  (parameterize ([pocket-access-token (getenv "POCKET_ACCESS_TOKEN")]
                 [pocket-consumer-key (getenv "POCKET_CONSUMER_KEY")])
    (hash-set* h
               'access_token (pocket-access-token)
               'consumer_key (pocket-consumer-key))))

(define (make-url path)
  (string->url (format "~a~a" HOST path)))

(define https-json (make-https-requester json-requester))

;; Return a hash-table of urls that have been archived, sorted
;; newest first. Use to remove Pinboard URLs from payload.
(define (fetch-pocket-urls)
  (define archived
    (post https-json
          (make-url PATH_RETRIEVE)
          (auth-payload
           (hasheq 'detailType "simple"
                   'state "all"))))

  ;; build a hash-table of archived URLs
  (for/hash ([url (hash-values (hash-ref archived 'list))])
    (values (hash-ref url 'given_url) 1)))

;; Post a JSON list of URLs to add to Pocket
(define (add-pocket-urls jsexpr)
  (post https-json
        (make-url PATH_MODIFY)
        (auth-payload jsexpr)))
