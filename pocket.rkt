#lang racket
(require json
         net/http-client
         net/uri-codec
         net/url
         net/url-structs)

(provide pocket-access-token!
         pocket-consumer-key!
         pocket-get
         pocket-post
         add-to-pocket)

(define HOST "https://getpocket.com/v3")
(define OAUTH_REQUEST "/oauth/request")
(define OAUTH_AUTHORIZE "/oauth/request")
(define RETRIEVE "/get")
(define ADD "/add")
(define MODIFY "/send")

(define pocket-access-token "")
(define pocket-consumer-key "")

(define (pocket-access-token! token)
  (set! pocket-access-token token))

(define (pocket-consumer-key! key)
  (set! pocket-consumer-key key))

(define DEFAULT_HEADERS
  '("Content-type: application/json; charset=UTF8"
    "X-Accept: application/json"))

(define (auth-payload h)
  (hash-set* h 'access_token pocket-access-token 'consumer_key pocket-consumer-key))

(define (make-headers headers)
  (flatten (cons headers DEFAULT_HEADERS)))

(define (make-uri endpoint)
  (string->url
   (string-join (list HOST endpoint) "")))

(define (is-ok? status)
  (equal? (bytes->string/utf-8 status) "HTTP/1.1 200 OK"))

;; GET endpoint
(define (pocket-get endpoint [headers '()])
  (define-values (status header response)
    (http-sendrecv/url
     (make-uri endpoint)
     #:headers (make-headers headers)))
  (if (is-ok? status)
      (read-json response)
      (raise-user-error
       'pocket-get "request to ~a failed: ~a" endpoint status)))

;; POST endpoint
(define (pocket-post endpoint query [headers '()])
  (define-values (status header response)
    (http-sendrecv/url
     (make-uri endpoint)
     #:headers (make-headers headers)
     #:method "POST"
     #:data (jsexpr->string (auth-payload query))))
  (if (is-ok? status)
      (read-json response)
      (raise-user-error
       'pocket-post "request to ~a failed: ~a on ~a"
       endpoint status (jsexpr->string (auth-payload query)))))

(define (add-to-pocket query)
  (pocket-post MODIFY query))

;; Batch MODIFY example structure
(define addq
  (hash
   'actions
   (list
    (hasheq
     'action "add"
     'tags "racket,test"
     'title "For"
     'url "http://docs.racket-lang.org/reference/for.html?")
    (hasheq
     'action "add"
     'tags "racket,test"
     'title "Set"
     'url "http://docs.racket-lang.org/reference/set_.html"))))
