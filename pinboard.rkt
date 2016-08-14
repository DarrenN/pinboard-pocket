#lang racket
(require json
         net/http-client
         net/url
         net/url-structs)

(provide get-recent-pinboard
         pinboard-api-token!)

(define HOST "https://api.pinboard.in/v1/")
(define PINBOARD-POSTS-PATH "posts/recent")

(define pinboard-api-token "")

(define (pinboard-api-token! token)
  (set! pinboard-api-token token))

(define (make-query pairs)
  (string-join
   (map (λ (p) (string-join p "="))
        (cons (list "format" "json")
              (cons (list "auth_token" pinboard-api-token) pairs))) "&"))

(define (make-uri path tag)
  (string->url
   (format "~a~a?~a" HOST path (make-query `(("tag" ,tag))))))

(define (get-pinboard method [tag "readlater"])
  (define-values (status header response)
    (http-sendrecv/url (make-uri method tag)))
  (if (equal? "HTTP/1.1 200 OK" (bytes->string/utf-8 status))
      (read-json response)
      (raise-user-error
       'get-pinboard "request to ~a failed: ~a" method status)))

(define (get-recent-pinboard [tag "readlater"])
  (get-pinboard PINBOARD-POSTS-PATH tag))