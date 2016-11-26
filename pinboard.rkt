#lang racket
(require json
         net/url
         request)

(provide get-recent-pinboard)

(define HOST "https://api.pinboard.in/v1/")
(define PINBOARD-POSTS-PATH "posts/recent")

(define pinboard-api-token (make-parameter ""))

(define (pinboard-api-token! token)
  (set! pinboard-api-token token))

(define (make-query pairs)
  ;; Use ENV vars
  (parameterize ([pinboard-api-token (getenv "PINBOARD_API_TOKEN")])
    (string-join
     (map (λ (p) (string-join p "="))
          (cons (list "format" "json")
                (cons (list "auth_token" (pinboard-api-token)) pairs))) "&")))
  
(define (make-uri path tag)
  (string->url
   (format "~a~a?~a" HOST path (make-query `(("tag" ,tag))))))

(define https-json (make-https-requester json-requester))

(define (get-recent-pinboard [tag "readlater"])
  (get https-json (make-uri PINBOARD-POSTS-PATH tag)))