#lang racket
(require json
         request)

(provide (contract-out
          [make-json-requester (->* (requester?) (list?) requester?)]
          [struct json-response
            ((code number?) (headers hash?) (body jsexpr?))]))

(struct json-response
  (code headers body) #:transparent)

(define json-headers '("Accept: application/json"))

(define (content-json? headers)
  (let ([content-type (hash-ref headers "Content-Type")])
    (string-contains? content-type "application/json")))

(define (raise-non-json-err headers)
  (let ([content-type (hash-ref headers "Content-Type")])
     (raise
      (exn:fail:network:http:code
       (format "Not Acceptable: ~a" content-type)
       (current-continuation-marks) 406))))

;; Wrap a requester with handlers for headers and responses specific to dealing
;; with JSON requests
(define (make-json-requester requester [headers '()])
  (wrap-requester-response
   (λ (response)
     (let ([headers (http-response-headers response)])
       (if (content-json? headers)
           (json-response
            (http-response-code response)
            headers
            (string->jsexpr (http-response-body response)))
          (raise-non-json-err headers))))
   (add-requester-headers (flatten (cons headers json-headers)) requester)))

#|
(define json-requester
  (make-json-requester
   (make-domain-requester "darrennewton.com" http-requester)))

(define resp
  (get json-requester
       "2015/03/08/sour-mash-getting-your-clojure-into-a-jar/atom.json"))
|#