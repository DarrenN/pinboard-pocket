#lang racket/base

(require racket/file
         racket/path
         racket/string
         threading)

(provide (all-defined-out))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Return a file name without the extension
;; (-> pathstring? bytes? string?)
(define (filename-without-ext file-name file-ext)
  (let ([fn (path->string file-name)]
        [ext (bytes->string/utf-8 file-ext)])
    (string-replace fn ext "")))

;; Return path without filename ex: "/foo/bar/baz.txt" -> "/foo/bar"
;; (-> path-string? path-string?)
(define (path-without-file-name path)
  (let ([file-name (file-name-from-path path)]
        [path-str (path->string path)]
        [has-ext (path-get-extension path)])
    (if (and has-ext file-name)
        (string->path (string-replace path-str (path->string file-name) ""))
        path)))

;; If a directory does not exist, create it and return original path
;; (-> path-string? path-string?)
(define (ensure-directory path)
  (let ([dir-path (path-without-file-name path)])
    (unless (directory-exists? dir-path)
      (make-directory* dir-path)))
  path)

;; Takes a string and converts to a truncated slug
;; (-> string? string?)
(define (slugify str)
  (~> str
      string-foldcase
      (substring 0 (min 50 (string-length str))) ;; truncate if over 50 chars
      (regexp-match* #px"[\\w\\s]*" _) ;; ASCII only, replace with spaces
      (string-join " ") ;; combine matches back into a string
      string-normalize-spaces ;; cleanup extra whitespace
      (string-replace " " "-")))

;; Take a url template (ex: "/{year}/{file}/index.html" and replace braced
;; vars with values from mapping hash. !! Any value missing from mappings will
;; be removed from the final string.
;; (-> string? hash? string?)
(define (expand-path-template template mapping)
  (let ([tpl-vars (map (λ (s) (string-replace s #rx"[\\{\\}]*" ""))
                       (regexp-match* #px"\\{[\\w]*\\}" template))])
    (foldl (λ (key result)
             (if (hash-has-key? mapping key)
                 (string-replace result (format "{~a}" key)
                                 (string-foldcase
                                  (format "~a" (hash-ref mapping key))))
                 (string-replace result (format "{~a}" key) "")))
           template tpl-vars)))


;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)

  (test-case "filename-without-ext"
    (define fn (string->path "dj-shadow.exe"))
    (define ext (string->bytes/utf-8 ".exe"))
    (check-equal? (filename-without-ext fn ext) "dj-shadow"))

  (test-case "path-without-file-name"
    (check-equal? (path-without-file-name (string->path "/foo/bar/baz.txt"))
                  (string->path "/foo/bar/"))

    (check-equal? (path-without-file-name (string->path "/foo/bar"))
                  (string->path "/foo/bar")))

  (test-case "ensure-directory creates a dir if it doesn't exist"
    (define dir-path (build-path (find-system-path 'temp-dir)
                                 (number->string (random 999999))
                                 (number->string (random 999999999))))
    (check-pred directory-exists? (ensure-directory dir-path)))

  (test-case "slugify"
    (check-equal? (slugify " Endtroducing by DJ   Shadow")
                  "endtroducing-by-dj-shadow")
    (check-equal?
     (slugify "Wes Chow on Off-the-Record Communication, or, Why Not To Use PGP")
     "wes-chow-on-off-the-record-communication-or-why")

    (check-equal?
     (slugify "Bodil Stokke on µKanren: A Minimal Functional Core for Relational Programming")
     "bodil-stokke-on-kanren-a-minimal-functional-core"))

  (test-case "expand-path-template"
    (check-equal?
     (expand-path-template "/{year}/{category}/{file}.jpg"
                           (hash "year" 2014 "category" "NewS" "file" "quux"))
     "/2014/news/quux.jpg")

    (check-equal?
     (expand-path-template "/{year}/{category}/{file}.jpg"
                           (hash "category" "news" "file" "quux"))
     "//news/quux.jpg")))
