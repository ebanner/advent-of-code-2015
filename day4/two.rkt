#lang racket

(provide get-hash-key)

(require file/md5)

(define ∞ (in-naturals))

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x f rest ...) (~> (f x) rest ...)]))

(define (valid? key)
  (string-prefix? key "000000"))

(define (my-hash secret-key i)
  (let ([key (string-append secret-key (number->string i))])
    (~>
     key
     string->bytes/utf-8
     md5
     bytes->string/utf-8)))

(define/contract (get-hash-key secret-key)
  (-> string? exact?)

  (for/first ([i ∞]
              #:when (valid? (my-hash secret-key i)))
    i))

(module+ main
  (get-hash-key "iwrupvqb"))

