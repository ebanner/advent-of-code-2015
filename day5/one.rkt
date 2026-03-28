#lang racket

(provide nice?)

(define ≥ >=)

(define-syntax for/any
  (syntax-rules ()
    [(_ clauses body ...)
     (for/or clauses body ...)]))

(define-syntax for/sum*
  (syntax-rules ()
    [(_ clauses body ...)
     (for/sum clauses
       (if (and body ...) 1 0))]))

(define letters "abcdefghijklmnopqrstuvwxyz")

(define (get-strings)
  (file->lines "input"))

(define (is-vowel? c)
  (or
   (char=? c #\a)
   (char=? c #\e)
   (char=? c #\i)
   (char=? c #\o)
   (char=? c #\u)))

(define (get-num-vowels s)
  (for/sum* ([c s])
    (is-vowel? c)))

(define/contract (nice? s)
  (-> string? boolean?)

  (let/ec return
    (when (or
           (string-contains? s "ab")
           (string-contains? s "cd")
           (string-contains? s "pq")
           (string-contains? s "xy"))
      (return #f))

    (and (≥ (get-num-vowels s) 3)
         (for/any ([c letters])
           (string-contains? s (string c c))))))

(module+ main
  (define strings (get-strings))

  (for/sum* ([s strings])
    (nice? s)))
