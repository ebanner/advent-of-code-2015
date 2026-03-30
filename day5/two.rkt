#lang racket

(require srfi/13)

(provide nice?)

(define ≥ >=)

(define-syntax for*/any
  (syntax-rules ()
    [(_ clauses body ...)
     (for*/or clauses body ...)]))

(define-syntax for/sum*
  (syntax-rules ()
    [(_ clauses body ...)
     (for/sum clauses
       (if (and body ...) 1 0))]))

(define-syntax for/first*
  (syntax-rules ()
    [(_ clauses body ...)
     (for/or clauses body ...)]))

(define (add2 x) (add1 (add1 x)))
(define (add3 x) (add1 (add2 x)))
(define (sub2 x) (sub1 (sub1 x)))

(define (get-strings)
  (file->lines "input"))

(define (prop1? S t)
  (let/ec return
    (when (not (string-contains S t))
      (return #f))

    (let ([i (string-contains S t)])
      (string-contains? (substring S (add2 i)) t))))

(define (prop2? s)
  (define N (string-length s))

  (for/first* ([i (sub2 N)])
    (let ([window (substring s i (add3 i))])
      (match-let ([(list c1 _ c3) (string->list window)])
        (char=? c1 c3)))))

(define letters "abcdefghijklmnopqrstuvwxyz")

(define/contract (nice? s)
  (-> string? boolean?)

  (and
   (for*/any ([l1 letters]
              [l2 letters])
     (prop1? s (string l1 l2)))
   (prop2? s)))

(module+ main
  (define strings (get-strings))

  (for/sum* ([s strings])
    (nice? s)))
