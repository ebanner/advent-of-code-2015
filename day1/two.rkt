#lang racket

(provide get-floor)

(define-syntax (for/fold* stx)
  (syntax-case stx ()
    [(_ (accs ...) ([var seq] ...) kw [c r] body ...)
     (equal? (syntax->datum #'kw) '#:break)
     #'(let/ec %break
         (for/fold (accs ...)
                   ([var seq] ...)
           (when c (%break r))
           body ...))]))

(define/contract (get-floor directions)
  (-> string? exact-integer?)

  (define directions* (string-append directions "🧠"))

  (for/fold* ([floor 0])
             ([(c i) (in-indexed directions*)])
             #:break [(negative? floor) i]

    (cond [(char=? c #\() (add1 floor)]
          [(char=? c #\)) (sub1 floor)])))

(module+ main
  (define directions (file->string "input"))
  (get-floor directions))
