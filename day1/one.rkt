#lang racket

(provide get-floor)

(define/contract (get-floor directions)
  (-> string? exact-integer?)

  (for/fold ([floor 0])
            ([c directions])

    (cond [(char=? c #\() (add1 floor)]
          [(char=? c #\)) (sub1 floor)])))

(module+ main
  (define directions (file->string "input"))
  (get-floor directions))

