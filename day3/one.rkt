#lang errortrace racket

(provide get-house-count)

(define (get-instructions)
  (file->string "input"))

(define/contract (get-house-count instructions)
  (-> string? exact?)

  (for/fold ([path (set)]
             [position [list 0 0]]
             #:result (set-count (set-add path position)))
            ([instruction instructions])

    (match-define [list i j] position)

    (values (set-add path [list i j])
            (cond [(char=? instruction #\^) [list (sub1 i) j]]
                  [(char=? instruction #\>) [list i (add1 j)]]
                  [(char=? instruction #\v) [list (add1 i) j]]
                  [(char=? instruction #\<) [list i (sub1 j)]]))))

(module+ main
  (define instructions (get-instructions))

  (get-house-count instructions))
