#lang errortrace racket

(provide get-house-count)

(define (get-instructions)
  (file->string "input"))

(define (get-houses instructions)
  (for/fold ([path (set)]
             [position [list 0 0]]
             #:result (set-add path position))
            ([instruction instructions])

    (match-define [list i j] position)

    (values (set-add path [list i j])
            (cond [(char=? instruction #\^) [list (sub1 i) j]]
                  [(char=? instruction #\>) [list i (add1 j)]]
                  [(char=? instruction #\v) [list (add1 i) j]]
                  [(char=? instruction #\<) [list i (sub1 j)]]))))

(define (get-santa-instructions instructions)
  (define N (string-length instructions))
  (for/list ([i (in-range 0 N 2)])
    (string-ref instructions i)))

(define (get-robo-santa-instructions instructions)
  (define N (string-length instructions))
  (for/list ([i (in-range 1 N 2)])
    (string-ref instructions i)))

(define/contract (get-house-count instructions)
  (-> string? exact?)

  (define-values (santa-instructions robo-santa-instructions)
    (values (get-santa-instructions instructions)
            (get-robo-santa-instructions instructions)))

  (set-count
   (set-union
    (get-houses santa-instructions)
    (get-houses robo-santa-instructions))))

(module+ main
  (define instructions (get-instructions))

  (get-house-count instructions))
