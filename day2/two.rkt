#lang racket

(provide get-ribbon)

(define (get-specs)
  (define specs (file->lines "input"))
  specs)

(define (parse spec)
  (map string->number (string-split spec "x")))

(define (get-dimensions-list specs)
  (for/list ([spec specs])
    (parse spec)))

(define (get-smallest-perimeter l w h)
  (min (+ (* l 2) (* w 2))
       (+ (* w 2) (* h 2))
       (+ (* h 2) (* l 2))))

(define (get-bow l w h)
  (* l w h))

(define/contract (get-ribbon l w h)
  (-> exact? exact? exact? exact?)

  (define smallest-perimeter (get-smallest-perimeter l w h))

  (define area
    (+ (* 2 l w)
       (* 2 w h)
       (* 2 h l)))

  (define bow (get-bow l w h))

  (+ smallest-perimeter bow))

(module+ main
  (define specs (get-specs))

  (define dimensions-list (get-dimensions-list specs))

  (for/sum ([dimensions dimensions-list])
    (apply get-ribbon dimensions)))


