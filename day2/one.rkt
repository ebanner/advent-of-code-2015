#lang racket

(define (get-specs)
  (define specs (file->lines "input"))
  specs)

(define (parse spec)
  (map string->number (string-split spec "x")))

(define (get-dimensions-list specs)
  (for/list ([spec specs])
    (parse spec)))

(define (get-smallest-side l w h)
  (min (* l w)
       (* w h)
       (* h l)))

(define/contract (get-wrapping-paper l w h)
  (-> exact? exact? exact? exact?)

  (define smallest-side (get-smallest-side l w h))

  (define area
    (+ (* 2 l w)
       (* 2 w h)
       (* 2 h l)))

  (define slack (+ area smallest-side))

  slack)

(module+ main
  (define specs (get-specs))

  (define dimensions-list (get-dimensions-list specs))

  (for/sum ([dimensions dimensions-list])
    (apply get-wrapping-paper dimensions)))

(provide get-wrapping-paper
         parse)


