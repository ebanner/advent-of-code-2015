#lang errortrace racket

(require racket/string)

(provide turn-on
         toggle
         turn-off
         make-grid
         grid-ref
         parse)

(define-syntax for*/sum*
  (syntax-rules ()
    [(_ clauses body ...)
     (for*/sum clauses
       (if (and body ...) 1 0))]))

(define-syntax-rule (define* (name args ...) body ...)
  (define name
    (match-lambda*
      [(list args ...)
       body ...])))

(define GRID-SIZE 1000)

(define* (grid-ref grid [list i j])
  (vector-ref (list-ref grid i) j))

(define (get-instructions)
  (define lines (file->lines "input"))
  (for/list ([line lines])
    (parse line)))

(define (get-action line)
  (let ([tokens (string-split line)])
    (cond [(string=? (first tokens) "toggle") 'TOGGLE]
          [(equal? (take tokens 2) '("turn" "on")) 'TURN-ON]
          [(equal? (take tokens 2) '("turn" "off")) 'TURN-OFF])))

(define (parse-point p-str)
  (map string->number (string-split p-str ",")))

(define (get-rectangle line)
  (let ([tokens (string-split line)])
    (match-define
      (list p1-str _ p2-str) (take-right tokens 3))

    (list
     (parse-point p1-str)
     (parse-point p2-str))))

(define (parse line)
  (define action (get-action line))
  (define rectangle (get-rectangle line))
  (cons action rectangle))

(define (make-grid N [seed 0])
  (for/list ([_ N])
    (make-vector N seed)))

(define* (grid-set! grid [list i j] value)
  (vector-set! (list-ref grid i) j value))

(define* (grid-update! grid [list i j] f)
  (let ([value (grid-ref grid [list i j])])
    (vector-set! (list-ref grid i) j (f value))))

(define* (turn-on grid [list i1 j1] [list i2 j2])
  (for* ([i (in-range i1 (add1 i2))]
         [j (in-range j1 (add1 j2))])

    (grid-set! grid [list i j] 1)))

(define* (toggle grid [list i1 j1] [list i2 j2])
  (for* ([i (in-range i1 (add1 i2))]
         [j (in-range j1 (add1 j2))])

    (grid-update! grid [list i j]
                  (λ (v) (bitwise-xor v 1)))))

(define* (turn-off grid [list i1 j1] [list i2 j2])
  (for* ([i (in-range i1 (add1 i2))]
         [j (in-range j1 (add1 j2))])

    (grid-set! grid [list i j] 0)))

(define* (do-instruction grid [list action p1 p2])
  (cond [(equal? action 'TURN-ON) (turn-on grid p1 p2)]
        [(equal? action 'TOGGLE) (toggle grid p1 p2)]
        [(equal? action 'TURN-OFF) (turn-off grid p1 p2)]))

(module+ main
  (define instructions (get-instructions))

  (define grid (make-grid GRID-SIZE))

  (for ([instruction instructions])
    (do-instruction grid instruction))

  (for*/sum* ([i GRID-SIZE]
             [j GRID-SIZE])
    (positive? (grid-ref grid [list i j]))))
