#lang racket
(require threading)

(define day 5)
(define day-str (~r day #:min-width 2 #:pad-string "0"))

(require racket/runtime-path)
(define-runtime-path input-dir "../input")
(define input-file (build-path input-dir (format "~a.txt"  day-str)))

(define string->range
  (lambda~> (string-split "-")
            (map string->number _)))

(define parse-ranges
  (lambda~> string-split
            (map string->range _)))

(define (part-1 in)
  (match-let* ([(list ranges-in ingredients-in) (string-split in "\n\n")]
               [ranges (parse-ranges ranges-in)]
               [ingredients (~> ingredients-in string-split (map string->number _))])
    (for/sum ([ingr ingredients])
      (if (for/or ([rng ranges])
            (<= (first rng) ingr (second rng)))
          1
          0))))

(define (intersect? lhs rhs)
  (match-let ([(list x1 y1) lhs]
              [(list x2 y2) rhs])
    (and (>= y1 x2) (>= y2 x1))))

(define (merge lhs rhs)
  (match-let ([(list x1 y1) lhs]
              [(list x2 y2) rhs])
    (list (min x1 x2) (max y1 y2))))

(define (merge-ranges ranges)
  (let loop ([cur (first ranges)]
             [rs (rest ranges)]
             [acc empty])
    (cond [(empty? rs) (cons cur acc)]
          [(intersect? cur (first rs))
           (loop (merge cur (first rs)) (rest rs) acc )]
          [else (loop (first rs) (rest rs) (cons cur acc))])))

(define (range-size rng)
  (add1 (- (second rng) (first rng))))

(define (part-2 in)
  (~> (string-split in "\n\n")
      first
      parse-ranges
      (sort (lambda (a b) (< (car a) (car b))))
      merge-ranges
      (map range-size _)
      (apply + _)))

(module+ main
  (let ([in (file->string input-file)])
    (displayln (format "Day ~a" day-str))
    (displayln (format "  Part 1: ~a" (part-1 in)))
    (displayln (format "  Part 2: ~a" (part-2 in)))))

(module+ test
  (require rackunit)
  (define sample "3-5
10-14
16-20
12-18

1
5
8
11
17
32")
  (check-equal? (part-1 sample) 3)
  (check-equal? (part-1 (file->string input-file)) 868)
  (check-equal? (part-2 sample) 14)
  (check-equal? (part-2 (file->string input-file)) 354143734113772))
