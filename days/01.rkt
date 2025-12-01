#lang racket

(require racket/runtime-path)
(define-runtime-path input-dir "../input")

(define (parse-rotation line)
  (let ([parse-value (compose string->number list->string)])
    (match (string->list line)
      [(list #\L num ... ) (- (parse-value num))]
      [(list #\R num ...) (parse-value num)])) )

(define (part-1 lines)
  (for/fold ([count 0] [start 50] #:result count) ([line lines])
    (let ([after-rotation (remainder (+ start (parse-rotation line)) 100)])
      (values (if (zero? (modulo after-rotation 100))
                  (+ 1 count)
                  count)
              (if (negative? after-rotation)
                  (+ after-rotation 100)
                  after-rotation)))))

(define (part-2 lines)
  (for/fold ([count 0] [start 50] #:result count) ([line lines])
    (let ([after-rotation (+ start (parse-rotation line))])
      (values (+ count (quotient (abs after-rotation) 100)
                 (if (or (positive? after-rotation) (zero? start))
                     0
                     1))
              (modulo after-rotation 100)))))

(module+ main
  (let ([lines (file->lines (build-path input-dir "01.txt") )])
    (displayln (format "Day 01"))
    (displayln (format "  Part 1: ~a" (part-1 lines)))
    (displayln (format "  Part 2: ~a" (part-2 lines)))))

(module+ test
  (require rackunit)
  (define sample "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")
  (check-equal? 3 (part-1 (string-split sample)))
  (check-equal? 1059 (part-1 (file->lines (build-path input-dir "01.txt"))))
  (check-equal? 6 (part-2 (string-split sample)))
  (check-equal? 6305 (part-2 (file->lines (build-path input-dir "01.txt")))))
