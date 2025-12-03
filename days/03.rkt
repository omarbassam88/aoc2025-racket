#lang racket
(require threading)

(define day 3)
(define day-str (~r day #:min-width 2 #:pad-string "0"))

(require racket/runtime-path)
(define-runtime-path input-dir "../input")
(define input-file (build-path input-dir (format "~a.txt"  day-str)))

(define (string->digits s)
  (map (compose string->number string) (string->list s)))

(define (digits->number digits)
  (string->number
   (string-join (map number->string digits) "")))

(define (largest-sequence digits size)
  (if (zero? size) null
      (let* ([mx (apply max (drop-right digits (- size 1)))]
             [after-max (drop digits (+ 1 (index-of digits mx)))])
        (cons mx (largest-sequence after-max (- size 1))))))

(define (part-1 in)
  (for/sum ([line (~> in string-trim string-split)])
    (~> (string->digits line)
        (largest-sequence 2)
        digits->number)))

(define (part-2 in)
  (for/sum ([line (~> in string-trim string-split)])
    (~> (string->digits line)
        (largest-sequence 12)
        digits->number)))

(module+ main
  (let ([in (file->string input-file)])
    (displayln (format "Day ~a" day-str))
    (displayln (format "  Part 1: ~a" (part-1 in)))
    (displayln (format "  Part 2: ~a" (part-2 in)))))

(module+ test
  (require rackunit)
  (define sample "987654321111111
811111111111119
234234234234278
818181911112111")
  (check-equal? (part-1 sample) 357)
  (check-equal? (part-1 (file->string input-file)) 17445)
  (check-equal? (part-2 sample) 3121910778619)
  (check-equal? (part-2 (file->string input-file)) 173229689350551))
