#lang racket

(define (parse-rotation line)
  (match (string->list line)
    [(list #\L num ... ) (- (string->number (list->string num)))]
    [(list #\R num ...) (string->number (list->string num))]) )

(define (part-1 lines)
  (define start 50)
  (define count 0)
  (for-each
   (lambda (line)
     (let ([after-rotation (remainder (+ start (parse-rotation line)) 100)])
       (set! start
         (if (negative? after-rotation)
             (+ after-rotation 100)
             after-rotation))
       (when (zero? start)
         (set! count (+ 1 count)))))
   lines)
  count)

(define (part-2 lines)
  (define start 50)
  (define count 0)
  (for-each
   (lambda (line)
     (let* ([rotation (parse-rotation line)]
            [after-rotation (+ start rotation)])
       (set! count
         (+ count (quotient (abs after-rotation) 100)
            (if (or (positive? after-rotation) (zero? start))
                0
                1)))
       (set! start (modulo after-rotation 100))))
   lines)
  count)

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
  (check-equal? 1059 (part-1 (file->lines "./input/01.txt")))
  (check-equal? 6 (part-2 (string-split sample)))
  (check-equal? 6305 (part-2 (file->lines "./input/01.txt"))))
