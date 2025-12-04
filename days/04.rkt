#lang racket
(require threading)

(define day 4)
(define day-str (~r day #:min-width 2 #:pad-string "0"))

(require racket/runtime-path)
(define-runtime-path input-dir "../input")
(define input-file (build-path input-dir (format "~a.txt"  day-str)))

(define directions
  '((-1 . -1) ; UL
    (-1 . 0)  ; U
    (-1 . 1)  ; UR
    (0 . -1)  ; L
    (0 . 1)   ; R
    (1 . -1)  ; DL
    (1 . 0)   ; D
    (1 . 1)   ; DR
    ))

(define (within-bounds grid row col)
  (and (<= 0 row (sub1 (length grid)))
       (<= 0 col (sub1 (length (car grid))))))

(define (removable? grid r c)
  (let ([ch (~> (list-ref grid r)
                (list-ref c))])
    (and (char=? ch #\@)
         (< (count (lambda~> (char=? #\@))
                   (neighbors grid r c))
            4))))

(define (neighbors grid row col)
  (filter
   identity
   (for/list ([dir directions])
     (let ([r (+ row (car dir))]
           [c (+ col (cdr dir))])
       (and (within-bounds grid r c)
            (~> (list-ref grid r)
                (list-ref c)))))))

(define (part-1 in)
  (let ([grid (~> in string-split (map string->list _))])
    (for/sum ([(row i) (in-indexed grid)])
      (count identity
             (for/list ([j (in-range (length row))])
               (removable? grid i j))))))

(define (part-2 in)
  (let ([grid (~> in string-split (map string->list _))])
    (let loop ([grid grid] [removed 0] [total 0])
      (let ([new-grid (for/list ([(row i) (in-indexed grid)])
                        (for/list ([(ch j) (in-indexed row)])
                          (if (removable? grid i j)
                              (begin (set! removed (add1 removed)) #\.)
                              ch)
                          ))])
        (if (zero? removed)
            total
            (loop new-grid 0 (+ total removed)))))))

(module+ main
  (let ([in (file->string input-file)])
    (displayln (format "Day ~a" day-str))
    (displayln (format "  Part 1: ~a" (part-1 in)))
    (displayln (format "  Part 2: ~a" (part-2 in)))))

(module+ test
  (require rackunit)
  (define sample "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")
  (check-equal? (part-1 sample) 13)
  (check-equal? (part-1 (file->string input-file)) 1602)
  (check-equal? (part-2 sample) 43)
  (check-equal? (part-2 (file->string input-file)) 9518))
