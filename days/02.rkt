#lang racket

(require threading)
(require racket/list/grouping)

(require racket/runtime-path)
(define-runtime-path input-dir "../input")

(define (parse-input in)
  (~> (lambda~> string-trim
                (string-split "-")
                (map string->number _)
                (apply inclusive-range _))
      (map (string-split in ","))))

(define (sum-invalid range invalid-id?)
  (foldl + 0 (filter invalid-id? range)))

(define (part-1 in)
  (define (invalid-id? id)
    (let* ([str (number->string id)]
           [half (/ (string-length str) 2)])
      (and (integer? half)
           (string=? (substring str 0 half)
                     (substring str half)))))
  (for/sum ([range (parse-input in)])
    (sum-invalid range invalid-id?)))

(define (part-2 in)
  (define (invalid-id? id)
    (let ([str (number->string id)])
      (for/or ([index (inclusive-range 1 (/ (string-length str) 2))])
        (~> (string->list str)
            (windows index index _)
            remove-duplicates
            length
            (= 1)
            (and (zero? (modulo (string-length str) index)))))))
  (for/sum ([range (parse-input in)])
    (sum-invalid range invalid-id?)))

(module+ main
  (let ([in (file->string (build-path input-dir "02.txt"))])
    (displayln (format "Day 02"))
    (displayln (format "  Part 1: ~a" (part-1 in)))
    (displayln (format "  Part 2: ~a" (part-2 in)))))

(module+ test
  (require rackunit)
  (define sample "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
  (check-equal? (part-1 sample) 1227775554)
  (check-equal? (part-1 (file->string (build-path input-dir "02.txt"))) 18893502033)
  (check-equal? (part-2 sample) 4174379265)
  (check-equal? (part-2 (file->string (build-path input-dir "02.txt"))) 26202168557))
