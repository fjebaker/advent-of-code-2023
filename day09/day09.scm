(import
  (chicken base)
  (chicken io)
  (chicken string)
  (srfi 1)
  scheme
)

(define ((curry f x) y) (f x y))

(define ((all-are? what) l)
  (every (curry equal? 0) l))

(define (getlines filename) (call-with-input-file filename read-lines))

(define (parse/line line)
  (map string->number (string-split line)))

(define (diff x)
  (let loop [(x (cdr x)) (prev (car x)) (accum '())]
    (if (null? x)
      (reverse accum)
      (loop (cdr x) (car x) (cons (- (car x) prev) accum)))))

(define (diff-until condition? l)
  (let loop [(diffs (cons l '()))]
    (if (condition? (car diffs))
      diffs
      (loop (cons (diff (car diffs)) diffs)))))

(define (forecast x)
  (let [(diffs (diff-until (all-are? 0) x))]
    (let loop [(diffs (map reverse (cdr diffs))) (carry 0)]
      (if (null? diffs)
        carry
        (loop
          (cdr diffs)
          (+ carry (caar diffs)))))))

(let* [(lines (getlines "day09/input.txt"))
       (readings (map parse/line lines))
       (forecasts (map forecast readings))
       (backcasts (map (compose forecast reverse) readings))]
  (print "Part1: " (apply + forecasts))
  (print "Part1: " (apply + backcasts)))

