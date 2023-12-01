(import
  (chicken base)
  (chicken io)
  (chicken string)
  srfi-1
  scheme
)

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (is-numeric? c)
  (and
    (>= (char->integer c) (char->integer #\0))
    (<= (char->integer c) (char->integer #\9))))

(define (atoi a) (- (char->integer a) (char->integer #\0)))

(define (starts-with? s start)
  (define end (min (string-length s) (string-length start)))
  (string=? (substring s 0 end) start))

(define (get-first s vals part2)
  (let loop [(i 0)]
    (if (>= i (string-length s))
      (error "unreachable")
      ; get the first character of the string
      (let [(char (string-ref s i))]
        (if (is-numeric? char)
          (atoi char)
          (if part2
            (let inner-loop [(k 0) (vs vals)]
              (define name (car vs))
              (if (starts-with? (substring s i) name)
                (+ k 1)
                (if (>= (+ k 1) (length vals))
                  (loop (+ i 1))
                  (inner-loop (+ k 1) (cdr vs)))))
            (loop (+ i 1))))))))

(define (get-last s vals part2)
  (get-first (string-reverse s) (map string-reverse vals) part2))

(define integer-lookup
  '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(define (getlines filename)
  (call-with-input-file filename (lambda (port) (read-lines port))))

(define (runner line part2)
  (+ (* 10 (get-first line integer-lookup part2)) (get-last line integer-lookup part2)))

(define (part1 line)
  (runner line #f))

(define (part2 line)
  (runner line #t))

; test runners
(let [(input (getlines "day01/test01.txt"))]
  (display (apply + (map part1 input))) (newline))

(let [(input (getlines "day01/test02.txt"))]
  (display (apply + (map part2 input))) (newline))

; for the input
(let [(input (getlines "day01/input.txt"))]
  (display (apply + (map part1 input))) (newline))

(let [(input (getlines "day01/input.txt"))]
  (display (apply + (map part2 input))) (newline))

