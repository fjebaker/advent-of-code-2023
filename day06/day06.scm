(import
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken string)
  (srfi 1)
  (srfi 69)
  defstruct
  scheme
)

(define (getlines filename) (call-with-input-file filename read-lines))

(define (line->numbers line)
  (map string->number (string-split (cadr (string-split line ":")))))

(define (roots total-time distance-needed)
  (let* [(disc (sqrt (- (expt total-time 2) (* 4 distance-needed))))
         (root1 (ceiling (/ (+ total-time disc) 2)))
         (root2 (floor (/ (- total-time disc) 2)))]
    (list root1 root2)))

(define (count-options roots)
  (- (- (car roots) (cadr roots)) 1))

(define (dual-map f l1 l2)
  (let loop [(l1 l1) (l2 l2) (accum '())]
    (if (or (null? l1) (null? l2))
      accum
      (loop (cdr l1) (cdr l2) (cons (f (car l1) (car l2)) accum)))))

(define (reinterpret-numbers nums)
  (string->number (apply conc (map ->string nums))))

(let* [(lines (getlines "day06/input.txt"))
       (times (line->numbers (car lines)))
       (distances (line->numbers (cadr lines)))
       (t2 (reinterpret-numbers times))
       (d2 (reinterpret-numbers distances))]
  (print "Part1: " (apply * (map count-options (dual-map roots times distances))))
  (print "Part2: " (count-options (roots t2 d2))))
