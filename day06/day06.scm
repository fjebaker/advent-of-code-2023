(import
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken string)
  scheme
)

(define (getlines filename) (call-with-input-file filename read-lines))

(define (line->numbers line)
  (map string->number (string-split (cadr (string-split line ":")))))

(define (roots total-time distance-needed)
  (let* [(disc (sqrt (- (expt total-time 2) (* 4 distance-needed))))
         (root1 (ceiling (/ (+ total-time disc) 2)))
         (root2 (floor (/ (- total-time disc) 2)))]
    (cons (inexact->exact root1) (inexact->exact root2))))

(define (count-options roots) (- (car roots) (cdr roots) 1))

(define (reinterpret-numbers nums)
  (string->number (apply conc (map ->string nums))))

(let* [(lines (getlines "day06/input.txt"))
       (times (line->numbers (car lines)))
       (distances (line->numbers (cadr lines)))
       (t2 (reinterpret-numbers times))
       (d2 (reinterpret-numbers distances))]
  (print "Part1: " (apply * (map count-options (map roots times distances))))
  (print "Part2: " (count-options (roots t2 d2))))
