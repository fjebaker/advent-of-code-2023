(import
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken string)
  (srfi 1)
  defstruct
  scheme
)

(define (count-intersection nums1 nums2)
  (length (set/intersection nums1 nums2)))

(define (parse/numbers s)
  (map string->number (string-split s)))
(define (parse/card-num s)
  (string->number (cadr (string-split s))))

(define (parse/card line)
  (let* [(parts (string-split line ":"))
         (all-numbers (string-split (cadr parts) "|"))
         (winning-nums (parse/numbers (car all-numbers)))
         (card-nums (parse/numbers (cadr all-numbers)))
         (score (count-intersection winning-nums card-nums))]
    (list (parse/card-num (car parts)) score winning-nums card-nums)))

(define (set/intersection l1 l2)
  (let accumulator [(rem l2) (l l1) (elements '())]
    (if (null? rem)
      elements
      (if (null? l)
        (accumulator (cdr rem) l1 elements)
        (let [(curr (car rem)) (other (car l))]
          (when (equal? curr other) (set! elements (cons curr elements)))
          (accumulator rem (cdr l) elements))))))

(define (count-cards! tally cards)
  (let loop [(cards cards)]
    (unless (null? cards)
      (let* [(current (car cards))
            (card-num (- (car current) 1)) ; subtract one for indexing
            (score (list-ref current 1))
            (factor (vector-ref tally card-num))]
        (let inner-loop [(i 1) (remaining score)]
          (if (<= remaining 0)
            (loop (cdr cards))
            (let [(index (+ card-num i))]
              (vector-set! tally index (+ (vector-ref tally index) factor))
              (inner-loop (+ i 1) (- remaining 1)))))))))

(define (calc-score card)
  (let [(num-winning (list-ref card 1))]
    (if (equal? num-winning 0)
      0
      (expt 2 (- num-winning 1)))))

(define (getlines filename) (call-with-input-file filename read-lines))

(let [(lines (getlines "day04/input.txt"))]
  (let* [(cards (map parse/card lines))
         (scores (map calc-score cards))
         ; counts all 1, since we have one copy of each to start with
         (counts (make-vector (length cards) 1))]
    (count-cards! counts cards)
    (print "Part 1: " (apply + scores))
    (print "Part 2: " (apply + (vector->list counts)))))
