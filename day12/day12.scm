(import
  (chicken base)
  (chicken io)
  (chicken string)
  (srfi 1)
  memoize
  scheme
)

(define (getlines filename) (call-with-input-file filename read-lines))

(define (is-in-next-n? char n s)
  (let loop [(s s) (n n)]
    (cond
      ((null? s) #f)
      ((eq? 0 n) #f)
      ((char=? char (car s)) #t)
      (else (loop (cdr s) (- n 1))))))

(define (is-in? char s)
  (is-in-next-n? char (length s) s))

(define (skip s l)
  (if (eq? l 0)
    s
    (skip (cdr s) (- l 1))))

(define (count-combinations s groupings)
  (define (inner s j)
    (if (eq? j (length groupings))
      (if (is-in? #\# s)
        0
        1)
      (let* [(group (list-ref groupings j))
            (n (+ 1 group))
            (x 0)]
        (if (< (length s) n)
          0
          (begin
            (when (not (char=? #\# (car s)))
              (let [(k (memoed-inner (cdr s) j))]
                (set! x (+ x k))))
            (when (and
                    (not (is-in-next-n? #\. group s))
                    (not (char=? #\# (list-ref s group))))
              (let [(k (memoed-inner (skip s n) (+ j 1)))]
                (set! x (+ x k))))
            x)))))
  (define memoed-inner (memoize inner))
  (memoed-inner s 0))

(define (parse-line line)
  (let* [(sections (string-split line ", "))
         (s (string->list (car sections)))
         (groupings (map string->number (cdr sections) ))]
    (cons s groupings)))

(define (append-and-run sg)
  (count-combinations (append (car sg) '(#\.)) (cdr sg)))

(define ((repeat-input n) sg)
  (let loop [(n n) (s-accum '()) (g-accum '())]
    (if (eq? 0 n)
      (cons (join s-accum '(#\?)) g-accum)
      (loop (- n 1) (cons (car sg) s-accum) (append (cdr sg) g-accum)))))

(let* [(lines (getlines "day12/input.txt"))
       (parsed (map parse-line lines))
       (combinations (map append-and-run parsed))
       (repeated-parsed (map (repeat-input 5) parsed))
       (repeated-combinations (map append-and-run repeated-parsed))]
  (print "Part1: " (apply + combinations))
  (print "Part2: " (apply + repeated-combinations))
  )
