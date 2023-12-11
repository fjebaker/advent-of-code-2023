(import
  (chicken base)
  (chicken io)
  (chicken string)
  (srfi 1)
  scheme
)

(define (getlines filename) (call-with-input-file filename read-lines))

(define (transpose grid)
  (let loop [(new '()) (i 0)]
    (if (>= i (length (car grid)))
      (reverse new)
      (loop (cons (map (lambda (row) (list-ref row i)) grid) new) (+ i 1)))))

(define (is-empty? char)
  (char=? char #\.))

(define (expand/rows grid)
  (let loop [(grid grid) (new '())]
    (if (null? grid)
      (reverse new)
      (let [(row (car grid))]
        (if (every is-empty? (car grid))
          (loop (cdr grid) (cons row (cons row new)))
          (loop (cdr grid) (cons row new)))))))

(define (find-zeroes rows)
  (let loop [(i 0) (rows rows) (indices '())]
    (if (null? rows)
      indices
      (if (every is-empty? (car rows))
        (loop (+ i 1) (cdr rows) (cons i indices))
        (loop (+ i 1) (cdr rows) indices)))))

(define (expand/universe grid)
  (expand/rows (transpose (expand/rows (transpose grid)))))

(define (find/galaxies grid)
  (let loop/row [(y 0) (grid grid) (coords '())]
    (if (null? grid)
      coords
      (let loop/col [(x 0) (row (car grid)) (coords coords)]
        (if (null? row)
          (loop/row (+ y 1) (cdr grid) coords)
          (if (char=? #\# (car row))
            (loop/col (+ x 1) (cdr row) (cons (cons x y) coords))
            (loop/col (+ x 1) (cdr row) coords)))))))

(define (dist coord1 coord2)
  (+ (abs (- (car coord2) (car coord1))) (abs (- (cdr coord2) (cdr coord1)))))

(define (solve/min-dist coords)
  (let loop [(rem coords) (dists '())]
    (if (null? rem)
      dists
      (let inner-loop [(d 0) (coords coords)]
        (if (null? coords)
          (loop (cdr rem) (cons d dists))
          (let [(k (dist (car coords) (car rem)))]
            (inner-loop (+ d k) (cdr coords))))))))

(define (inflate indices rows cols expansion)
  (map (lambda (index)
         (let loop [(rows rows) (cols cols) (new-index index)]
           (let [(x (car new-index)) (y (cdr new-index))]
             (cond
               ((and (null? rows) (null? cols)) new-index)
               ((null? rows)
                (let [(col (car cols))]
                  (if (>= (car index) col)
                    (loop rows (cdr cols) (cons (+ x expansion) y))
                    (loop rows (cdr cols) new-index))))
               (else
                (let [(row (car rows))]
                  (if (>= (cdr index) row)
                    (loop (cdr rows) cols (cons x (+ y expansion)))
                    (loop (cdr rows) cols new-index))))))))
       indices))

(define (adjust-for-inflation grid galaxy-indices expansion)
  (let [(row-indices (find-zeroes grid))
        (col-indices (find-zeroes (transpose grid)))]
    (inflate galaxy-indices row-indices col-indices expansion)))

(let* [(lines (getlines "day11/input.txt"))
      (universe (map string->list lines))
      (galaxy-indices (find/galaxies universe))
      (indices (adjust-for-inflation universe galaxy-indices 1))
      (big-indices (adjust-for-inflation universe galaxy-indices (- 1000000 1)))
      ]
  (print (/ (apply + (solve/min-dist indices)) 2))
  (print (/ (apply + (solve/min-dist big-indices)) 2))
  )
