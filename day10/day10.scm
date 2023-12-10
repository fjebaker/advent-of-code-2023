(import
  (chicken base)
  (chicken io)
  (chicken string)
  (chicken sort)
  (srfi 1)
  scheme
)

(define-constant NORTH 'NORTH)
(define-constant EAST 'EAST)
(define-constant SOUTH 'SOUTH)
(define-constant WEST 'WEST)
(define-constant INVALID 'INVALID)

(define (getlines filename) (call-with-input-file filename read-lines))

;; utility for getting grid dimensions
(define (size/x grid) (length (car grid)))
(define (size/y grid) (length grid))

;; utility for accessing the grid
(define (get-index grid x y) (list-ref (list-ref grid y) x))
(define (get-index/safe grid x y)
  (let [(Y (size/y grid)) (X (size/x grid))]
    (if (or (< x 0) (>= x X) (< y 0) (>= y Y))
      #\.
      (get-index grid x y))))

(define (get-surrounding grid x y)
  (list
    (cons NORTH (get-index/safe grid x (- y 1)))
    (cons EAST (get-index/safe grid (+ x 1) y))
    (cons SOUTH (get-index/safe grid x (+ y 1)))
    (cons WEST (get-index/safe grid (- x 1) y))))

(define (find-tile grid char)
  (let loop [(x 0) (y 0)]
    (if (>= y (size/y grid))
      (error "unreachable")
      (if (>= x (size/x grid))
        (loop 0 (+ y 1))
        (if (char=? char (get-index grid x y))
          (cons x y)
          (loop (+ x 1) y))))))

(define (traverse/step-v-pipe facing)
  (cond
    ((eq? facing NORTH) NORTH)
    ((eq? facing SOUTH) SOUTH)
    (else INVALID)))

(define (traverse/step-h-pipe facing)
  (cond
    ((eq? facing EAST) EAST)
    ((eq? facing WEST) WEST)
    (else INVALID)))

(define (traverse/step-L facing)
  (cond
    ((eq? facing SOUTH) EAST)
    ((eq? facing WEST) NORTH)
    (else INVALID)))

(define (traverse/step-J facing)
  (cond
    ((eq? facing SOUTH) WEST)
    ((eq? facing EAST) NORTH)
    (else INVALID)))

(define (traverse/step-7 facing)
  (cond
    ((eq? facing EAST) SOUTH)
    ((eq? facing NORTH) WEST)
    (else INVALID)))

(define (traverse/step-F facing)
  (cond
    ((eq? facing WEST) SOUTH)
    ((eq? facing NORTH) EAST)
    (else INVALID)))

(define (traverse/step char facing)
  (case char
    ((#\|) (traverse/step-v-pipe facing))
    ((#\-) (traverse/step-h-pipe facing))
    ((#\L) (traverse/step-L facing))
    ((#\J) (traverse/step-J facing))
    ((#\7) (traverse/step-7 facing))
    ((#\F) (traverse/step-F facing))))

(define (facing->step facing)
  (case facing
    ((INVALID) (cons 0 0))
    ((NORTH) (cons 0 -1))
    ((SOUTH) (cons 0 1))
    ((EAST) (cons 1 0))
    ((WEST) (cons -1 0))))

(define (traverse-until condition? grid x y facing)
  (let loop [(facing facing) (x x) (y y) (steps '())]
    (if (condition? (get-index grid x y))
      steps
      (let* [(new-facing (traverse/step (get-index grid x y) facing))
             (diffs (facing->step new-facing))]
        (if (eq? new-facing INVALID)
          '()
          (loop
            new-facing
            (+ x (car diffs))
            (+ y (cdr diffs))
            (cons (cons (get-index grid x y) (cons x y)) steps)))))))

(define (at-start? c)
  (char=? c #\S))

(define (valid-start? init)
  (not (char=? #\. (cdr init))))

(define (run-loops grid x y options)
  (if (null? options)
    (error "Ran out of options")
    (let* [(init (car options))
           (facing (car init))
           (char (cdr init))
           (diffs (facing->step facing))
           (loop (traverse-until
                  at-start?
                  grid
                  (+ x (car diffs))
                  (+ y (cdr diffs))
                  facing))]
      (if (eq? 0 (length loop))
        (run-loops grid x y (cdr options))
        (cons (cons #\S (cons x y)) loop)))))

(define (vertical? aitem bitem)
  (let [(a (cdr aitem)) (b (cdr bitem))]
    (let [(y1 (cdr a)) (y2 (cdr b))]
      (if (eq? y1 y2)
        (<= (car a) (car b))
        (<= y1 y2)))))

(define (area-between c1 c2)
  (- (cadr c1) (cadr c2)))

(define (next-line sorted-loop)
  (let line-builder [(height (cddar sorted-loop))
                     (sorted-loop sorted-loop)
                     (line '())]
    (if (eq? 0 (length sorted-loop))
      (cons sorted-loop (reverse line))
      (if (eq? height (cddar sorted-loop))
        (line-builder height (cdr sorted-loop) (cons (car sorted-loop) line))
        (cons sorted-loop (reverse line))))))

(define (is-clockwise s)
  (let [(x (car s))]
    (or (char=? #\| x) (char=? #\J x) (char=? #\L x))))

(define (next-clockwise line)
  (let loop [(line line) (counter 0)]
    (if (null? line)
      (cons '(()) 0)
      (if (is-clockwise (car line))
        (cons line counter)
        (loop (cdr line) (+ counter 1))))))

(define (calc-area sorted-loop)
  (let loop [(sorted-loop sorted-loop) (area 0)]
    (if (null? sorted-loop)
      area
      (let* [(carry (next-line sorted-loop))
             (line (cdr carry))]
        (let walk [(line line) (area area)]
          (if (null? line)
            (loop (car carry) area)
            (let* [(info1 (next-clockwise line))
                   (c1 (caar info1))
                   (info2 (next-clockwise (cdar info1)))
                   (c2 (caar info2))
                   (count (cdr info2))]
              (if (null? c2)
                (walk '() area)
                (let [(new-area (- (cadr c2) (cadr c1) count 1))]
                  (walk (cdar info2) (+ new-area area))
                  )))))))))

(let* [(lines (getlines "day10/input.txt"))
       (grid (map string->list lines))
       (start (find-tile grid #\S))
       (options (filter
                  valid-start?
                  (get-surrounding grid (car start) (cdr start))))
       (loop (run-loops grid (car start) (cdr start) options))
       (sorted-loop (sort loop vertical?))]
  (print "Part1: " (round (/ (length loop) 2)))
  (print "Part2: " (calc-area sorted-loop))
  )
