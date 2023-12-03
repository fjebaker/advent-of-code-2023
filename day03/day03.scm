(import
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken string)
  (srfi 1)
  ; hash tables
  (srfi 69)
  defstruct
  scheme
)

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

;; return 8 surrounding elements
;;    0 1 2
;;    3   4
;;    5 6 7
(define (get-surrounding grid x y)
  (let loop [(vs '()) (i -1) (j -1)]
    (unless (and (equal? i 0) (equal? j 0))
      (set! vs (cons (get-index/safe grid (+ x i) (+ y j)) vs)))
    (cond
      ((and (equal? i 1) (equal? j 1)) vs)
      ((equal? i 1) (loop vs -1 (+ j 1)))
      (else (loop vs (+ i 1) j)))))

(define (char-is-symbol? c)
  (not (or (char-numeric? c) (equal? c #\.))))

;; structure representing a number in the grid
(defstruct schema/number
  (adjacent-symbols '())
  (value 0))

(define (display/schema-number n)
  (printf
    "Number ~A [~A] ~%"
    (schema/number-value n)
    (schema/number-adjacent-symbols n)))

(define (char->number a) (- (char->integer a) (char->integer #\0)))

;; digits are passed in the reverse order
(define (digits->value digits)
  (let loop [(total 0) (power 0) (digits digits)]
    (if (null? digits)
      total
      (loop
        (+ total (* (expt 10 power) (char->number (car digits))))
        (+ power 1)
        (cdr digits)))))

(define (symbols-positions surrounding x y)
  (let loop [(rem (reverse surrounding)) (i -1) (j -1) (s-positions '())]
    (let [(x (+ x i)) (y (+ y j))] ; x and y of current tile
      (if (and (equal? i 0) (equal? j 0))
        ; skip middle without (cdr rem)
        (loop rem (+ i 1) j s-positions)
        (let [(c (car rem))]
          (when (char-is-symbol? c)
            (set! s-positions (cons (list c x y) s-positions)))
          (cond
            ((and (equal? i 1) (equal? j 1)) s-positions)
            ((equal? i 1) (loop (cdr rem) -1 (+ j 1) s-positions))
            (else (loop (cdr rem) (+ i 1) j s-positions))))))))

(define (unique l)
  (let loop [(rem l) (new '())]
    (if (null? rem)
      (reverse new)
      (let [(el (car rem))]
        (if (not (member el new))
          (loop (cdr rem) (cons el new))
          (loop (cdr rem) new))))))

;; given position of the first digit of the number at (x, y)
;; parses the number from the grid into a `schema/number`
(define (parse-schema/number grid x y)
  (let stepper [(i x) (digits '()) (adjacent-symbols '())]
    (let [(digit (get-index/safe grid i y))]
      (if (not (char-numeric? digit))
        (list
          ;; return the number and the new x index
          (make-schema/number
            adjacent-symbols: (unique adjacent-symbols)
            value: (digits->value digits))
          i)
        (let* [(surrounding (get-surrounding grid i y))
               ; (symbols (filter char-is-symbol? surrounding))]
               (symbols (symbols-positions surrounding i y))]
          (stepper (+ i 1) (cons digit digits) (append symbols adjacent-symbols)))))))

(define (parse-schema/row grid y)
  (let loop [(x 0) (numbers '())]
    (if (>= x (size/x grid))
      numbers
      (let [(c (get-index/safe grid x y))]
        (if (char-numeric? c)
          (let [(info (parse-schema/number grid x y))]
            (loop (cadr info) (cons (car info) numbers)))
          (loop (+ x 1) numbers))))))

(define (parse-schema grid)
  (let loop [(y 0) (numbers '())]
    (if (>= y (size/y grid))
      numbers
      (loop (+ y 1) (append (parse-schema/row grid y) numbers)))))

(define (is-adjacent? n)
  (> (length (schema/number-adjacent-symbols n)) 0))

(define (is-cog-number? n)
  (let [(cog-filter (lambda (syms) (equal? (car syms) #\*)))]
    (> (length (filter cog-filter (schema/number-adjacent-symbols n))) 0)))

;; add all of the numbers to the hash table
(define (add-to-table! table n)
  (let loop [(rem (schema/number-adjacent-symbols n))]
    (unless (null? rem)
      (hash-table-update!/default
        table
        (cdar rem)
        (lambda (existing) (cons (schema/number-value n) existing))
        '())
      (loop (cdr rem)))))

(define (get-valid-cogs table)
  (let [(valids '())]
    (hash-table-walk
      table
      (lambda (key value)
        (when (equal? (length value) 2) (set! valids (cons value valids)))))
    valids))

(define (getlines filename)
  (call-with-input-file filename (lambda (port) (read-lines port))))

(let [(lines (getlines "day03/input.txt"))]
  (let* [(grid (map string->list lines))
         (numbers (parse-schema grid))
         (numbers/adjacent (filter is-adjacent? numbers))]
    (print "Part 1: " (apply + (map schema/number-value numbers/adjacent)))
    (let [(cog-numbers (filter is-cog-number? numbers/adjacent))
           (cog-table (make-hash-table))]
      (for-each (lambda (n) (add-to-table! cog-table n)) cog-numbers)
      (let* [(cogs (get-valid-cogs cog-table))]
        (print "Part 2: " (apply + (map (lambda (items) (apply * items)) cogs)))))))
