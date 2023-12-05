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

(defstruct mapping
  from
  to
  ranges)

(define (display/mapping mapping)
  (print
    (mapping-from mapping) " -> " (mapping-to mapping) " "
    (mapping-ranges mapping)
    ))

(define (parse/values line)
  (map string->number (string-split line)))

(define (parse/seeds line)
  (parse/values (cadr (string-split line ":"))))

(define (parse/from-to line)
  (let* [(parts (string-split (car (string-split line " ")) "-"))
         (from (car parts))
         (to (caddr parts))]
    (list from to)))

(define (parse/mapping lines)
  (let loop [(rem lines) (ranges '())]
    (if (equal? 1 (length rem))
      ; only the from-to left to parse
      (let [(from-to (parse/from-to (car rem)))]
        (make-mapping from: (car from-to) to: (cadr from-to) ranges: ranges))
      (loop (cdr rem) (cons (parse/values (car rem)) ranges)))))

(define (add-to-lookup! table mapping)
  (hash-table-set! table (mapping-from mapping) mapping))

(define (parse/mappings lines)
  (let [(lookup (make-hash-table))]
    (let loop [(rem lines) (accum '())]
      (if (or (null? rem) (equal? 0 (string-length (car rem))))
        (let [(mapping (parse/mapping accum))]
          (add-to-lookup! lookup (parse/mapping accum))
          (if (null? rem)
            lookup
            (loop (cdr rem) '())))
        (loop (cdr rem) (cons (car rem) accum))))))

(define (update-seed seed ranges)
  (if (null? ranges)
    seed
    (let* [(range (car ranges))
           (start (cadr range))
           (end (car range))
           (len (- (caddr range) 1))
           (offset (- seed start))]
      (if (and (>= offset 0) (<= offset len))
        (+ end offset)
        (update-seed seed (cdr ranges))))))

(define (range/less-than r1 r2)
  (let* [(offset (- (car r1) (car r2)))]
    (if (< offset 0)
      (list (car r1) (min (- offset) (cadr r1)))
      '())))

(define (range/more-than r1 r2)
  (let* [(end1 (+ (car r1) (cadr r1)))
         (end2 (+ (car r2) (cadr r2)))
         (offset (- end2 end1))]
    (if (< offset 0)
      (list (max (car r1) (+ end1 offset)) (min (cadr r1) (- offset)))
      '())))

(define (range/intersection r1 r2)
  (let* [(start1 (car r1))
         (start2 (car r2))
         (end1 (+ start1 (cadr r1)))
         (end2 (+ start2 (cadr r2)))
         (diff/a (- end1 start2))
         (diff/b (- start1 end2))
         ]
    (if (not (equal? (signum diff/a) (signum diff/b)))
      (let* [(new-start (max start1 start2))
            (new-end (min end1 end2))
            (len (- new-end new-start))]
        (if (> len 0)
          (list new-start (- new-end new-start))
          '()))
      '())))

;; returns '((r1 less than r2) (r1 in r2) (r1 greater than r2))
(define (range/diff-remap r1 range)
  (let* [(r2 (cdr range))
        (int (range/intersection r1 r2))
        (rem (list (range/less-than r1 r2) (range/more-than r1 r2)))]
    (list
      (if (not (null? int))
        ; start of the range plus the offset
        (list (+ (car range) (- (car int) (cadr range))) (cadr int))
        '())
      (filter (compose not null?) rem))))

(define (update-seed-ranges seed-ranges ranges)
  (let accumulator [(ranges ranges) (rem seed-ranges) (new '())]
    ; (print "--> " rem " new " new)
    (if (or (null? ranges) (null? rem))
      (append rem new)
      (let* [(updates
              (map
                (lambda (sr)
                  (range/diff-remap sr (car ranges)))
                rem))
             (intersects (map car updates))
             (differences (flatten (map cdr updates)))]
        (when (not (null? differences))
          (set! differences (list->pairs differences)))
        (accumulator
          (cdr ranges)
          differences
          (filter (compose not null?) (append intersects new)))))))

(define (next-seed-numbers seeds almanac current updater)
  (if (string=? current "location")
    seeds
    (let* [(current (hash-table-ref almanac current))
           (ranges (mapping-ranges current))
           (next (mapping-to current))]
      (next-seed-numbers
        ((updater ranges) seeds)
        almanac
        next
        updater))))

(define (list->pairs items)
  (let loop [(items items) (pairs '())]
    (if (null? items)
      pairs
      (loop (cddr items) (cons (list (car items) (cadr items)) pairs)))))

(define ((part1-updater ranges) seeds)
  (map (lambda (seed) (update-seed seed ranges)) seeds))

(define ((part2-updater ranges) seed-ranges)
  (update-seed-ranges seed-ranges ranges))

(define (getlines filename) (call-with-input-file filename read-lines))

(let [(lines (getlines "day05/input.txt"))]
  (let* [(seeds (parse/seeds (car lines)))
         (seed-ranges (list->pairs seeds))
         (almanac (parse/mappings (cddr lines)))
         (part1 (next-seed-numbers seeds almanac "seed" part1-updater))
         (part2 (next-seed-numbers seed-ranges almanac "seed" part2-updater))]
    (print "Part1: " (apply min part1))
    (print "Part2: " (apply min (map car part2)))))
