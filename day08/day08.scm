(import
  (chicken base)
  (chicken io)
  (chicken irregex)
  (srfi 69)
  (srfi 1)
  scheme
)

(define (getlines filename) (call-with-input-file filename read-lines))

(define (parse/line line)
  (let [(matches (irregex-search
                   '(: ($ (+ alphanum)) " = (" ($ (+ alphanum)) ", " ($ (+ alphanum)) ")")
                   line))]
    (list
      (irregex-match-substring matches 1)
      (irregex-match-substring matches 2)
      (irregex-match-substring matches 3))))

(define (parse/map lines)
  (let loop [(lines lines) (lookup (make-hash-table))]
    (if (null? lines)
      lookup
      (let* [(line (car lines))
             (comps (parse/line line))]
        (hash-table-set! lookup (car comps) (cdr comps))
        (loop (cdr lines) lookup)))))

(define (follow-path-until condition start full-path lookup)
  (let loop [(current start) (path full-path) (step 0)]
    (when (null? path) (set! path full-path))
    ; (print current " -> " (car path) " via " (hash-table-ref lookup current))
    (if (condition current)
      step
      (let [(options (hash-table-ref lookup current))
            (direction (car path))]
        (loop
          (if (char=? direction #\L) (car options) (cadr options))
          (cdr path)
          (+ step 1))))))


(define ((ends-with-? char) s)
  (char=? (string-ref s 2) char))

(define (get-starts lookup)
  (filter (ends-with-? #\A) (hash-table-keys lookup)))

(define (steps-to-ends path lookup)
  (let* [(starts (get-starts lookup))]
    (let loop [(starts starts) (steps '())]
      (if (null? starts)
        steps
        (loop
          (cdr starts)
          (cons (follow-path-until
                  (ends-with-? #\Z)
                  (car starts)
                  path
                  lookup) steps))))))

(let* [(lines (getlines "day08/input.txt"))
       (path (string->list (car lines)))
       (lookup (parse/map (cddr lines)))]
  (print "Part1: " (follow-path-until (lambda (c) (string=? "ZZZ" c)) "AAA" path lookup))
  (print "Part2: " (apply lcm (steps-to-ends path lookup)))
  )
