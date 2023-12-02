(import
  (chicken base)
  (chicken io)
  (chicken string)
  (srfi 1)
  defstruct
  scheme
)

(defstruct game/cubes
  (blue 0)
  (green 0)
  (red 0))

(defstruct game
  id rounds)

(define (parse/game-id line)
  (let [(prefix (string-split line ":"))]
    (string->number (cadr (string-split (car prefix))))))

(define (parse/game-round text)
  (let loop [(cubes (string-split text " ,")) (r (make-game/cubes))]
    (if (null? cubes)
      r
      (let [(num (string->number (car cubes)))
             (color (cadr cubes))]
        (cond
          ((string=? color "blue") (game/cubes-blue-set! r (+ (game/cubes-blue r) num)))
          ((string=? color "red") (game/cubes-red-set! r (+ (game/cubes-red r) num)))
          ((string=? color "green") (game/cubes-green-set! r (+ (game/cubes-green r) num)))
          (else (error cubes)))
        (loop (cddr cubes) r)))))

(define (parse/game-rounds line)
  (let [(prefix (cadr (string-split line ":")))]
    (map parse/game-round (string-split prefix ";"))))

(define (parse/game line)
  (let [(id (parse/game-id line)) (rounds (parse/game-rounds line))]
    (make-game id: id rounds: rounds)))

(define (max/cubes game)
  (let loop [(rounds (game-rounds game)) (blue 0) (red 0) (green 0)]
    (if (null? rounds)
      (make-game/cubes blue: blue red: red green: green)
      (let* [(r (car rounds))
             (b (game/cubes-blue r))
             (g (game/cubes-green r))
             (r (game/cubes-red r))]
        (loop (cdr rounds) (max blue b) (max red r) (max green g))))))

(define (is-possible cubes)
  (and
    (<= (game/cubes-red cubes) 12)
    (<= (game/cubes-green cubes) 13)
    (<= (game/cubes-blue cubes) 14)))

(define (filter/predicated predicates items)
  (let loop [(p predicates) (i items) (acc '())]
    (if (or (null? p) (null? i))
      acc
      (if (car p)
        (loop (cdr p) (cdr i) (cons (car i) acc))
        (loop (cdr p) (cdr i) acc)))))

(define (calc-power cube)
  (* (game/cubes-red cube) (game/cubes-blue cube) (game/cubes-green cube)))

(define (getlines filename)
  (call-with-input-file filename (lambda (port) (read-lines port))))

(define (solver filename)
  (let* [(lines (getlines filename))
         (games (map parse/game lines))
         (maximums (map max/cubes games))
         (possible (map is-possible maximums))]
    (display "Part1: ") 
    (display (apply + (map (lambda (g) (game-id g)) (filter/predicated possible games))))
    (newline)
    (display "Part2: ") 
    (display (apply + (map calc-power maximums)))
    (newline)))

(solver "day02/test.txt")

(solver "day02/input.txt")
