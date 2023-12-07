(import
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken string)
  (chicken sort)
  (chicken plist)
  scheme
)

(define ((make-card->value J) c)
  (case c
    ((#\A) 14)
    ((#\K) 13)
    ((#\Q) 12)
    ((#\J) J)
    ((#\T) 10)
    (else (- (char->integer c) (char->integer #\0)))))

(define (hand->plist h)
  (set! (symbol-plist 'pl) '())
  (let loop [(cards (car h))]
    (if (null? cards)
      'pl
      (let* [(c (car cards))
             (prop (get 'pl c))]
        (if prop
          (put! 'pl c (+ 1 prop))
          (put! 'pl c 1))
        (loop (cdr cards))))))

(define-constant *five-of-kind* 6)
(define-constant *four-of-kind* 5)
(define-constant *full-house* 4)
(define-constant *three-of-kind* 3)
(define-constant *two-pair* 2)
(define-constant *pair* 1)
(define-constant *high-card* 0)

(define (bump-jokers t count)
  (if (equal? 0 count)
    t
    (begin
      (bump-jokers
        (cond
          ((equal? t *pair*) *three-of-kind*)
          ((equal? t *two-pair*) *full-house*)
          ((equal? t *three-of-kind*) *four-of-kind*)
          (else (min (+ 1 t) 6)))
        (- count 1)))))

(define (hand->type/impl index num-cards)
  (cond
    ((equal? 1 num-cards) *five-of-kind*)
    ((equal? 2 num-cards) ; could be 4 or full house
     (if (or (equal? 3 (cadr index)) (equal? 2 (cadr index)))
       *full-house*
       *four-of-kind*))
    ((equal? 3 num-cards) ; could be three of a kind of two pair
     (if (or (equal? 3 (list-ref index 1))
             (and (>= (length index) 3) (equal? 3 (list-ref index 3)))
             (and (>= (length index) 5) (equal? 3 (list-ref index 5))))
       *three-of-kind*
       *two-pair*))
    ((equal? 4 num-cards) *pair*)
    ((equal? 5 num-cards) *high-card*)))

(define (hand->type h with-jokers)
  (let* [(symb (hand->plist h))
        (num-jokers (get symb #\J 0))]
    (let* [(index (symbol-plist symb))
           (len (+ (/ (length index) 2)
                   ; avoid counting e.g. two jokers as a pair
                   (if with-jokers (max 0 (- num-jokers 1)) 0)))
           (t (hand->type/impl index len))]
      (bump-jokers t num-jokers))))

(define ((hand/cards<? card->value) h1 h2)
  (let loop [(h1 (car h1)) (h2 (car h2))]
    (when (or (null? h1) (null? h2))
      (error "unreachable"))
    (let [(c1 (card->value (car h1))) (c2 (card->value (car h2)))]
      (if (equal? c1 c2)
        (loop (cdr h1) (cdr h2))
        (< c1 c2)))))

(define ((make-hand<? J skip) h1 h2)
  (let [(t1 (hand->type h1 skip))
        (t2 (hand->type h2 skip))]
    (if (equal? t1 t2)
      ((hand/cards<? (make-card->value J)) h1 h2)
      (< t1 t2))))

(define (getlines filename) (call-with-input-file filename read-lines))

(define (parse/hand line)
  (let [(comps (string-split line))]
    (cons (string->list (car comps)) (string->number (cadr comps)))))

(define (calculate-score ranked)
  (let loop [(ranked ranked) (i 1) (total 0)]
    (if (null? ranked)
      total
      (loop (cdr ranked) (+ i 1) (+ total (* i (cdar ranked)))))))

(let* [(lines (getlines "day07/input.txt"))
       (hands (map parse/hand lines))
       (ranked (sort hands (make-hand<? 11 #f ))) ; J == jack
       (ranked-jokers (sort hands (make-hand<? 0 #t)))] ; J == joker
  (print (calculate-score ranked))
  (print (calculate-score ranked-jokers))
  )
