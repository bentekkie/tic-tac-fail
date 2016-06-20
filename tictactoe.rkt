#! /usr/bin/env racket
#lang racket
(require net/url)
(require (planet dmac/spin))
(define comp #\O)
(define human #\X)
(get "/playFirst"
     (lambda () (play (state->string (map string->list (string-split (ai-move test comp human) ","))))))
(get "/playSecond"
     (lambda () (play (state->string test))))
(get "/move/:state"
     
     (lambda (req)
       (local [(define currstate (map string->list (string-split (params req 'state) ",")))]
         (cond [(and (string=? "first" (params req 'pos)) (equal? currstate test))
                (ai-move currstate human comp)]
               [else (local [(define newmove (ai-move currstate human comp))]
                       (string-append newmove ))]))))

(define test '((#\. #\. #\.)(#\. #\. #\.)(#\. #\. #\.)))
(define test1 '((#\. #\. #\.)(#\. #\X #\.)(#\. #\. #\.)))
(define (make-move board coords player)
  (local[(define x (first coords))
         (define y (second coords))
         ]
  (foldr (lambda (a b c) (cons (cond
                                 [(= b y)
                                  (foldr (lambda (i j k)
                                           (cons (cond
                                                   [(= j x) player]
                                                   [else i])
                                                 k))
                                         empty
                                         a
                                         (build-list (length (first board)) identity))]
                                 [else a])
                               c))
         empty
         board
         (build-list (length board) identity))))

(define (ai-move board p1 p2)
  (local
    [(define moves (neighbours board p1 p2 p1 p2))
     (define result
       (cond
         [(= 1 (length moves)) (first moves)]
         [else (foldl (lambda (a b)
                        (cond
                          [(> (first a) (first b)) a]
                          [else b]))
                      (first moves)
                      (rest moves))]))
     (define new-board (rest (rest (rest (rest result)))))]
    (cond [(winner? new-board p2) (state->string new-board)]
          [(winner? new-board p1) (state->string new-board)]
          [(= 0 (length (empty-poss new-board)))(state->string new-board)]
          [else (state->string new-board)])))
            
(define (state->string state)
  (string-join (foldr (lambda (a b) (cons (list->string a) b)) empty state) ","))
(define (empty-poss board)
  (foldl (lambda (a b c)
           (append (foldl (lambda (x y z)
                            (cond
                              [(char=? x #\.) (cons (list y b) z)]
                              [else z]))
                    empty
                    a
                    (build-list (length a) identity))
                   c))
   empty
   board
   (build-list (length board) identity)))

(define (sum-rating lor)
  (list (foldl + 0 (map first lor))
        (foldl + 0 (map second lor))
        (foldl + 0 (map third lor))))

(define (neighbours board currplayer nextplayer p1 p2)
  (local [(define empty-list (empty-poss board))]
  (cond
    [(winner? board p1) (list (append (list 1 0 0 '()) board))]
    [(winner? board p2) (list (append (list 0 1 0 '()) board))]
    [(= 0 (length empty-list)) (list (append (list 0 0 1 '()) board))]
    [else (map (lambda (x) (append (sum-rating (neighbours (make-move board x nextplayer)
                                                           nextplayer
                                                           currplayer
                                                           p1 p2))
                                   (list x)
                                   (make-move board x nextplayer)))
               empty-list) ])))

(define (rotate board)
  (foldl (lambda (a b)
           (foldr (lambda (x y z)
                    (cons (cons x y) z))
                  empty
                  a
                  b))
         (map list (first board))
         (rest board)))
(define (diag? board off player)
  (or (empty? board)
      (= off (length (first board)))
      (and (char=? (list-ref (first board) off) player)
           (diag? (rest board) (+ off 1) player))))
(define (winner? board player)
  (or (ormap (lambda (x)
               (andmap (lambda (y) (char=? y player)) x))
             board)
      (ormap (lambda (x)
               (andmap (lambda (y) (char=? y player)) x))
             (rotate board))
      (diag? board 0 player)
      (diag? (rotate board) 0 player)))

(define (play state)
  (cond
    [(winner? (map string->list (string-split state ",")) comp)
     (string-join (cons (string-append (list->string (list comp)) " win <br>") (map (lambda (x) (list->string(append x (list #\newline)))) (map string->list (string-split state ",")))) "<br>")]
    [(winner? (map string->list (string-split state ",")) human)
     (string-join (cons (string-append (list->string (list human)) " win <br>") (map (lambda (x) (list->string(append x (list #\newline)))) (map string->list (string-split state ",")))) "<br>")]
    [(= 0 (length (empty-poss (map string->list (string-split state ",")))))
     (string-join (cons "Tie <br>" (map (lambda (x) (list->string(append x (list #\newline)))) (map string->list (string-split state ",")))) "<br>")]
    [else (local [(define resp (port->string (get-pure-port (string->url (string-append "http://localhost:8000/move/" state)))))]
            (play (ai-move (map string->list (string-split resp ",")) comp human)))]))

(run)
