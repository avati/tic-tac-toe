#!/usr/bin/guile \
-e main -s 
!#

;;;
;;; tic-tac-toe v2
;;;
;;; the tic-tac-toe game layout is considered a single list
;;; numbered 0 - 8 where 0 to 2 is top row, 3 to 5 the middle
;;; row and 6 to 8 the bottom row, similarly right most
;;; column is 2-5-8. The indices look like this:
;;;
;;;         0   1   2
;;;         3   4   5
;;;         6   7   8
;;;
;;;
;;;  each index is a container for a value, #f means still unfilled
;;;  'x means the program's move, 'y is the user's move.
;;;
;;; the 'my' in the code refer's to the "program" AI mind, the user
;;; is referred to as 'her'
;;;
;;;
;;; Suggestions and improvements submitted to avati@80x25.org will be
;;; appreciated
;;;

; to get line number on runtime exceptions
(use-modules (ice-9 debugger))
(use-modules (ice-9 rdelim))


; Identity function for using with and-map
(define (I x)
  x)


; equivalent of python's "if member in list:"
(define (has the-list member)
  (cond ((equal? the-list '())
	 #f)
	((equal? (car the-list) member)
	 #t)
	(else
	 (has (cdr the-list) member))))


; standard reduce
(define (reduce op list)
  (define (reduce-inner op list ans)
    (if (equal? list '())
        ans
        (reduce-inner op (cdr list) (op ans (car list)))))
  (reduce-inner op (cdr list) (car list)))


; (range x) returns a list (0 .. x-1)
(define (range x)
  (letrec ((range-inner (lambda (x half-ans)
			  (if (= x 0) 
			      half-ans
			      (range-inner (- x 1)
					   (append (list (- x 1))
						   half-ans))))))
    (range-inner x (list))))


; remove elements from thelist if the predicate 'func' returns #f 
; for those elements and return as a new list
(define (filter func thelist)
  (letrec ((filter-inner (lambda (func thelist ans-list)
			   (cond ((equal? thelist '())
				  ans-list)
				 ((func (car thelist))
				  (filter-inner func 
						(cdr thelist)
						(append ans-list
							(list (car thelist)))))
				 (else
				  (filter-inner func
						(cdr thelist)
						ans-list))))))
    (filter-inner func thelist '())))


; count the number of occurances of val in thelist
(define (count thelist val)
  (length (filter (lambda (x)
		    (equal? x val))
		  thelist)))


; empty game layout to begin with (all are #f meaning all are unfilled)
(define initial-soln-space (list #f #f #f
				 #f #f #f
				 #f #f #f))


; toggle switch to alternate user's move and program's move.
; #t means the program shold make the next mark and #f means 
; it is the user's turn. this is used in (ttt-solve) procedure

(define now-my-move #f)


; helper for (ttt-solved?)
(define (filled-and-equal? the-list three-indices)
  (and (list-ref the-list (car three-indices))
       (equal? (list-ref the-list (car three-indices))
	       (list-ref the-list (cadr three-indices))
	       (list-ref the-list (caddr three-indices)))))

; given a tic-tac-toe layout it determines whether
; the game is over, if so who won or was it a tie, or
; if the game is still unsolved
; 
; return values:
;
;    'x    -   the program won
;    'y    -   the user won
;    #t    -   it was a tie
;    #f    -   game should still continue

(define (ttt-solved? soln-space)
  (cond ((filled-and-equal? soln-space '(0 1 2))
	 (list-ref soln-space 0))
	((filled-and-equal? soln-space '(3 4 5))
	 (list-ref soln-space 3))
	((filled-and-equal? soln-space '(6 7 8))
	 (list-ref soln-space 6))
	((filled-and-equal? soln-space '(0 3 6))
	 (list-ref soln-space 0))
	((filled-and-equal? soln-space '(1 4 7))
	 (list-ref soln-space 1))
	((filled-and-equal? soln-space '(2 5 8))
	 (list-ref soln-space 2))
	((filled-and-equal? soln-space '(0 4 8))
	 (list-ref soln-space 0))
	((filled-and-equal? soln-space '(2 4 6))
	 (list-ref soln-space 2))
	((and-map I soln-space)
	 #t)
	(else
	 #f)))

; procedure to display the solution space in a 3x3 matrix style

(define (display-or-space val)
  (if val
      (display val)
      (display " ")))

(define (ttt-show soln-space)
  (display "Computer's mark is x, user's mark is y:") 
  (newline) (newline)
  (display "    0  ")
  (display-or-space (list-ref soln-space 0)) 
  (display "   |1  ")
  (display-or-space (list-ref soln-space 1)) 
  (display "   |2  ")
  (display-or-space (list-ref soln-space 2)) 
  (newline)
  (display "   --------+-------+-------")
  (newline)
  (display "    3  ")
  (display-or-space (list-ref soln-space 3)) 
  (display "   |4  ")
  (display-or-space (list-ref soln-space 4)) 
  (display "   |5  ")
  (display-or-space (list-ref soln-space 5)) 
  (newline)
  (display "   --------+-------+-------")
  (newline)
  (display "    6  ")
  (display-or-space (list-ref soln-space 6)) 
  (display "   |7  ")
  (display-or-space (list-ref soln-space 7)) 
  (display "   |8  ")
  (display-or-space (list-ref soln-space 8)) 
  (newline)
  (newline))

; part of the strategy
(define (pick-random-move winning-moves)
  (list-ref winning-moves (random (length winning-moves)
                                  (seed->random-state (tms:clock (times))))))

; get the "other" mark. given 'x, return 'y and vice versa
(define (other mark)
  (if (equal? mark 'x)
      'y
      'x))

(define (list-set list idx val)
  (let ((new-list (list-copy list)))
    (list-set! new-list idx val)
    new-list))


(define (add-lists list-a list-b)
  (list (+ (car list-a) (car list-b))
        (+ (cadr list-a) (cadr list-b))
        (+ (caddr list-a) (caddr list-b))))


(define Tx '(-1 0 0))
(define Ty '(0 -1 0))
(define Tt '(0 0 -1))

(define (try-move-deep soln-space mark idx)
  (let* ((marked-soln-space (list-set soln-space idx mark))
         (verdict (ttt-solved? marked-soln-space)))
    (cond ((equal? verdict 'x)
           Tx)
          ((equal? verdict 'y)
           Ty)
          ((equal? verdict #t)
           Tt)
          (else
           (reduce add-lists (filter list?
                                     (get-moves marked-soln-space
                                                (other mark))))))))

(define (try-move-shallow soln-space mark idx)
  (let* ((marked-soln-space (list-set soln-space idx mark))
         (verdict (ttt-solved? marked-soln-space)))
    (cond ((equal? verdict 'x)
           Tx)
          ((equal? verdict 'y)
           Ty)
          ((equal? verdict #t)
           Tt)
          (else
           #f))))


(define (is-terminal? x)
  (cond ((equal? x Tx) #t)
        ((equal? x Ty) #t)
        ((equal? x Tt) #t)
        (else #f)))


(define (generate-soln-space soln-space mark try-move-fn)
  (map (lambda (idx)
         (let ((pos-val (list-ref soln-space idx)))
           (if pos-val
               pos-val
               (try-move-fn soln-space mark idx))))
       (range 9)))


(define (determinalize x)
  (cond ((equal? x Tx) '(1 0 0))
        ((equal? x Ty) '(0 0 1))
        ((equal? x Tt) '(0 1 0))
        (else x)))


(define (get-moves soln-space mark)
  (let ((shallow-soln-space (generate-soln-space soln-space
                                                 mark
                                                 try-move-shallow)))
    (if (is-terminal? (reduce (lambda (x y)
                                (if (is-terminal? x)
                                    x
                                    y))
                              shallow-soln-space))
        (map determinalize shallow-soln-space)
        (generate-soln-space soln-space mark try-move-deep))))


(define (calc-weight triplet)
  (if (= (caddr triplet) 0)
      (caddr triplet)
      (/ (car triplet)
         (caddr triplet))))

(define (pick-best-move my-moves)
  (let ((selected-moves '())
        (selected-weight 0))
    (for-each (lambda (idx)
                (let ((val (list-ref my-moves idx)))
                  (if (list? val)
                      (let ((weight (calc-weight val)))
                        (cond ((= weight selected-weight)
                               (begin (set! selected-weight weight)
                                      (set! selected-moves (append selected-moves
                                                                   (list idx)))))
                              ((> weight selected-weight)
                               (begin (set! selected-weight weight)
                                      (set! selected-moves (list idx)))))))))
              (range 9))
    (pick-random-move selected-moves)))

; strategy function (decision maker)
(define (my-best-next-move soln-space)
  (let* ((my-moves (get-moves soln-space 'x)))
    (pick-best-move my-moves)))


; get move from user during her turn. and do basic
; validy that the move number is within 0-8 and
; that the input number does not already contain
; a previous move
(define (her-best-next-move soln-space)
  (display "=====================\n")
  (ttt-show soln-space)
  (display "Enter index number for your move: ")
  (let ((her-move (string->number (read-line))))
    (if (or (not her-move)
	    (> her-move (- (length soln-space) 1))
	    (< her-move 0)
	    (list-ref soln-space her-move))
	(begin
	  (display "Invalid move, try again!\n")
	  (her-best-next-move soln-space))
	her-move)))


; the game loop
(define (ttt-solve soln-space)
  (let ((winner (ttt-solved? soln-space)))
    (if winner
	winner
	(begin
	  (if now-my-move
	      (list-set! soln-space (my-best-next-move soln-space) 'x)
	      (list-set! soln-space (her-best-next-move soln-space) 'y))
	  (set! now-my-move (not now-my-move))
	  (ttt-solve soln-space)))))


; kick start
(define (main args)
  (let ((verdict (ttt-solve initial-soln-space)))
    (cond ((equal? verdict #t)
	   (display "No winner"))
	  ((equal? verdict 'x)
	   (display "The computer won"))
	  ((equal? verdict 'y)
	   (display "You won!"))
	  (else
	   (display "I'm confused"))))
  (newline))

