#!/usr/bin/guile \
-e main -s 
!#

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
;;; appretiated
;;;

; to get line number on runtime exceptions
(use-modules (ice-9 debugger))

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

; remove member from the-list and return as a new list
(define (without the-list member)
  (define (without-inner the-list member half-ans)
    (cond ((equal? the-list '())
	   half-ans)
	  ((equal? (car the-list)
		 member)
	   (without-inner (cdr the-list) 
			  member 
			  half-ans))
	  (else
	   (without-inner (cdr the-list) 
			  member 
			  (append half-ans
				  (list (car the-list)))))))
    (without-inner the-list member (list)))

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
  (list-ref winning-moves (random (length winning-moves))))

; get the "other" mark. given 'x, return 'y and vice versa
(define (other mark)
  (if (equal? mark 'x)
      'y
      'x))

; strategy function (calculates parameters for decision making)
(define (get-moves soln-space my-mark)
  (let ((can-win-moves (list)) 
	(can-lose-moves (list))
	(can-tie-moves (list))
	(will-tie-moves (list))
	(will-win-moves (list))
	(will-lose-moves (list)))
    (for-each (lambda (idx)
		(if (not (list-ref soln-space idx))
		    (let ((temp-list (list-copy soln-space)))
		      (list-set! temp-list idx my-mark)
		      (let ((verdict (ttt-solved? temp-list)))
			(cond ((equal? verdict #t)
			       (set! will-tie-moves 
				     (append will-tie-moves 
					     (list idx))))
			      ((equal? verdict my-mark)
			       (set! will-win-moves
				     (append will-win-moves 
					     (list idx))))
			      (else
			       (let* ((enemy-moves 
				       (get-moves temp-list 
						  (other my-mark)))
				      (enemy-can-win-moves 
				       (list-ref enemy-moves 0))
				      (enemy-can-lose-moves 
				       (list-ref enemy-moves 1))
				      (enemy-can-tie-moves 
				       (list-ref enemy-moves 2))
				      (enemy-will-win-moves 
				       (list-ref enemy-moves 3))
				      (enemy-will-lose-moves 
				       (list-ref enemy-moves 4))
				      (enemy-will-tie-moves 
				       (list-ref enemy-moves 5))
				      (empty-spaces 
				       (length (filter (lambda (x)
							 (not x))
						       temp-list))))
				 ; if enemy has a 'will-win', then i will lose
				 (and (> (length enemy-will-win-moves) 0)
				      (set! will-lose-moves
					    (append will-lose-moves 
						    (list idx))))
				 ; if all enemy's moves make him 'will-lose', then i will win
				 (if (= empty-spaces 
					(length enemy-will-lose-moves))
				     (set! will-win-moves
					   (append will-win-moves
						   (list idx)))
				     ; if few of enemy's movies make him 'will-lose', then i can win
				     (and (> (length enemy-will-lose-moves) 0)
					  (set! can-win-moves
						(append can-win-moves 
							(list idx)))))
				 ; if enemy can win, i can lose
				 (and (> (length enemy-can-win-moves) 0)
				      (set! can-lose-moves
					    (append can-lose-moves
						    (list idx))))
				 ; if enemy has all 'will-tie', then i will tie
				 (if (= empty-spaces 
					(length enemy-will-tie-moves))
				     (set! will-tie-moves
					   (append will-tie-moves 
						   (list idx)))
				     ; if few of enemy's moves make him 'will-tie', then i can tie
				     (and (> (length enemy-will-tie-moves) 
					     0)
					  (set! can-tie-moves
						(append can-tie-moves
							(list idx))))))))))))
	      (range 9))
;    (for-each (lambda (lose-move)
;		(and (has win-moves lose-move)
;		     (set! win-moves (without win-moves lose-move)))
;		(and (has tie-moves lose-move)
;		     (set! tie-moves (without tie-moves lose-move))))
;	      lose-moves)
    (list can-win-moves 
	  can-lose-moves 
	  can-tie-moves 
	  will-win-moves 
	  will-lose-moves
	  will-tie-moves)))

; strategy function (decision maker)
(define (my-best-next-move soln-space)
  (let* ((my-moves (get-moves soln-space 'x))
	 (i-can-win-moves (list-ref my-moves 0))
	 (i-can-lose-moves (list-ref my-moves 1))
	 (i-can-tie-moves (list-ref my-moves 2))
	 (i-will-win-moves (list-ref my-moves 3))
	 (i-will-lose-moves (list-ref my-moves 4))
	 (i-will-tie-moves (list-ref my-moves 5))
	 (possible-moves (filter (lambda (x)
				   (not (list-ref soln-space x)))
				 (range 9)))
	 (final-move #f))

    (for-each (lambda (losing-move)
		(set! possible-moves
		      (without possible-moves losing-move))
		(set! i-will-win-moves
		      (without i-will-win-moves losing-move))
		(set! i-can-win-moves
		      (without i-can-win-moves losing-move))
		(set! i-will-tie-moves
		      (without i-will-tie-moves losing-move))
		(set! i-can-tie-moves
		      (without i-can-tie-moves losing-move)))
	      i-will-lose-moves)

;    for debugging
;    (for-each (lambda (a-list)
;		(display a-list) (newline))
;	      my-moves)


    (if (> (length i-will-win-moves)
	   0)
	(set! possible-moves i-will-win-moves)
	(if (> (length i-can-win-moves)
	       0)
	    (set! possible-moves i-can-win-moves)
	    (if (> (length i-will-tie-moves)
		   0)
		(set! possible-moves i-will-tie-moves)
		(if (> (length i-can-tie-moves)
		       0)
		    (set! possible-moves i-can-tie-moves)))))

    (set! final-move (pick-random-move possible-moves))
    (display "Computer chose: ")
    (display final-move) (newline)
    final-move))


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
	  (her-bext-next-move soln-space))
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

