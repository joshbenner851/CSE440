;Permutation Scheme Program
;Josh Benner
;Spring 2016
;AI Dekai

(define (grabFirst n)
	(if (equal? n '())
		(print "#f")
		(begin
			(print (car n))
			(grabFirst (cdr n))
		)
	)
)


(define (recurse s)
	(if (equal? s '())
			(print "#f")
			;(display "#f \n")
		(begin
			;(print (car s) "\n")
			(print (cons (car s) (recurseList (cdr s) '() )) ) 
			(recurse (cdr s))
		)
	)
)
;Recursive for loop
(define (recurseList a b)
	;we return when the first arg is an empty list
	(if (equal? a '())
			b ; base case, return the appended string
			;(display "#f \n")
		(begin
			;(display '(car a))
			(recurseList (cdr a) (append b (list(car a))) )
		)
	)
)

; (if (equal? (list? '(a)) #t)
; 	;do work
; )
; permute '() '() '() '(t e a)
; permute '() '(t) '(e a) '(e a)
; permute '() '(t) '(a e) '(a e)

; permute '(e) '(a) '() '()
; permute '(a) '(e) '() '()

(define (permute bf l af seq)
	(define before '())
	;(print "before: " before)
	(define letter (list(car seq)))
	;(print "letter: " letter)
	(define after (cdr seq))
	;(print "after: " after)
	(if (equal? af '())
		(print "#f")
	)

	;before + after = (append letter (append before after)
	(define result (append letter (append before after)))
	(print "result: " result)

	(print (append (append before letter) after))
	(if (equal? (length (car(cdr seq))) 1)
		(print "hi")
		(begin
			(permute (cdr seq))
		)
	)
)

(define (permuteAlt a)
	;Check if empty list 
	(if (equal? (length a) 0) #f)
	(if (equal? (length a) 1)
		a
	)
)

; Accepts two lists
;dict = dictionary we're using as all valid sequences
;a = sequence we're finding permutations of and checking if valid
(define (anagram dict a)
	;(print dict)
	;for loop through dictionary
	;a is anagram
	;b is word in dictionary



	;Check if it's a valid word
	;valid = if in dictionary
	(if (equal? dict '() )
		(print "checked all words")
		(begin
			; need to get the permutation and set as a			
			(if (equal? (list(car dict)) (permuteAlt a) )
				(print "valid: " a)
				(begin
					;(print "invalid: " a)
					(anagram (cdr dict) a)
				)
			) 
		)
	)
	; (print "#f")
	
)

; returns a list with the removed element
(define (remove l bef aft curr seq)
	(print "before: " bef)
	(print "after: " aft)
	(print "curr: " curr)
	(print "seq: " seq)
	(set! curr (list(car seq)))
	;if the sequence is empty we're finished
	(if (equal? seq '())
		(print "finished")
		(begin
			;initialize the after sequence
			(if (equal? aft '())
				(define aft (cdr seq))
				(print "declare after: " aft)
			)
			; if the letter is equal to current
			(if (equal? l curr) 
				;print the list w/o the element
				(print (append bef aft))
				(begin
					;l is constant, letter we're removing
					;bef adds the previous letter
					;aft loses it's first letter
					;curr shifts to the right 1 letter each iteration
					;seq moves 1 letter to the right
					(remove l (append curr bef) (cdr aft) curr (cdr seq) )
				)	
			)
		)
	)
)

(define (rewrite l bef aft seq)
	(cond 
		;if sequence is empty we're done
		( (equal? seq '()) (print "finished") )
		;initialize after to the seq w/o first letter
		( (equal? aft '()) 
			
				( define aft (cdr seq) )
				; (print "initialized after: ")
				; #f
		)
	)
	(cond
		;if sequence is empty we're done
		( (equal? seq '()) (print "finished") )
		( (equal? l (list(car seq))) 
			( 
				(print(append bef aft))
			) 
		)
		(else 
			(print "fml")
		) 
	)
	
)

(define (test a)
	(cond ((> 3 3) "greater")
	((< 3 3) "less")
	(else "equal")) 
)


; (define (delete item lst)
; 	(if (equal? (length lst) 0)
; 		(print "empty")
; 		(begin
; 			(if (equal? item (list(car lst)))
; 				(cdr list)
; 				(begin
; 					(cons (car lst))
; 					(deleteItem item (cdr list))
; 				)
; 			)
; 		)
; 	)

; )

; (define (deleteItem item list) 
;   (cond((empty? list) '())
;     ((equal? item (car list)) (cdr list))
;     (else (cons (car list) (deleteItem item (cdr list))))))


;(append (car dictionary) '()) '(a) )

; No clue how this is different then line 47
; (define (checkValid a b)
; 	(if (equal? a b)
; 		#t
; 		(begin
; 			#f
; 		)
; 	)
; )


; iterative approach
; recurse(s)
; for(i=0;i<s.length-1;i++){
; 	return s[i] + recurse[s,s.length-1]
; }
; 	