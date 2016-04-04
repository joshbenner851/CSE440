;Permutation Scheme Program
;Josh Benner
;Spring 2016
;AI Dekai

; returns a list with the removed element
(define (remove l bef aft curr seq)
	; (print "before: " bef)
	; (print "after: " aft)
	; (print "curr: " curr)
	; (print "seq: " seq)
	(set! curr (list(car seq)))
	;if the sequence is empty we're finished
	(if (equal? seq '())
		(print "finished")
		(begin
			;initialize the after sequence
			(if (equal? aft '())
				(define aft (cdr seq))
				; (print "declare after: " aft)
			)
			; if the letter is equal to current
			(if (equal? l curr) 
				;print the list w/o the element
				(append bef aft)
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

(define dictionary '(a act ale at ate cat eat etc tea))

;n = current sequence that's being modified as we go
;orig = very original sequence that stays constant
(define (grabFirst n orig)
	(if (equal? n '())
		(print "#f")
		(begin
			;;we need to recurse here on 
			;the sub strings to get all possibilites
			;ONLY NEED THIS FOR ANAGRAM NOT PERMUTE
			;;THIS WORKS BUT WE"RE NOT CHECKING ALL PERMUTATIONS YET SO UNCOMMENT IT ONCE
			;THAT IS DONE
			; (if (equal? #t (checkDict dictionary (append 
			; 			(list(car n)) 
			; 			(remove (list(car n)) '() '() '() orig )) 
			; 	))
			; 	(print "valid: " (append 
			; 			(list(car n)) 
			; 			(remove (list(car n)) '() '() '() orig )))
			; )
			;COMMENT THIS OUT ONCE DONE
			( print (append 
						(list(car n)) 
						(remove (list(car n)) '() '() '() orig ) 
					) 
			)
			(grabFirst (cdr n) orig)
		)
	)
)

; (define (forloop )

; )

(define (perm n orig)
	(if (equal? n '())
		#f
		(begin
			;;we need to recurse here on 
			;the sub strings to get all possibilites
			
			(perm (cdr n) orig)
			(append 
						(list(car n)) 
						(remove (list(car n)) '() '() '() orig ) 
					)
		)
	)
)
;hi

(define (permutation s)
	(grabFirst s s)
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

; (define (permuteAlt a)
; 	;Check if empty list 
; 	(if (equal? (length a) 0) #f)
; 	(if (equal? (length a) 1)
; 		a
; 	)
; )

;The input format differs from the dictionary so we need to convert it to the dict format
;initially rest = ""
;seq = the original sequence we need to convert from '(x y z) to '(xyz) 
(define (fixformat seq rest)
	(if (equal? seq '())
		(list(string->symbol rest))
		(fixformat (cdr seq) (string-append rest (symbol->string (car seq))  ))
	)
)

(define (checkDict dict word)
	(if (equal? dict '() )
		#f
		(begin
			; need to get the permutation and set as a			
			(if (equal? (list(car dict)) (fixformat word "")) ;)(permuteAlt 
				;(print "valid: " (permuteAlt word))
				#t
				(begin
					;(print (list(car dict)) (fixformat word ""))
					;(print "invalid: " a)
					;(print "cdr: " (cdr dict) " word: " word)
					(checkDict (cdr dict) word)
				)
			) 
		)
	)
)

; Accepts two lists
;dict = dictionary we're using as all valid sequences
;a = sequence we're finding permutations of and checking if valid
(define (anagram dict word)
	;(print dict)
	;for loop through dictionary
	;a is anagram
	;b is word in dictionary

	;Check if it's a valid word
	;valid = if in dictionary
	
	(if (equal? #t (checkDict dict word))
		(print word)
	)
	; (if (equal? dict '() )
	; 	(print "#f")

	; 	(begin
	; 		; need to get the permutation and set as a			
	; 		(if (equal? (list(car dict)) (permuteAlt a) )
	; 			(print "valid: " (permuteAlt a))
	; 			(begin
	; 				;(print "invalid: " a)
	; 				(anagram (cdr dict) a)
	; 			)
	; 		) 
	; 	)
	; )
	; (print "#f")
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

; (if (equal? act (fixformat '(a c t) ""))
; 	(print "#t")
; 	(print "#f")
; )


; (if (equal? #t (checkDict dictionary (append 
; 						(list(car '(t e a))) 
; 						(remove (list(car '(t e a))) '() '() '() '(t e a) )) 
; 				))
; 				(print "valid: " (append 
; 						(list(car '(t e a))) 
; 						(remove (list(car '(t e a))) '() '() '() '(t e a) )))
; 			)

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