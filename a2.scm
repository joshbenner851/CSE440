(define (remove l bef aft curr seq)
	(print "before: " bef)
	(print "after: " aft)
	(print "curr: " curr)
	(print "seq: " seq)
	(set! curr (list(car seq)))
	;if the sequence is empty we're finished
	(if (equal? seq '())
		( 
			(print "finished")
			(print (append bef aft))
		)
		(begin
			;initialize the after sequence
			(if (equal? aft '())
				(define aft (cdr seq))
				(print "declare after: " aft)
			)
				;l is constant, letter we're removing
				;bef adds the previous letter
				;aft loses it's first letter
				;curr shifts to the right 1 letter each iteration
				;seq moves 1 letter to the right
				(remove l (append curr bef) (cdr aft) curr (cdr seq) )
		)
	)
)