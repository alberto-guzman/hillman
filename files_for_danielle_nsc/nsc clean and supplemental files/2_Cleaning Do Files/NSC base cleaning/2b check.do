*******************************************************************************

* NSC DATASET

* RECHECK DECISION RULES APPLICATION

*******************************************************************************

	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& r<r[_n+1]
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& r<r[_n-1]
	
	drop Sequence
	
	**** Check Sequence I
	by first last: egen Sequence = rank(EnrollmentBegin), track
	
	* Flag
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
