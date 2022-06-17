*******************************************************************************

* PROMISE
* NSC DATASET

* RECHECK DECISION RULES APPLICATION

*******************************************************************************

	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& r<r[_n+1]
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& r<r[_n-1]
	
	drop Sequence
	
	**** Check Sequence I
	by StudentID: egen Sequence = rank(EnrollmentBegin), track
	
	* Flag
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
