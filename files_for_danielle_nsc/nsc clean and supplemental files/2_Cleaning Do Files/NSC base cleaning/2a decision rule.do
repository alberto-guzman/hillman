*******************************************************************************


* NSC DATASET

* DECISION RULES SCRIPT

*******************************************************************************
	
	keep first last CollegeName ///
	degreetitle CollegeSequence RecordFound CollegeCodeBranch ///
	CollegeType CollegePublic EnrollmentBegin EnrollmentEnd EnrollmentStatus ///
	Graduated GraduationDate State GraduationYear GraduationMon hs_grad_year enrollmentmajor1 enrollmentcip1 enrollmentmajor2 enrollmentcip2 degreemajor1 degreecip1 degreemajor2 degreecip2
	
	
	* CREATE COLLEGE SEQUENCE

	bys first last (EnrollmentBegin): egen Sequence = rank(EnrollmentBegin), track
	
	* IF CURRENT ENROLLMENT PERIOD IS WITHIN THE LAST ENROLLMENT PERIOD
	sort first last
	
	by first last: 	replace Sequence = Sequence[_n-1]/*
	*/				if EnrollmentBegin >= EnrollmentBegin[_n-1]/*
	*/				& EnrollmentEnd <= EnrollmentEnd[_n-1]/*
	*/				& Sequence!=1
	
	* IF CURRENT ENROLLMENT PERIOD OVERLAPS THE LAST ENROLLMENT PERIOD
	sort first last EnrollmentBegin EnrollmentEnd 
	
	by first last: 	replace Sequence = Sequence[_n-1]/*
	*/ 				if EnrollmentBegin >= EnrollmentBegin[_n-1]/*
	*/ 				& EnrollmentBegin < EnrollmentEnd[_n-1]/*
	*/ 				& EnrollmentEnd >= EnrollmentEnd[_n-1]/*
	*/				& Sequence!=1
	
	*** SAME ENROLLMENT PERIOD
	sort first last
	
	* FULL-TIME STATUS
	gen byte FullTime=EnrollmentStatus==1
	
	* TWO ENROLLMENTS IN ANY TWO SEQUENCES
	gen EnrollmentSame = 0
	
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	
	* RULE #1: DROP IF WITHDRAWN FROM COLLEGE
	
	drop if EnrollmentStatus == 6
	
	* RULE #2: DROP IF THE OTHER ENROLLMENT IS IN A 2-YEAR INSTITUTION
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& CollegeType<CollegeType[_n+1]
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& CollegeType<CollegeType[_n-1]
	
	* RULE #3: DROP IF THE OTHER ENROLLMENT IS PART TIME
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& FullTime<FullTime[_n+1]
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& FullTime<FullTime[_n-1]
	
	* RULE #4: IF ALL EQUIVALENT, SELECT THE SAME INSTITUTIONS
	by first last:	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& CollegeName!=CollegeName[_n-1]
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& CollegeName!=CollegeName[_n+1]
	
	* RULE #5: SELECT AT RANDOM
	* regenerate enrollment ID
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	* generate random number
	gen r = runiform() if EnrollmentSame==1
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& r<r[_n+1]
	
	by first last: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& r<r[_n-1]
	
	drop Sequence
	
	* create a new sequence
	by first last: egen Sequence = rank(EnrollmentBegin), track
	
	* Flag
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by first last: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	
	
