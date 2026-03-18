*******************************************************************************

* PROMISE
* NSC DATASET

* DECISION RULES SCRIPT

*******************************************************************************
	

	
	* CREATE COLLEGE SEQUENCE

	bys StudentID (EnrollmentBegin): egen Sequence = rank(EnrollmentBegin), track
	
	* IF CURRENT ENROLLMENT PERIOD IS WITHIN THE LAST ENROLLMENT PERIOD
	sort StudentID
	
	by StudentID: 	replace Sequence = Sequence[_n-1]/*
	*/				if EnrollmentBegin >= EnrollmentBegin[_n-1]/*
	*/				& EnrollmentEnd <= EnrollmentEnd[_n-1]/*
	*/				& Sequence!=1
	
	* IF CURRENT ENROLLMENT PERIOD OVERLAPS THE LAST ENROLLMENT PERIOD
	sort StudentID EnrollmentBegin EnrollmentEnd 
	
	by StudentID: 	replace Sequence = Sequence[_n-1]/*
	*/ 				if EnrollmentBegin >= EnrollmentBegin[_n-1]/*
	*/ 				& EnrollmentBegin < EnrollmentEnd[_n-1]/*
	*/ 				& EnrollmentEnd >= EnrollmentEnd[_n-1]/*
	*/				& Sequence!=1
	
	*** SAME ENROLLMENT PERIOD
	sort StudentID
	
	* FULL-TIME STATUS
	gen byte FullTime=EnrollmentStatus==1
	
	* TWO ENROLLMENTS IN ANY TWO SEQUENCES
	gen EnrollmentSame = 0
	
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	
	* RULE #1: DROP IF WITHDRAWN FROM COLLEGE
	
	drop if EnrollmentStatus == 6
	
	* RULE #2: DROP IF THE OTHER ENROLLMENT IS IN A 2-YEAR INSTITUTION
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& CollegeType<CollegeType[_n+1]
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& CollegeType<CollegeType[_n-1]
	
	* RULE #3: DROP IF THE OTHER ENROLLMENT IS PART TIME
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& FullTime<FullTime[_n+1]
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& FullTime<FullTime[_n-1]
	
	* RULE #4: IF ALL EQUIVALENT, SELECT THE SAME INSTITUTIONS
	by StudentID:	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& CollegeName!=CollegeName[_n-1]
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& CollegeName!=CollegeName[_n+1]
	
	* RULE #5: SELECT AT RANDOM
	* regenerate enrollment ID
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	* generate random number
	gen r = runiform() if EnrollmentSame==1
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n+1]/*
	*/				& r<r[_n+1]
	
	by StudentID: 	drop if EnrollmentSame == 1/*
	*/				& Sequence == Sequence[_n-1]/*
	*/				& r<r[_n-1]
	
	drop Sequence
	
	* create a new sequence
	by StudentID: egen Sequence = rank(EnrollmentBegin), track
	
	* Flag
	drop EnrollmentSame
	
	gen EnrollmentSame = 0
	
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n-1] & Sequence!=.
	by StudentID: replace EnrollmentSame = 1 if Sequence == Sequence[_n+1] & Sequence!=.
	
	
	drop youruniqueidentifier r
		destring *cip*, replace


	br *cip*
		br enrollmentmajor1 enrollmentcip1 enrollmentmajor2 enrollmentcip2 degreetitle degreemajor1 degreecip1 degreemajor2 degreecip2 degreemajor3 degreecip3 degreemajor4 degreecip4 if enrollmentcip1 < 10000 | enrollmentcip1 > 600000

		
		*CURATE THESE, first pass with david and the rest with 
	
	
	replace enrollmentcip1 = . if enrollmentcip1 < 10000 | enrollmentcip1 > 600000
	replace enrollmentcip2 = . if enrollmentcip2 < 10000 | enrollmentcip2 > 600000
	replace degreecip1 = . if degreecip1 < 10000 | degreecip1 > 600000
	replace degreecip2 = . if degreecip2 < 10000 | degreecip2 > 600000
	replace degreecip3 = . if degreecip3 < 10000 | degreecip3 > 600000
	replace degreecip4 = . if degreecip4 < 10000 | degreecip4 > 600000

	keep enrollmentmajor1 enrollmentcip1 enrollmentmajor2 enrollmentcip2 degreetitle degreemajor1 degreecip1 degreemajor2 degreecip2 degreemajor3 degreecip3 degreemajor4 degreecip4
	
	