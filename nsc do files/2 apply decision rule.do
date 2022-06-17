*******************************************************************************

* PROMISE
* NSC DATASET

* RUN DECISION RULE SCRIPT

*******************************************************************************
	
	clear
	
	foreach i of numlist 2005/2022 {
	
	use "3_Clean Data Files\NSC\cohort`i'.dta"
	do "2_Cleaning Do Files\NSC base cleaning\2a decision rule.do"
	sum EnrollmentSame
	
	if `r(mean)'>0 {
	do "2_Cleaning Do Files\NSC base cleaning\2b check.do"
	sum EnrollmentSame
	}
	
	* drop variables
	drop r EnrollmentSame Sequence
	
	* gen Sequence
	sort lrdc_id EnrollmentBegin
	by lrdc_id: egen EnrollmentSequence = rank(EnrollmentBegin), track
	
	* degree title
	do "2_Cleaning Do Files\NSC base cleaning\2c degree title.do"
	
	* Semester Classification
	do "2_Cleaning Do Files\NSC base cleaning\2d semester classification.do"
	
	save "3_Clean Data Files\NSC\clean`i'.dta", replace
	}
	
