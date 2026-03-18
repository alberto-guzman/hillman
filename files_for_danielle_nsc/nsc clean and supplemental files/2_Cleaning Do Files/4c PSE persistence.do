clear

*Import file
	use "3_Clean Data Files\MEGA_enrollments_by semester.dta"

	
*simple enrollment variable
	gen enrolled_NSC=1
	drop if year==.
	
	reshape wide enrolled_NSC institution IPEDS_UNIT_ID ipeds_have degree_NSC DegreeAttainedSTEM in_state seq inst_sector_simple major_stem, i(firstn lastn) j(year_semester)
	recode enrolled_NSC* (.=0)
	save "3_Clean Data Files\MEGA_simple_enroll_NSC",replace
	
	clear 
	
*Import file
	use "3_Clean Data Files\MEGA_enrollment.dta"

	duplicates drop firstn lastn, force

*merge in enrollment
	
	merge 1:1 firstn lastn using "3_Clean Data Files\MEGA_simple_enroll_NSC.dta"
	drop _merge

*merge in degree
	rename firstn NSCfirstname
	rename lastn NSClastname
	merge 1:1 NSCfirstn NSClastn using "3_Clean Data Files\MEGA_degree_outcomes_NSC.dta"
	drop _merge
	

	rename NSC* *NSC 
	
*Persistence or Degree for NSC comparison	
	gen fall_enter_cohort=.
	foreach i of numlist 2017/2026 {
	local Y = `i'-1
	recode fall_enter (.=`Y') if first_term==`i'1
	}
	
	gen fall_enter_cohort_stem=.
	foreach i of numlist 2017/2026 {
	local Y = `i'-1
	recode fall_enter_cohort_stem (.=`Y') if first_term==`i'1 & major_stem==1
	}
	
	gen reten_fall_enter=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	capture noisily: recode reten_fall_enter (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1
	capture noisily: recode reten_fall_enter (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=.)
	capture noisily: recode reten_fall_enter (.=0) if first_term==`i'1 & institution`i'1~=institution`Y'1
	}
	
	gen reten_fall_enter_stem=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	capture noisily: recode reten_fall_enter_stem (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & major_stem`i'1==1 & major_stem`Y'1==1
	capture noisily: recode reten_fall_enter_stem (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=.)
	capture noisily: recode reten_fall_enter_stem (.=0) if first_term==`i'1 & institution`i'1~=institution`Y'1
	}
	
	
	gen reten_fall_enter2=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local Z = `i'+2
	capture noisily: recode reten_fall_enter2 (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & institution`i'1==institution`Z'1
	capture noisily: recode reten_fall_enter2 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=. | degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`Z'1~=.)
	capture noisily: recode reten_fall_enter2 (.=0) if first_term==`i'1 & (institution`i'1~=institution`Y'1 | institution`i'1~=institution`Z'1)
	}
	
	
	gen reten_fall_enterSTEM2=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local Z = `i'+2
	capture noisily: recode reten_fall_enterSTEM2 (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & institution`i'1==institution`Z'1  & major_stem`i'1==1 & major_stem`Y'1==1 & major_stem`Z'1==1
	capture noisily: recode reten_fall_enterSTEM2 (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=. | DegreeAttainedSTEM`Y'3~=. | DegreeAttainedSTEM`Y'4~=. | DegreeAttainedSTEM`Z'1~=.)
	capture noisily: recode reten_fall_enterSTEM2 (.=0) if first_term==`i'1 & (institution`i'1~=institution`Y'1 | institution`i'1~=institution`Z'1)
	}
	
	
	
	
	gen reten_fall_enter3=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local Z = `i'+2
	local X = `i'+3
	capture noisily: recode reten_fall_enter3 (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & institution`i'1==institution`Z'1 & institution`i'1==institution`X'1
	capture noisily: recode reten_fall_enter3 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=. | degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`Z'1~=. | degree_NSC`Z'3~=.|degree_NSC`Z'4~=.|degree_NSC`X'1~=.)
	capture noisily: recode reten_fall_enter3 (.=0) if first_term==`i'1 & (institution`i'1~=institution`Y'1 | institution`i'1~=institution`Z'1 | institution`i'1~=institution`X'1)
	}
	
	
	gen reten_fall_enterSTEM3=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local Z = `i'+2
	local X = `i'+3
	capture noisily: recode reten_fall_enterSTEM3 (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & institution`i'1==institution`Z'1 & institution`i'1==institution`X'1 & major_stem`i'1==1 & major_stem`Y'1==1 & major_stem`Z'1==1 & major_stem`X'1==1
	capture noisily: recode reten_fall_enterSTEM3 (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=. | DegreeAttainedSTEM`Y'3~=. | DegreeAttainedSTEM`Y'4~=. | DegreeAttainedSTEM`Z'1~=. | DegreeAttainedSTEM`Z'3~=.|DegreeAttainedSTEM`Z'4~=.|DegreeAttainedSTEM`X'1~=.)
	capture noisily: recode reten_fall_enterSTEM3 (.=0) if first_term==`i'1 & (institution`i'1~=institution`Y'1 | institution`i'1~=institution`Z'1 | institution`i'1~=institution`X'1)
	}
	
	
	
	gen reten_fall_enter4=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local Z = `i'+2
	local X = `i'+3
	local Q = `i'+4
	capture noisily: recode reten_fall_enter4 (.=1) if first_term==`i'1 & institution`i'1==institution`Y'1 & institution`i'1==institution`Z'1 & institution`i'1==institution`X'1 & institution`i'1==institution`Q'1
	capture noisily: recode reten_fall_enter4 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=. | degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`Z'1~=. | degree_NSC`Z'3~=.|degree_NSC`Z'4~=.|degree_NSC`X'1~=. | degree_NSC`X'3~=.|degree_NSC`X'4~=.|degree_NSC`Q'1~=.)
	capture noisily: recode reten_fall_enter4 (.=0) if first_term==`i'1 & (institution`i'1~=institution`Y'1 | institution`i'1~=institution`Z'1 | institution`i'1~=institution`X'1 | institution`i'1~=institution`Q'1)
	}
	
	gen pers_fall_enter=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	capture noisily: recode pers_fall_enter (.=1) if first_term==`i'1 & enrolled_NSC`Y'1==1
	capture noisily: recode pers_fall_enter (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=.)
	capture noisily: recode pers_fall_enter (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0
	}
	
	gen pers_fall_enterSTEM=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	capture noisily: recode pers_fall_enterSTEM (.=1) if first_term==`i'1 & major_stem`Y'1==1
	capture noisily: recode pers_fall_enterSTEM (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=.)
	capture noisily: recode pers_fall_enterSTEM (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0
	}
	
	gen pers_fall_enter2=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local X = `i'+2
	capture noisily: recode pers_fall_enter2 (.=1) if first_term==`i'1 & enrolled_NSC`Y'1==1 & enrolled_NSC`X'1==1
	capture noisily: recode pers_fall_enter2 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=.| degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`X'1~=.)
	capture noisily: recode pers_fall_enter2 (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0 & enrolled_NSC`X'1==0
	}
	
	
	gen pers_fall_enterSTEM2=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local X = `i'+2
	capture noisily: recode pers_fall_enterSTEM2 (.=1) if first_term==`i'1 & major_stem`Y'1==1 & major_stem`X'1==1 
	capture noisily: recode pers_fall_enterSTEM2 (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=.| DegreeAttainedSTEM`Y'3~=. | DegreeAttainedSTEM`Y'4~=. | DegreeAttainedSTEM`X'1~=.)
	capture noisily: recode pers_fall_enterSTEM2 (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0 & enrolled_NSC`X'1==0
	}
	
	
	gen pers_fall_enter3=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local X = `i'+2
	local Z = `i'+3
	capture noisily: recode pers_fall_enter3 (.=1) if first_term==`i'1 & enrolled_NSC`Y'1==1 & enrolled_NSC`X'1==1 & enrolled_NSC`Z'1==1
	capture noisily: recode pers_fall_enter3 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=.| degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`X'1~=. |degree_NSC`X'3~=.|degree_NSC`X'4~=.| degree_NSC`Z'1~=.)
	capture noisily: recode pers_fall_enter3 (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0 & enrolled_NSC`X'1==0 & enrolled_NSC`Z'1==0
	}
	
	
	gen pers_fall_enterSTEM3=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local X = `i'+2
	local Z = `i'+3
	capture noisily: recode pers_fall_enter3 (.=1) if first_term==`i'1 & major_stem`Y'1==1 & major_stem`X'1==1 & major_stem`Z'1==1
	capture noisily: recode pers_fall_enter3 (.=1) if first_term==`i'1 & (DegreeAttainedSTEM`i'3~=.|DegreeAttainedSTEM`i'4~=.|DegreeAttainedSTEM`Y'1~=.| DegreeAttainedSTEM`Y'3~=. | DegreeAttainedSTEM`Y'4~=. | DegreeAttainedSTEM`X'1~=. |DegreeAttainedSTEM`X'3~=.|DegreeAttainedSTEM`X'4~=.| DegreeAttainedSTEM`Z'1~=.)
	capture noisily: recode pers_fall_enter3 (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0 & enrolled_NSC`X'1==0 & enrolled_NSC`Z'1==0
	}
	
	
	
	
	gen pers_fall_enter4=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	local X = `i'+2
	local Z = `i'+3
	local Q = `i'+4
	capture noisily: recode pers_fall_enter4 (.=1) if first_term==`i'1 & enrolled_NSC`Y'1==1 & enrolled_NSC`X'1==1 & enrolled_NSC`Z'1==1 & enrolled_NSC`Q'1==1
	capture noisily: recode pers_fall_enter4 (.=1) if first_term==`i'1 & (degree_NSC`i'3~=.|degree_NSC`i'4~=.|degree_NSC`Y'1~=.| degree_NSC`Y'3~=. | degree_NSC`Y'4~=. | degree_NSC`X'1~=. |degree_NSC`X'3~=.|degree_NSC`X'4~=.| degree_NSC`Z'1~=. |degree_NSC`Z'3~=.|degree_NSC`Z'4~=.| degree_NSC`Q'1~=.)
	capture noisily: recode pers_fall_enter4 (.=0) if first_term==`i'1 & enrolled_NSC`Y'1==0 & enrolled_NSC`X'1==0 & enrolled_NSC`Z'1==0 & enrolled_NSC`Q'1==0
	}
	
	gen pers_cohort=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+2
	local B = `i'+1
	capture noisily: recode pers_cohort (.=1) if cohort==`i' & enrolled_NSC`Y'1==1
	capture noisily: recode pers_cohort (.=1) if cohort==`i' & (degree_NSC`B'3~=.|degree_NSC`B'4~=.|degree_NSC`Y'1~=.)
	capture noisily: recode pers_cohort (.=0) if cohort==`i' & enrolled_NSC`Y'1==0
	}
	
	
	gen pers_cohortSTEM=.
	foreach i of numlist 2017/2026 {
	local Y = `i'+2
	local B = `i'+1
	capture noisily: recode pers_cohort (.=1) if cohort==`i' & major_stem`Y'1==1
	capture noisily: recode pers_cohort (.=1) if cohort==`i' & (DegreeAttainedSTEM`B'3~=.|DegreeAttainedSTEM`B'4~=.|DegreeAttainedSTEM`Y'1~=.)
	capture noisily: recode pers_cohort (.=0) if cohort==`i' & enrolled_NSC`Y'1==0
	}
	
**Any Degree in 6years after first fall enrollment
**Fall 2006
	gen degree_NSCin6_firstfall=.
	foreach i of numlist 2017/2026 {
	local first = `i'+1
	local second = `i'+2
	local third = `i'+3
	local fourth = `i'+4
	local fifth =`i'+5
	local sixth = `i'+6
	foreach x of numlist 1 3 4 {
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`first'`x'~=.
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`second'`x'~=.
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`third'`x'~=.
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`fourth'`x'~=.
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`fifth'`x'~=.
	capture noisily: recode degree_NSCin6_firstfall (.=1) if fall_e==`i' & degree_NSC`sixth'`x'~=.
	}
	}

	
	gen degree_in6_grad=.
	foreach i of numlist 2017/2026 {
	local first = `i'+1
	local second = `i'+2
	local third = `i'+3
	local fourth = `i'+4
	local fifth =`i'+5
	local sixth = `i'+6
	foreach x of numlist 1 3 4 {
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`first'`x'~=.
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`second'`x'~=.
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`third'`x'~=.
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`fourth'`x'~=.
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`fifth'`x'~=.
	capture noisily: recode degree_in6_grad (.=1) if hs_grad_year==`i' & degree_NSC`sixth'`x'~=.
	}
	}

	
	
	gen STEMdegree_in6_grad=.
	foreach i of numlist 2017/2026 {
	local first = `i'+1
	local second = `i'+2
	local third = `i'+3
	local fourth = `i'+4
	local fifth =`i'+5
	local sixth = `i'+6
	foreach x of numlist 1 3 4 {
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`first'`x'~=.
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`second'`x'~=.
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`third'`x'~=.
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`fourth'`x'~=.
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`fifth'`x'~=.
	capture noisily: recode STEMdegree_in6_grad (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`sixth'`x'~=.
	}
	}
	
	gen degree_in6_cohort=.
	foreach i of numlist 2017/2026 {
	local first = `i'+1
	local second = `i'+2
	local third = `i'+3
	local fourth = `i'+4
	local fifth =`i'+5
	local sixth = `i'+6
	foreach x of numlist 1 3 4 {
	capture noisily:  recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`first'`x'~=.
	capture noisily:  recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`second'`x'~=.
	capture noisily: recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`third'`x'~=.
	capture noisily: recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`fourth'`x'~=.
	capture noisily: recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`fifth'`x'~=.
	capture noisily: recode degree_in6_cohort (.=1) if cohort==`i' & degree_NSC`sixth'`x'~=.
	}
	}


	****enrolled 6 years later
	gen enrolled_NSCin6_cohort=.
	foreach i of numlist 2017/2026 {
	local sixth = `i'+6
	foreach x of numlist 1 3 4 {
	capture noisily: recode enrolled_NSCin6_cohort (.=1) if cohort==`i' & enrolled_NSC`sixth'`x'==1
	}
	}
	
	

	drop enrolled_NSC2* degree_NSC2*

	

*Save
	save "3_Clean Data Files\MEGA_alloutcomes.dta",replace