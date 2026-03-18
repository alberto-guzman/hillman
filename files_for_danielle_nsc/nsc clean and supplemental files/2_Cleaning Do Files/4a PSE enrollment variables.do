
clear
set more off

*Import file
	use "3_Clean Data Files\MEGA_LONG.dta"

	
	bys first last (year_sem): gen seq_NSC=_n 
	*using carryforward and need to go backwards
	gen negyear_semester = -year_semester


	
	drop if year_sem==.
	sort firstn lastn year_sem
	
*****dummy for DE / getting rid of dual enrollment records
tostring year_semester, gen(year_string)
replace year_string = substr(year_string, 1, 4)
destring year_string, replace
	gen dual_enrollment = 1 if year_string<=hs_grad_year
drop if year_string<=hs_grad_year
drop year_string
	
*first term
	bys firstn lastn (year): egen first_term_NSC = min(year) 
	bys firstn lastn (year): carryforward first_term_NSC, replace
	bys firstn lastn (negyear): carryforward first_term_NSC, replace
	
*DE
	bysort firstn lastn (year): carryforward dual_enrollment, replace
	bysort firstn lastn (negyear): carryforward dual_enrollment, replace
	
	
	*simple sector NSC
	gen inst_sector_simpleNSC=.
	replace inst_sector_simpleNSC=3 if inst_sector==1 
	replace inst_sector_simpleNSC=4 if inst_sector==2 
	replace inst_sector_simpleNSC=4 if inst_sector==3 
	replace inst_sector_simpleNSC=1 if inst_sector==4 
	replace inst_sector_simpleNSC=2 if inst_sector==5 
	replace inst_sector_simpleNSC=2 if inst_sector==6 
	replace inst_sector_simpleNSC=5 if inst_sector==7 
	replace inst_sector_simpleNSC=5 if inst_sector==8 
	replace inst_sector_simpleNSC=5 if inst_sector==9 
	bysort firstn lastn (year): carryforward inst_sector_simpleNSC, replace
	bysort firstn lastn (negyear): carryforward inst_sector_simpleNSC, replace
	la val inst_sector_simpleNSC simpledegreelevel
	
	

*initial college
	gen institution_initial_NSC=institution if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward institution_initial_NSC, replace
	bysort firstn lastn (negyear): carryforward institution_initial_NSC, replace
	
*initial college in-state
	gen instate_initial_NSC=in_state if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward instate_initial_NSC, replace
	bysort firstn lastn (negyear): carryforward instate_initial_NSC, replace
	
	
*initial college degree level
	gen inst_sector_initial_NSC=inst_sector if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward inst_sector_initial_NSC, replace
	bysort firstn lastn (negyear): carryforward inst_sector_initial_NSC, replace
	
	la val inst_sector_initial_NSC label_sector
	
	
*initial college degree level simple
	gen inst_sectorsimple_initial_NSC=inst_sector_simpleNSC if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward inst_sectorsimple_initial_NSC, replace
	bysort firstn lastn (negyear): carryforward inst_sectorsimple_initial_NSC, replace

	la val inst_sectorsimple_initial_NSC simpledegreelevel

*initial enrollment status
	gen enrollmentstatus_initial=EnrollmentStatus if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward enrollmentstatus_initial, replace
	bysort firstn lastn (negyear): carryforward enrollmentstatus_initial, replace


*initial IPEDS
	gen IPEDS_initial_NSC=IPEDS_UNIT_ID if year_sem==first_term_NSC
	bysort firstn lastn (year): carryforward IPEDS_initial_NSC, replace
	bysort firstn lastn (negyear): carryforward IPEDS_initial_NSC, replace

*initial degree
	gen first_major = enrollmentmajor1 if year_sem==first_term_NSC
	
	gen major_stem = 0
		rename enrollmentcip1 cip
		merge m:1 cip using "3_Clean Data Files\cip_stem.dta"
			replace major_stem = 1 if group=="STEM" 
			drop cip 
			drop if _merge==2
			drop _merge 
		rename enrollmentcip2 cip 
		merge m:1 cip using "3_Clean Data Files\cip_stem.dta"
			replace major_stem = 1 if group=="STEM" 
			drop cip 
			drop if _merge==2
			drop _merge
			
			gen first_major_stem = 0
			replace first_major_stem = 1 if major_stem==1 & year_sem==first_term_NSC
			

	bysort firstn lastn (year): carryforward first_major_stem, replace
	bysort firstn lastn (negyear): carryforward first_major_stem, replace
	bysort firstn lastn (year): carryforward first_major, replace
	bysort firstn lastn (negyear): carryforward first_major, replace	
	
*last term
	*NSC
	bys firstn lastn (year_seme): gen year_sem_baNSC = year_seme if degree_NSC==4 
	bys firstn lastn (year_sem_baNSC): replace year_sem_baNSC = . if year_sem_baNSC[1]~= year_sem_baNSC[_n] 

	bys firstn lastn (year_seme): carryforward year_sem_baNSC, replace
	bys firstn lastn (negyear_sem): carryforward year_sem_baNSC, replace
	
	bys firstn lastn (year_seme): gen in_undergradNSC = 1 if year_seme<=year_sem_baNSC & year_sem_baNSC~=.
	bys firstn lastn (year_seme): replace in_undergradNSC = 1 if year_sem_baNSC==.
	

	bys firstn lastn (year_seme): gen post_gradNSC = 1 if year_seme>year_sem_baNSC & in_undergradNSC~=1
	
	
		
*last term
	bys firstn lastn (year_seme): egen last_term_NSC = max(year_seme) if in_undergrad==1

*last college
	gen institution_last_NSC=institution if year_seme==last_term_NSC
	bys firstn lastn (negyear_sem): carryforward institution_last_NSC, replace back
		bys firstn lastn (year_seme): carryforward institution_last_NSC, replace 
	
	
*last college in-state
	gen instate_last_NSC=in_state if year_seme==last_term_NSC
	bysort firstn lastn (year_seme): carryforward instate_last_NSC, replace
	bysort firstn lastn (negyear): carryforward instate_last_NSC, replace
	
	
*last college degree level
	gen inst_sector_last_NSC=inst_sector if year_seme==last_term_NSC
	bysort firstn lastn (year_seme): carryforward inst_sector_last_NSC, replace
	bysort firstn lastn (negyear): carryforward inst_sector_last_NSC, replace

	la val inst_sector_last_NSC label_sector
	
*last college degree level simple
	gen inst_sectorsimple_last_NSC=inst_sector_simpleNSC if year_seme==last_term_NSC
	bysort firstn lastn (year_seme): carryforward inst_sectorsimple_last_NSC, replace
	bysort firstn lastn (negyear): carryforward inst_sectorsimple_last_NSC, replace

	la val inst_sectorsimple_last_NSC simpledegreelevel	

*last IPEDS
	gen IPEDS_last_NSC=IPEDS_UNIT_ID if year_seme==last_term_NSC
	bysort firstn lastn (year_seme): carryforward IPEDS_last_NSC, replace
	bysort firstn lastn (negyear): carryforward IPEDS_last_NSC, replace
	
*first and last post graduate school and term
	egen first_postterm = min(year_seme) if post_grad==1, by(firstn lastn)
	egen last_postterm = max(year_seme) if post_grad==1, by(firstn lastn)
	
	gen institution_postfirst=institution if year_seme==first_postterm
	bys firstn lastn (year_seme): carryforward institution_postfirst, replace
	bys firstn lastn (negyear_seme): carryforward institution_postfirst, replace
	gen institution_postlast=institution if year_seme==last_postterm
	bys firstn lastn (negyear_sem): carryforward institution_postlast, replace back
	bys firstn lastn (year_seme): carryforward institution_postlast, replace back
	
	
*summer
	tostring year_semester, gen(term_string)
	gen summer=substr(term, 5,5)
	destring summer, replace
	replace summer=0 if summer~=4
	replace summer=1 if summer==4	



*Enrolled ever
	gen enrolled_ever_NSC = 1 
	bys firstn lastn (year_seme): carryforward enrolled_ever_NSC, replace
	bys firstn lastn (negyear_seme): carryforward enrolled_ever_NSC, back replace
	
	gen enrolled_ever_stem = 1 if major_stem==1
	bys firstn lastn (year_seme): carryforward enrolled_ever_stem, replace
	bys firstn lastn (negyear_seme): carryforward enrolled_ever_stem, back replace	
	

*bring in query indicator

	merge m:1 firstn lastn using "3_Clean Data Files\NSC record.dta"
	drop _merge


	
preserve	
*by semester enrollments NO outcomes
	keep firstn lastn year_semester IPEDS_UNIT  ///
	 ipeds_have institution  inst_sector_simple degree_NSC DegreeAttainedSTEM in_state seq* major_stem

	save "3_Clean Data Files\MEGA_enrollments_by semester.dta", replace	
restore
	
*Create transfer variable*
	gen transferNSC=.
	bys firstn lastn (year_seme): replace transferNSC=1 if IPEDS_UNIT_ID[_n]~=IPEDS_UNIT_ID[_n-1] & [_n]~=[1] & in_undergradNSC==1 & summer==0

	bys firstn lastn: egen number_transfersNSC = total(transferNSC), missing
	
	bys firstn lastn transferNSC (year_seme): gen transfer_seqNSC = _n if transferNSC==1
	replace transferNSC=. if transfer_seqNSC~=1
	
*Transfer directionality*
	la de transferdirection 1 "2Y to 2Y" 2 "2Y to 4Y" 3 "4Y to 4Y" 4 "4Y to 2Y"
	
	gen transfer_directionNSC=.
	bys firstn lastn (year_seme): replace transfer_directionNSC=1 if inst_sector_simpleNSC[_n-1]<=2 & transferNSC[_n]==1 & inst_sector_simpleNSC[_n]<=2
	bys firstn lastn (year_seme): replace transfer_directionNSC=2 if inst_sector_simpleNSC[_n-1]<=2 & transferNSC[_n]==1 & inst_sector_simpleNSC[_n]>=3
	bys firstn lastn (year_seme): replace transfer_directionNSC=3 if inst_sector_simpleNSC[_n-1]>=3 & transferNSC[_n]==1 & inst_sector_simpleNSC[_n]>=3
	bys firstn lastn (year_seme): replace transfer_directionNSC=4 if inst_sector_simpleNSC[_n-1]>=3 & transferNSC[_n]==1 & inst_sector_simpleNSC[_n]<=2

	la val transfer_directionNSC transferdirection

	
	bys firstn lastn (year_seme): carryforward transferNSC, replace
	bys firstn lastn (negyear_seme): carryforward transferNSC, replace

	
	bys firstn lastn (year_seme): carryforward number_transfersNSC, replace
	bys firstn lastn (negyear_seme): carryforward number_transfersNSC, replace
	
	
	bys firstn lastn (year_seme): carryforward transfer_directionNSC, replace
	bys firstn lastn (negyear_seme): carryforward transfer_directionNSC, replace
	

	
	
	
*isid
	drop GraduationYear IPEDS_UNIT_ID	
	drop year_sem_ba negyear_sem in_undergrad post_grad 
	drop seq* inst_sector_simple 
	drop institution_initial ipeds_have institution inst_sector in_state 
	duplicates drop
	drop if firstn==""
	drop if lastn==""

	
	
**Creating Cohorts**
	gen cohort_NSC=.
	
	foreach i of numlist 2017/2026 {
	local Y = `i'+1
	replace cohort_NSC = `i' if first_term_NSC==`i'3|first_term_NSC==`i'4|first_term_NSC==`Y'1
	}
	
	


*Delayed enrollment.*

	la def delay 0 "Seamless enrollment" 1 "Enrollment within 5 years" 2 "Enrollment greater than 5 years"


	
	gen delayed_enrollment_NSC=.
	
		foreach i of numlist 2017/2026 {
		local Y = `i'+1
		local X = `i'+5
		replace delayed_enrollment_NSC = 0 if first_term_NSC <=`Y'6 & hs_grad_year==`i'
		replace delayed_enrollment_NSC = 1 if delayed_enrollment_NSC~=0 & first_term_NSC <=`X'6 & hs_grad_year==`i'
		replace delayed_enrollment_NSC = 2 if delayed_enrollment_NSC~=0 & first_term_NSC>`X'6 & hs_grad_year==`i' & first_term_NSC~=.
	}

	
	replace delayed_enrollment_NSC=. if first_term_NSC==.
	
	la val delayed_enrollment_N delay

	
*Delayed enrollment STEM*
	
	gen delayed_enrollment_STEM=.
	
		foreach i of numlist 2017/2026 {
		local Y = `i'+1
		local X = `i'+5
		replace delayed_enrollment_STEM = 0 if first_term_NSC <=`Y'6 & hs_grad_year==`i' & major_stem==1
		replace delayed_enrollment_STEM = 1 if delayed_enrollment_STEM~=0 & first_term_NSC <=`X'6 & hs_grad_year==`i' & major_stem==1
		replace delayed_enrollment_STEM = 2 if delayed_enrollment_STEM~=0 & first_term_NSC>`X'6 & hs_grad_year==`i' & first_term_NSC~=.
	}

	
	replace delayed_enrollment_STEM=. if first_term_NSC==.
	
	la val delayed_enrollment_STEM delay

	
**Delayed detailed**
	la de delayed_detail 2 "Delayed 1 year" 3 "Delayed more than 1 year" 4 "Seamless enrollment", modify


*****delayed enrollment NSC	
	gen delayed_detail_NSC=.
	replace delayed_detail_NSC=4 if delayed_enrollment_NSC==0

	foreach i of numlist 2017/2026 {
		local X = `i'+2
		replace delayed_detail_NSC=2 if hs_grad_year==`i' & first_term_NSC<=`X'4 & delayed_detail_NSC==.
	}
	
	replace delayed_detail_NSC=3 if delayed_detail_NSC==. & first_term_NSC~=.

	la val delayed_detail_N delayed_detail

	
	
	
*PSE enrollment w/in 2 years

	la de pse_enroll 1 "Enrolled w/i 2y" 0 "NOT enrolled w/i 2y" 

*PSE enrollment w/in 2 years
	gen pse_enrolled_2_NSC=.

	foreach i of numlist 2017/2026 {
		local X = `i'+2
	replace pse_enrolled_2_NSC=1 if hs_grad_year==`i' & first_term_N<=`X'4
	recode pse_enrolled_2_N (.=0) if hs_grad_year==`i' & enrolled_ever_NSC==1
	}
	
	la val pse_enrolled_2_NSC pse_enroll
	la var pse_enrolled_2_NSC "Enrollment in PSE w/in 2years after HS graduation"

	
	
	
	
*PSE enrollment w/in 6 years

	la de pse_enroll6 1 "Enrolled w/i 6y" 0 "NOT enrolled w/i 6y" 


*PSE enrollment w/in 6 years
	gen pse_enrolled_6_NSC=0

	foreach i of numlist 2017/2026 {
		local C = `i'+6
	replace pse_enrolled_6_NSC=1 if hs_grad_year==`i' & first_term_NSC<=`C'4
		}	

	
	la val pse_enrolled_6_NSC pse_enroll6
	la var pse_enrolled_6_NSC "Enrollment in PSE w/in 6years after HS graduation"	
	
	
	
***NSC 

*PSE enrollment w/in 8 years
	gen pse_enrolled_8_NSC=0

	foreach i of numlist 2017/2026 {
		local E = `i'+8
	replace pse_enrolled_8_NSC=1 if hs_grad_year==`i' & first_term_NSC<=`E'4
		}	

	la de pse_enroll8 1 "Enrolled w/i 8y" 0 "NOT enrolled w/i 8y" 
	la val pse_enrolled_8_NSC pse_enroll8
	la var pse_enrolled_8_NSC "Enrollment in PSE w/in 8years after HS graduation"	
	
	
	drop transfer_seq*
	drop first_postterm
	drop last_postterm

	
*Save
	save "3_Clean Data Files\MEGA_enrollment.dta", replace
	




