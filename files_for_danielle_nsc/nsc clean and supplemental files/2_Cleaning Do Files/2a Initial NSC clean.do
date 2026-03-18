*****************************************
* Hillman data
*****************************************


clear
set more off


	***grad years
	import delimited "1_Raw Data Files\hillman_grad_years.csv"
	
	*trimming
	replace first = ustrtrim(first)
	replace last = ustrtrim(last)
	replace first = subinstr(first, "-", "",.)
	replace last = subinstr(last, "-", "",.)
	replace first = subinstr(first, " ", "",.)
	replace last = subinstr(last, " ", "",.)
	replace first = subinstr(first, "'", "",.)
	replace last = subinstr(last, "'", "",.)
	replace last = regexr(last_name, "\(grade\d+\)", "")
	
	
	*drop NA
	drop if hs_grad == "NA"
	drop grade year
	
	duplicates drop
	
	sort first last hs_gra 
	duplicates tag first last, gen(dup)

	
	by first last (hs_grad): drop if dup==1 & hs_grad[_n]==hs_grad[1]
	
	isid first last
	
	rename first_name firstname
	rename last_name lastname

	destring hs_grad_year, replace
	drop dup
	

	
	save "3_Clean Data Files\hs_grad.dta", replace
	

	
	*import CSV
	import delimited "1_Raw Data Files\hillman_nsc.csv", clear
	
	**get rid of empty variables
	drop youruniq middlein
	

	duplicates drop 
	replace first = strlower(first)
	replace last = strlower(last)
	replace first = ustrtrim(first)
	replace last = ustrtrim(last)
	replace first = subinstr(first, "-", "",.)
	replace last = subinstr(last, "-", "",.)
	replace first = subinstr(first, " ", "",.)
	replace last = subinstr(last, " ", "",.)
	replace first = subinstr(first, "'", "",.)
	replace last = subinstr(last, "'", "",.)
	
	
	merge m:1 first last using "3_Clean Data Files\hs_grad.dta"
	drop if _merge==2
	drop _merge
	
	save "3_Clean Data Files\NSC_raw.dta", replace 
	******************1834 unique students
	*****************19 students in NSC did not match to grad year
	*****************353 students in grad year file did not match
	
	**enrollmentmajor1 enrollmentcip1
	
*simple cleaning do
	do "2_Cleaning Do Files\NSC base cleaning\1a clean.do"
	
	
	drop  enrollmentbe enrollmenten 
	
	duplicates drop firstname lastname collegecodebranch CollegeName CollegeState EnrollmentBegin EnrollmentEnd degreetitle RecordFound CollegeCodeBranch CollegeType CollegePublic EnrollmentStatus Graduated GraduationDate State, force

	
	
***Loop across years***

	
	foreach i of numlist 2017/2025 {
	preserve
	keep if hs_grad_year == `i'
	save "3_Clean Data Files\NSC\cohort`i'",replace
	restore
	}
	

	do "2_Cleaning Do Files\NSC base cleaning\2 apply decision rule.do"

*create master nsc file
clear

*********

use "3_Clean Data Files\NSC\clean2017.dta" 


foreach i of numlist 2017/2025 {
	drop CollegeSequence
	append using "3_Clean Data Files\NSC\clean`i'.dta"
}

*save all years in one file
save "3_Clean Data Files\NSC_initial_clean.dta", replace 


