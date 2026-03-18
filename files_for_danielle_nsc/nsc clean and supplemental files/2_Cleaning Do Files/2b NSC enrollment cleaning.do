
****NSC DATA****

	clear
	use "3_Clean Data Files\NSC_initial_clean.dta"

	drop if Record==0
	
*remove all graduation rows (cleaned in do-file 3c)
	drop if Graduated==1
	
* create school years similar to promise admin data*
	recode Semester (3=4) (2=3)

*Some records missing Semester entry
	replace Semester=1 if Semester==. & StartMonth==8
	replace Semester=1 if Semester==. & StartMonth==9
	replace Semester=1 if Semester==. & StartMonth==10
	replace Semester=1 if Semester==. & StartMonth==11
	replace Semester=3 if Semester==. & StartMonth==12
	replace Semester=3 if Semester==. & StartMonth==1
	replace Semester=3 if Semester==. & StartMonth==2
	replace Semester=3 if Semester==. & StartMonth==3
	replace Semester=3 if Semester==. & StartMonth==4
	replace Semester=4 if Semester==. & StartMonth==5
	replace Semester=4 if Semester==. & StartMonth==6
	replace Semester=4 if Semester==. & StartMonth==7

*Making enrollment year represent SCHOOL YEAR and not calendar year
*2004-2005*
	gen year=.
	foreach i of numlist 2017/2023 {
		replace year = `i' if (EndYear==(`i'-1) & (Semester==1)) | (EndYear==`i' & (Semester==3|Semester==4))
	}
	

	
*create yearsemester variable
	label values Semester
	tostring Semester, replace
	tostring year, replace
	gen year_semester=year+Semester
	replace year_semester="" if year_semester==".."
	destring year_semester, replace

*Data errors...enrollment ending 5/2018
	drop if year_sem==.3

**removing duplicate records by student name and year_semester
	sort first last year_semester
	duplicates tag first last year_semester, gen(dups)
	*duplicates due to non-tradional enrollment terms (quarters and trimesters)
	duplicates drop first last year_semester, force
	drop dups

* Out of state college
	gen in_state=.
	replace in_state=1 if State==42
	replace in_state=0 if in_state==.
	
*drop unneeded*
	drop EnrollmentBegin EnrollmentEnd GraduationDate StartYear StartMonth EndYear EndMonth year
	drop State Semester FullTime EnrollmentSeq CREDENTIAL_TITLE_REPORTED_TO_NSC CREDENTIAL_LEVEL_DESC
	drop CollegeSe RecordF Graduated GraduationM 
	drop GraduationY degree_attained_NSC

	drop if year_semester==.1
	

*Save
	save "3_Clean Data Files\NSC_clean_enrollments.dta", replace



