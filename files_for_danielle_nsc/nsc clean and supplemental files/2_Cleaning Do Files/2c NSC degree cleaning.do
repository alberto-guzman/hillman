*****************************************
* Cleaning Promise NSC Data
*****************************************

clear
set more off

*import and merge NSC 

use "3_Clean Data Files\NSC_raw.dta"
		


*simple 
	do "2_Cleaning Do Files\NSC base cleaning\1a clean.do"
	
*simple degree title clean
	do "2_Cleaning Do Files\NSC base cleaning\2c degree title.do"
	
*Only keep graduates
	keep if Graduated==1

	
	
*CLEAN UP DEGREE TITLE
	drop if CREDENTIAL_TITLE_REPORTED_TO_NSC=="MINOR"
	drop if CREDENTIAL_TITLE_REPORTED_TO_NSC=="RELATED CONCENTRATION"
	replace degree_attained_NSC=4 if CREDENTIAL_TITLE_REPORTED_TO_NSC=="B.S. IN REHABILITATION SCIENCE"

*Decision rule...same student gradYear and degree type...example dropping if 2 associ in one semester
	duplicates drop first last GraduationYear degree_attained_NSC, force

*Some students have two rows for enrollment where they received a degree; removing*	
	duplicates tag first last Graduated GraduationYear, gen(dup_grad_row)
	replace dup_grad_row=. if dup_grad_row==0
	
	gen status=1 if dup_g==.
	replace status=2 if dup_g~=. & degree_attained~=.
	replace status=3 if dup_g~=. & degree_attained==.
	
	label define vDupDegreeStatus 1 "NO dup has degree" 2 "Dup with degree_title" 3 "Dup with NO degree_title"
	la val status vDupDegreeStatus
	
	drop if status==3
	drop dup_grad_row
	
*Recode missing degree_title as unknown degree
	replace degree_attained=0 if degree_attained==.
	

	
	
* Out of state college
	gen in_state=.
	replace in_state=1 if CollegeState=="PA"
	replace in_state=0 if in_state==.
	
	
*Remove un-needed
	drop collegecodebranch CollegeState enrollmentbegin enrollmentend ///
	CREDENTIAL_TITLE_REPORTED_TO_NSC CollegeSequence RecordFound ///
	EnrollmentBegin EnrollmentEnd EnrollmentStatus State GraduationDate Graduated CREDENTIAL_LEVEL_DESC
	
*Format degree level **
	gen school_degree_2level=1 if (CollegeType==1|CollegeType==0) & CollegePublic==1
	recode school_degree_2level (.=2) if (CollegeType==1|CollegeType==0) & CollegePublic==0
	recode school_degree_2level (.=3) if CollegeType==2 & CollegePublic==1
	recode school_degree_2level (.=4) if CollegeType==2 & CollegePublic==0
	recode school_degree_2level (.=5) if CollegeType==0

	la def degreelevel 1 "2y public" 2 "2y private" 3 "4y public" 4 "4y private" 5 "Less than 2"
	la val school_degree_2level degreelevel

	drop CollegeType CollegePublic

*Graduation Term
	gen semester=.
	replace semester=1 if semester==. & GraduationMonth==10
	replace semester=1 if semester==. & GraduationMonth==11
	replace semester=1 if semester==. & GraduationMonth==12
	replace semester=1 if semester==. & GraduationMonth==1
	replace semester=3 if semester==. & GraduationMonth==2
	replace semester=3 if semester==. & GraduationMonth==3
	replace semester=3 if semester==. & GraduationMonth==4
	replace semester=3 if semester==. & GraduationMonth==5
	replace semester=3 if semester==. & GraduationMonth==6
	replace semester=4 if semester==. & GraduationMonth==7
	replace semester=4 if semester==. & GraduationMonth==8
	replace semester=4 if semester==. & GraduationMonth==9
	
	la def gradseme 1 "Fall" 3 "Spring" 4 "Summer"
	la val semester gradseme
	
*Making enrollment year represent SCHOOL YEAR and not calendar year

	gen year_grad=.
	foreach i of numlist 2017/2026 {
	replace year_grad = `i' if (GraduationYear==(`i'-1) & (semester==1)) | (GraduationYear==`i' & (semester==3|semester==4))
	}

	
*create yearsemester variable
	label values semester
	tostring semester, replace
	tostring year, replace
	gen year_semester=year+semester
	destring year_semester, force replace

*Sorting 
	gsort first last year_seme -degree_attained
	
*final cleanup
	drop GraduationMonth semester year_grad
	
*getting rid of the duplicates
	duplicates tag first last year_semester, gen(dup)
	bys first last GraduationYear (degree_attained_NSC): gen keep = 1 if degree_attained_NSC==degree_attained_NSC[_N]
	replace keep=0 if keep==.
	bys first last GraduationYear (degree_attained_NSC): drop if dup>0 & keep==0
	drop dup keep
	drop hs_grad
	
*Save
	save "3_Clean Data Files\NSC_clean_degree.dta", replace

	


