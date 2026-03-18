
	
clear
set more off

*use
	use "3_Clean Data Files\NSC_clean_enrollments.dta"
	gen College_level=.
	recode College_level (.=1) if CollegePublic==1 & CollegeType==2
	recode College_level (.=2) if CollegePublic==0 & CollegeType==2
	recode College_level (.=4) if CollegePublic==1 & CollegeType==1
	recode College_level (.=5) if CollegePublic==0 & CollegeType==1
	recode College_level (.=7) if CollegePublic==1 & CollegeType==0
	recode College_level (.=8) if CollegePublic==0 & CollegeType==0
	label define label_sector 1 "Public, 4-year or above", add
	label values College_l label_sector
	label define label_sector 2 "Private not-for-profit, 4-year or above", add
	label values College_l label_sector
	label define label_sector 3 "Private for-profit, 4-year or above", add
	label values College_l label_sector
	label define label_sector 4 "Public, 2-year", add
	label values College_l label_sector
	label define label_sector 5 "Private not-for-profit, 2-year", add
	label values College_l label_sector
	label define label_sector 6 "Private for-profit, 2-year", add
	label values College_l label_sector
	label define label_sector 7 "Public, less-than 2-year", add
	label values College_l label_sector
	label define label_sector 8 "Private not-for-profit, less-than 2-year", add
	label values College_l label_sector
	label define label_sector 9 "Private for-profit, less-than 2-year", add
	label values College_l label_sector
	label define label_sector 99 "Sector unknown (not active)", add
	label values College_l label_sector
	
	*CollegeCodeBranch is not fully populated 
	bys CollegeName: replace CollegeCodeBranch=CollegeCodeBranch[_n+1] if CollegeCodeBranch[_n+1]~=. & CollegeCodeBranch[_n]==.
		bys CollegeName: replace CollegeCodeBranch=CollegeCodeBranch[_n-1] if CollegeCodeBranch[_n-1]~=. & CollegeCodeBranch[_n]==.
	
	keep first last CollegeName CollegeCodeBranch year_sem College_level EnrollmentStatus in_state enrollmentmajor1 enrollmentcip1 hs_grad

	merge 1:1 first last year_semester using "3_Clean Data Files\NSC_clean_degree.dta"
	drop _merge
	
	
	
	*merging in NSC college codes
	merge m:1 CollegeCodeBranch using "3a_Supplementary Data Files\unitid_collegecode.dta"
	drop if _merge==2
	drop _m

	replace IPEDS_UNIT_ID=164748 if CollegeName=="BERKLEE COLLEGE OF MUSIC- THE BOSTON CONSERVATORY"
	replace CollegeCodeBranch=212601 if CollegeName=="BERKLEE COLLEGE OF MUSIC- THE BOSTON CONSERVATORY"
	replace IPEDS_UNIT_ID=382708 if CollegeName=="NATIONAL COLLEGE OF BUSINESS & TECHNOLOG"
	replace CollegeCodeBranch=2276400 if CollegeName=="NATIONAL COLLEGE OF BUSINESS & TECHNOLOG"
	replace CollegeCodeBranch=290300 if CollegeName=="ALBERT EINSTEIN COLLEGE OF MEDICINE"
	replace IPEDS_UNIT_ID=197708 if CollegeName=="ALBERT EINSTEIN COLLEGE OF MEDICINE"
	
	replace IPEDS_UNIT_ID=157748 if CollegeName=="SOUTHERN BAPTIST THEOLOGICAL SEMINARY"
	replace IPEDS_UNIT_ID=212805 if CollegeName=="GROVE CITY COLLEGE"
	replace IPEDS_UNIT_ID=216551 if CollegeName=="VALLEY FORGE MILITARY COLLEGE"
	
	duplicates drop first last year, force
	
	

	
*Save
	*save
	save "3_Clean Data Files\NSC_schoolname by yearsem_LONG.dta", replace
	

	save "3_Clean Data Files\MEGA_initial_LONG.dta", replace
	
	use"3_Clean Data Files\MEGA_initial_LONG.dta", clear
	gen ipeds_unit_id=.
	replace ipeds_unit_id=IPEDS_UNIT_ID if ipeds_unit_id==.
	drop IPEDS_UNIT_ID
	rename ipeds_unit_id IPEDS_UNIT_ID
	merge m:1 IPEDS_UNIT_ID using "3a_Supplementary Data Files\IPEDS_institutional_characteristics.dta"	
	drop if _merge==2
	drop _merge
	

	drop CollegeNameNSC
	
	
**Make one institution name variable
	gen school=""
	replace school=CollegeName if school=="" & CollegeName~=""
	drop CollegeName
	rename school institution
	label var institution "Name of institution attended"
	
**Make one sector variable
	rename sector sector_admin 
	gen sector=.
	replace sector=College_level if sector==. & College_level~=.
	replace sector=sector_admin if sector==. & sector_admin~=.
	la val sector label_sector
	
	recode sector (.=9) if institution=="ART INSTITUTE OF PITTSBURGH"
	recode sector (.=6) if institution=="BRIGHTWOOD CAREER INSTITUTE"
	recode sector (.=6) if institution=="CAREER TRAINING ACADEMY"
	recode sector (.=6) if institution=="DEAN INSTITUTE OF TECHNOLOGY"
	recode sector (.=9) if institu=="EVEREST INSTITUTE - PITTSBURGH"
	recode sector (.=6) if institution=="ITT TECHNICAL INSTITUTE"
	recode sector (.=7) if institu=="LAWRENCE COUNTY VO-TECH OF PRCTICAL NURSING"
	recode sector (.=6) if institu=="LE CORDON BLEU INSTITUTE OF CULINARY ARTS IN PGH"
	recode sector (.=9) if institu=="UNITED EDUCATION INSTITUTE-JACKSONVILLE"
	recode sector (.=6) if institu=="ART INSTITUTE OF PHILADELPHIA"
	recode sector (.=9) if institu=="BRADFORD SCHOOL"
	recode sector (.=9) if institu=="EMPIRE BEAUTY SCHOOL"
	
	recode sector (.=4) if institu=="HARRISBURG AREA COMMUNITY COLLEGE"
	recode sector (.=6) if institu=="LE CORDON BLEU INSTITUTE OF CULINARY ART"
	recode sector (.=8) if institu=="OHIO VALLEY GENERAL HOSPITALSCHOOL OF NURSING"
    recode sector (.=1) if institu=="PENNSYLVANIA STATE UNIVERSITY"
	recode sector (.=1) if institu=="PENNSYLVANIA STATE UNIVERSITY - GREATER ALLEGHENY"
    recode sector (.=2) if institu=="PHILADELPHIA UNIVERSITY"
    recode sector (.=1) if institu=="PURDUE UNIVERSITY GLOBAL"
    recode sector (.=2) if institu=="ROBERT MORRIS UNIVERSITY"
    recode sector (.=9) if institu=="WYOTECH - BLAIRSVILLE"

	
	drop school_degree_2 College_l sector_admin
	rename sector inst_sector
	label var inst_sector "Institution sector"

	gen degree=.
	replace degree=degree_attained_NSC if degree==.
	replace degree=2 if degree_attained==2 & degree==.
	replace degree=3 if degree_attained==3 & degree==.
	replace degree=4 if degree_attained==4 & degree==.

	lab val degree vDegreeAttained
	gen degree_NSC = degree_attained_NSC
	
	drop degree_attained*
	

	
	duplicates drop first last year, force
	

	save "3_Clean Data Files\MEGA_LONG.dta", replace
	
