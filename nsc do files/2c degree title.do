*******************************************************************************

* PROMISE
* NSC DATASETS

* DEGREE TITLE MATCHING

*******************************************************************************

	* ^ Beginning of string
	* \ is to be used before a period
	
	/* 
	keep degree_title
	keep if degree_title!=""
	sort degree_title
	*/
*merge in NSC degree code look-up file
	rename degreetitle CREDENTIAL_TITLE_REPORTED_TO_NSC
	merge m:1 CREDENTIAL_TITLE_REPORTED_TO_NSC using "3a_Supplementary Data Files\NSC_degree_name.dta"

	
	***some did not merge properly
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="BACHELOR OF ARTS IN INTERIOR ARCHITECTURE"
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="BACHELOR OF SCIENCE IN BUSINESS ADMINISTRATION"
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="BS IN BEHAVIORAL NEUROSCIENCE"	
		replace CREDENTIAL_LEVEL_CODE="UC" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="CERTIFICATE SUB BAC"	
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="DESIGNATED POE"	
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="DOUBLE MAJOR"	
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="INDIVIDUAL POE"
		replace CREDENTIAL_LEVEL_CODE="MD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="MASTER OF ARTS IN ART THERAPY AND COUNSELING"
		replace CREDENTIAL_LEVEL_CODE="MD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="MASTERS OF ARTS & SCIENCE"
		replace CREDENTIAL_LEVEL_CODE="MD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="MS IN ELECTRICAL & COMP ENGR"
		replace CREDENTIAL_LEVEL_CODE="MD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="MS IN SUSTAINABLE ENGINEERING"
		replace CREDENTIAL_LEVEL_CODE="PC" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="ORGANIZATION DEVELOPMENT CERT"
		replace CREDENTIAL_LEVEL_CODE="UC" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="POST SECOND CERT/DIPL <1 YR"
		replace CREDENTIAL_LEVEL_CODE="BD" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="SPECIALIST IN EDUCATION"
		replace CREDENTIAL_LEVEL_CODE="UC" if CREDENTIAL_TITLE_REPORTED_TO_NSC=="UCRT"
		
		
	
	drop if _merge==2
	drop _merge
	
	
	replace CREDENTIAL_LEVEL_CODE="1" if CREDENTIAL_LEVEL_CODE=="PD"
	replace CREDENTIAL_LEVEL_CODE="2" if CREDENTIAL_LEVEL_CODE=="CR"|CREDENTIAL_LEVEL_CODE=="UC"| CREDENTIAL_LEVEL_CODE=="PC"
	replace CREDENTIAL_LEVEL_CODE="3" if CREDENTIAL_LEVEL_CODE=="AD"
	replace CREDENTIAL_LEVEL_CODE="4" if CREDENTIAL_LEVEL_CODE=="BD"
	replace CREDENTIAL_LEVEL_CODE="5" if CREDENTIAL_LEVEL_CODE=="MD"
	replace CREDENTIAL_LEVEL_CODE="6" if CREDENTIAL_LEVEL_CODE=="DP"|CREDENTIAL_LEVEL_CODE=="DR"
	
	bys CREDENTIAL_LEVEL_CODE: replace CREDENTIAL_LEVEL_DESC=CREDENTIAL_LEVEL_DESC[_n+1] if CREDENTIAL_LEVEL_DESC[_n]=="" & CREDENTIAL_LEVEL_DESC[_n+1]~=""
		bys CREDENTIAL_LEVEL_CODE: replace CREDENTIAL_LEVEL_DESC=CREDENTIAL_LEVEL_DESC[_n-1] if CREDENTIAL_LEVEL_DESC[_n]=="" & CREDENTIAL_LEVEL_DESC[_n-1]~=""
	
	destring CREDENTIAL_LEVEL_CODE, replace
	
	rename CREDENTIAL_LEVEL_CODE degree_attained_NSC
	
	la def vDegreeAttained 4 "Bachelors" 6 "Doctorate" 5 "Masters" 3 "Associates" 2 "Certificate" 1 "Diploma" 0 "Unknown degree", modify
	la val degree_attained_NSC vDegreeAttained
	
	/* CHECK OVERLAPPING DEGREE
	egen Flag = rowtotal(Associate Certificate Diploma Bachelor Doctor Master Other)
	sort Flag
	tab Flag
	*/
