*******************************************************************************

* NSC DATASET

* CLEANING SCRIPTS
*******************************************************************************
import excel "/Users/albertoguzman-alvarez/Projects/inProgress/2020_hillman/CLEANED_502003_T209672.202110221010_DA.xlsx", sheet("502003_T209672.202110221010_DA") firstrow case(lower) allstring


	** CLEAN VARIABLES: destring, clean values, etc.
	
	* REQUESTER RETURN FIELD (this variable indicates students' ID)
	*split requesterreturnfield, p("")
	
	*destring requesterreturnfield1, replace
	
	*rename requesterreturnfield1 RequesterReturnField
	
	*la var RequesterReturnField "Requester Return Field"
	drop requesterreturnfield
	egen requesterreturnfield = group(firstname middleinitial lastname), missing
	codebook requesterreturnfield
	
	* RECORD FOUND
	gen RecordFound = .
	
	la var RecordFound "1 if Record is Found"
	
	replace RecordFound = 0 if recordfoundyn=="N"
	replace RecordFound = 1 if recordfoundyn=="Y"
	
	label define vRecordFound 0 "No" 1 "Yes"
	label values RecordFound vRecordFound
	
	
	* COLLEGE ID
	split collegecodebranch, p("-")
	
	gen CollegeCodeBranch = collegecodebranch1 + collegecodebranch2
	
	destring CollegeCodeBranch, replace
	la var CollegeCodeBranch "FICE Code of the college attended"
	
	rename collegename CollegeName
	la var CollegeName "Name of the college attended"
	
	drop collegecodebranch1 collegecodebranch2
	
	rename collegestate CollegeState
	la var CollegeState "State where the college is located"
	
	* COLLEGE TYPE
	
	gen CollegeType = 0
	la var CollegeType "Type of School"
	
	replace CollegeType = 2 if year4year=="4"
	replace CollegeType = 1 if year4year=="2"
	replace CollegeType = 0 if year4year=="L"
	
	label define vCollegeType 0 "Less than 2 year" 1 "2-year" 2 "4-year"
	label val CollegeT vCollegeType
	drop year4year
	
	* PUBLIC OR PRIVATE COLLEGE
	
	gen CollegePublic = .
	la var CollegePublic "1 if public college"
	
	replace CollegePublic = 1 if publicprivate == "Public"
	replace CollegePublic = 0 if publicprivate == "Private"
	
	drop publicprivate
	
	label define vCollegePublic 0 "Public" 1 "Private"
	label value CollegePublic vCollegePublic
	
	* START OF ENROLLMENT 
	tostring enrollmentbegin, replace format(%20.0f)
	
	gen EnrollmentBegin = date(enrollmentbegin, "YMD")
	format EnrollmentBegin %td

	la var EnrollmentBegin "Start of enrollment"
	
	* END OF ENROLLMENT
	tostring enrollmentend, replace format(%20.0f)
	
	gen EnrollmentEnd = date(enrollmentend, "YMD")
	format EnrollmentEnd %td
	
	la var EnrollmentEnd "End of enrollment" 
	
	* ENROLLMENT STATUS
	gen EnrollmentStatus = .
	la var EnrollmentStatus "Enrollment status"
	
	replace EnrollmentStatus = 1 if enrollmentstatus == "F"
	replace EnrollmentStatus = 2 if enrollmentstatus == "H"
	replace EnrollmentStatus = 3 if enrollmentstatus == "L"
	replace EnrollmentStatus = 4 if enrollmentstatus == "Q"
	replace EnrollmentStatus = 5 if enrollmentstatus == "A"
	replace EnrollmentStatus = 6 if enrollmentstatus == "W"
	replace EnrollmentStatus = 7 if enrollmentstatus == "D"
	
	drop enrollmentstatus

	label define vEnrollmentStatus 1 "Full-time" 2 "Half-time" 3 "Less than half-time"/*
	*/ 4 "Quarter Time" 5 "Leave of absence" 6 "Withdrawn" 7 "Deceased" 
	
	label values EnrollmentStatus vEnrollmentStatus
	
	* CLASS LEVEL
	gen ClassLevel = .
	la var ClassLevel "Class level on college"
	
	replace ClassLevel = 1 if classlevel == "F"
	replace ClassLevel = 2 if classlevel == "S"
	replace ClassLevel = 3 if classlevel == "J"
	replace ClassLevel = 4 if classlevel == "R"
	replace ClassLevel = 5 if classlevel == "C"
	replace ClassLevel = 6 if classlevel == "N"
	replace ClassLevel = 7 if classlevel == "M"
	replace ClassLevel = 8 if classlevel == "D"
	replace ClassLevel = 9 if classlevel == "P"
	replace ClassLevel = 10 if classlevel == "L"
	replace ClassLevel = 11 if classlevel == "G"
	
	drop classlevel

	label define vClassLevel 1 "Freshman" 2 "Sophomore" 3 "Junior"/*
	*/ 4 "Senior" 5 "Certificate" 6 "Unspecified" 7 "Master's" /*
	*/ 8 "Doctoral" 9 "Postdoctorate" 10 "First professional" 11 "Unspecified graduate"
	
	label values ClassLevel vClassLevel 
	

	* GRADUATION STATUS
	gen Graduated = .
	
	replace Graduated = 0 if graduated=="N"
	replace Graduated = 1 if graduated=="Y"
	
	la var Graduated "Graduated from college"
	
	label define vGraduated 1 "Yes" 0 "No"
	
	label values Graduated vGraduated
	
	drop graduated
	
	* GRADUATION DATE
	tostring graduationdate, replace format(%20.0f)
	
	gen GraduationDate = date(graduationdate, "YMD")
	
	format GraduationDate %td
	
	la var GraduationDate "Date of Graduation" 
	
	drop graduationdate
	
	* DEGREE AND MAJOR
	la var degreetitle "Degree Title"
	
	
	
	* COLLEGE SEQUENCE
	rename collegesequence CollegeSequence
	
	la var CollegeSequence "Sequence of college attendance"
	
	** STATE DUMMIES
	
	gen State = 0 if CollegeState == "DC"
	la var State "State where the college is located"
	
	replace State = 1 if CollegeState == "AL"
	replace State = 2 if CollegeState == "AK"
	replace State = 4 if CollegeState == "AZ"
	replace State = 5 if CollegeState == "AR"
	replace State = 6 if CollegeState == "CA"
	replace State = 8 if CollegeState == "CO"
	replace State = 9 if CollegeState == "CT"
	replace State = 10 if CollegeState == "DE"
	replace State = 12 if CollegeState == "FL"
	replace State = 13 if CollegeState == "GA"
	replace State = 15 if CollegeState == "HI"
	replace State = 16 if CollegeState == "ID"
	replace State = 17 if CollegeState == "IL"
	replace State = 18 if CollegeState == "IN"
	replace State = 19 if CollegeState == "IA"
	replace State = 20 if CollegeState == "KS"
	replace State = 21 if CollegeState == "KY"
	replace State = 22 if CollegeState == "LA"
	replace State = 23 if CollegeState == "ME"
	replace State = 24 if CollegeState == "MD"
	replace State = 25 if CollegeState == "MA"
	replace State = 26 if CollegeState == "MI"
	replace State = 27 if CollegeState == "MN"
	replace State = 28 if CollegeState == "MS"
	replace State = 29 if CollegeState == "MO"
	replace State = 30 if CollegeState == "MT"
	replace State = 31 if CollegeState == "NE"
	replace State = 32 if CollegeState == "NV"
	replace State = 33 if CollegeState == "NH"
	replace State = 34 if CollegeState == "NJ"
	replace State = 35 if CollegeState == "NM"
	replace State = 36 if CollegeState == "NY"
	replace State = 37 if CollegeState == "NC"
	replace State = 38 if CollegeState == "ND"
	replace State = 39 if CollegeState == "OH"
	replace State = 40 if CollegeState == "OK"
	replace State = 41 if CollegeState == "OR"
	replace State = 42 if CollegeState == "PA"
	replace State = 44 if CollegeState == "RI"
	replace State = 45 if CollegeState == "SC"
	replace State = 46 if CollegeState == "SD"
	replace State = 47 if CollegeState == "TN"
	replace State = 48 if CollegeState == "TX"
	replace State = 49 if CollegeState == "UT"
	replace State = 50 if CollegeState == "VT"
	replace State = 51 if CollegeState == "VA"
	replace State = 53 if CollegeState == "WA"
	replace State = 54 if CollegeState == "WV"
	replace State = 55 if CollegeState == "WI"
	replace State = 56 if CollegeState == "WY"
	
	*** GEN GRADUATION YEAR
	gen GraduationYear = year(GraduationDate)
	
	gen GraduationMonth = month(GraduationDate)
	
	*DROP UNNEEDED
	drop recordfoundyn 
	

	
	*** SORT ENROLLMENT BEGIN BY STUDENT
	sort requesterreturnfield EnrollmentBegin
	
	* STUDENT ID (Starting from 1)
	egen StudentID = group(requesterreturnfield)
	
	gen ApplicationID = requesterreturnfield
	
	drop if RecordFound==0

	
