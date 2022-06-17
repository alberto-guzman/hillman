*******************************************************************************

* SEMESTER CLASSIFICATION

* NSC

*******************************************************************************

	*sort lrdc EnrollmentBegin
	
	gen StartYear = year(EnrollmentBegin)
	gen StartMonth = month(EnrollmentBegin)
	
	gen EndYear = year(EnrollmentEnd)
	gen EndMonth = month(EnrollmentEnd)
	
	*** SEMESTER CLASSIFICATION
	gen Semester = .
	
	gen Flag = .
	
	gen Quarter = .
	
	*** FALL SEMESTER, Semester = 1
	
	* Start July
	replace Semester = 1 if StartMonth==7 & (EndMonth>9) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==7 & EndMonth!=12
	
	* Start August
	replace Semester = 1 if StartMonth==8 & (EndMonth>=8) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==7  & EndMonth!=12
	
	* Start September
	replace Semester = 1 if StartMonth==9 & (EndMonth>=9) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==9 & EndMonth!=12
	
	* Start October
	replace Semester = 1 if StartMonth==10 & (EndMonth>=10) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==10 /* flag all */
	
	* Start November
	* Assume late recording of enrollment
	replace Semester = 1 if StartMonth==11 & (EndMonth>=11) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==11 /* flag all */
	
	* Start December
	* Assume late recording of enrollment
	replace Semester = 1 if StartMonth==12 & (EndMonth>=12) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 1 & StartMonth==12 /* flag all */
	
	*** SPRING SEMESTER, Semester = 2
	* Start January
	* Winter semester for college with quarter system
	replace Semester = 2 if StartMonth==1 & (EndMonth>=1) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 2 & (EndMonth==1 | EndMonth==2 | EndMonth>=6)
	replace Quarter = 1 if Semester == 2 & EndMonth == 3
	
	* Start February
	replace Semester = 2 if StartMonth==2 & (EndMonth>=2) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 2 & (EndMonth==3 | EndMonth==4 | EndMonth>=6)
	
	* Start March
	* Spring semester for college with quarter system
	replace Semester = 2 if StartMonth==3 & (EndMonth>=3) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 2 & (EndMonth!=6)
	replace Quarter = 1 if Semester==2 & StartMonth==3
	
	* Start April
	replace Semester = 2 if StartMonth==4 & (EndMonth>=4) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 2 & (EndMonth!=6)
	
	*** SUMMER SEMESTER, Semester = 3
	* Any enrollment starting May, June, and July
	* Start May
	replace Semester = 3 if StartMonth==5 & (EndMonth>=5) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 3 & (EndMonth<6 & EndMonth>8)
	
	* Start June
	replace Semester = 3 if StartMonth==6 & (EndMonth>=6) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 3 & EndMonth==6 | EndMonth>=10
	
	* Start July
	replace Semester = 3 if StartMonth==7 & (EndMonth<=9) /*
	*/						& StartYear==EndYear
	
	replace Flag = 1 if Semester == 3 & EndMonth==7
	
	*** LEAP YEAR
	* Some fall enrollment ended in the spring of next year
	* Start August, revise the year
	replace Semester = 1 if StartMonth==8 & (EndMonth<=2) /*
	*/						& EndYear==StartYear+1
	
	replace EndYear = StartYear if Semester == 1 &/*
	*/		StartMonth==8 & EndMonth<=2 & EndYear==StartYear+1
	
	* Start September
	replace Semester = 1 if StartMonth==9 & (EndMonth<=2) /*
	*/						& EndYear==StartYear+1
	
	replace EndYear = StartYear if Semester == 1 & /*
	*/		StartMonth==9 & EndMonth<=2 & EndYear==StartYear+1
	
	* Start October
	replace Semester = 2 if StartMonth==10 & (EndMonth>=1) /*
	*/						& EndYear==StartYear+1
	
	gen byte FlagSpring = StartMonth==10 & EndMonth>3
	
	* Start November
	replace Semester = 2 if StartMonth==11 & (EndMonth>=1) /*
	*/						& EndYear==StartYear+1
	
	replace FlagSpring = 1 if StartMonth==11 & EndMonth>5
	
	* Start December
	replace Semester = 2 if StartMonth==12 & (EndMonth>=1) /*
	*/						& EndYear==StartYear+1
	
	replace FlagSpring = 1 if StartMonth==12 & EndMonth>5
	
	replace StartMonth = 1 if FlagSpring == 1
	replace StartYear = StartYear + 1 if FlagSpring == 1
	
	*** DEFINE LABEL
	
	label define vSemester 1 "Fall" 2 "Spring" 3 "Summer"
	label variable Semester "Semester"
	label value Semester vSemester
	
