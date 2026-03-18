

clear

*Import file
	use "3_Clean Data Files\MEGA_enrollments_by semester.dta"

	
*drop unneeded for degree outcomes 
	drop ipeds_have seq*
	drop if year==.
	
*Go wide
	reshape wide IPEDS_UNIT_ID institution inst_sector degree_NSC DegreeAttainedSTEM in_state major_stem, i(firstn lastn) j(year_semester)

*Bring in cohort
	merge 1:m firstn lastn using "3_Clean Data Files\MEGA_enrollment.dta", keepusing(cohort hs_grad_year)
	drop if _merge==2
	drop _merge
	
*order
	egen temp_id = group(firstn lastn)
	order cohort, after(temp_id)
	order hs_grad_year, after(temp_id)
	
*Degree attainment Y/N
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
			capture noisily: gen degree_nsc_`i'`x'=1 if degree_NSC`i'`x'~=.
		}
	}
	

	gen degree_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace degree_ever=1 if degree_nsc_`i'`x'==1
	}
	}
	
	
	gen degree_ever_stem=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace degree_ever_stem=1 if DegreeAttainedSTEM`i'`x'==1
	}
	}
	
*Degree count

	egen number_degree_nsc= rsum(degree_nsc_2*)


	la var number_degree "Total number of degrees recieved"

*Degree summarization
	gen degree_sum_nsc=.
	
	foreach i of numlist   2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace degree_sum_nsc=degree_NSC`i'`x' if degree_sum_nsc==.
	}
	}

	*la def vDegreeAttained 1 "Other" 2 "Diploma" 3 "Certificate" 4 "Associates" 5 "Bachelors"
	la val degree_sum_nsc vDegreeAttained
	la var degree_sum_nsc "First degree received NSC"

*Bach Ever
gen bach_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace bach_ever=1 if degree_NSC`i'`x'==4
	}	
	}
	
*Assoc Ever
gen assoc_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace assoc_ever=1 if degree_NSC`i'`x'==3
	}
	}
	
*Cert Ever
gen cert_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace cert_ever=1 if degree_NSC`i'`x'==2
	}
	}
		
*Dip Ever
gen dip_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace dip_ever=1 if degree_NSC`i'`x'==1
	}
	}
	
*Unknown Ever
gen unknown_ever=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace unknown_ever=1 if degree_NSC`i'`x'==0
	}
	}
	
*Institution level of degree attainment
	gen diploma_inst_level=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace diploma_inst_level=inst_sector_simple`i'`x' if degree_NSC`i'`x'==1
	}
	}

	la val diploma_inst_level degreelevel
	
	gen certificate_inst_level=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace certificate_inst_level=inst_sector_simple`i'`x' if degree_NSC`i'`x'==2
	}	
	}

	la val certificate_inst_level degreelevel
		
	gen associates_inst_level=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace associates_inst_level=inst_sector_simple`i'`x' if degree_NSC`i'`x'==3
	}
	}

	la val associates_inst_level degreelevel
	
	gen bachelors_inst_level=.
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace bachelors_inst_level=inst_sector_simple`i'`x' if degree_NSC`i'`x'==4
	}
	}
	
	la val bachelors_inst_level degreelevel


*Institution degree attainment for degree verify
	gen diploma_inst=""
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace diploma_inst=institution`i'`x' if degree_NSC`i'`x'==1
	}
	}
	
	gen certificate_inst=""
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace certificate_inst=institution`i'`x' if degree_NSC`i'`x'==2
	}	
	}
	
	gen associates_inst=""
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace associates_inst=institution`i'`x' if degree_NSC`i'`x'==3
	}
	}

	gen bachelors_inst=""
	foreach i of numlist  2017/2026 {
		foreach x of numlist 1 3 4 {
	capture noisily: replace bachelors_inst=institution`i'`x' if degree_NSC`i'`x'==4
	}
	}


*Any Degree in 4years post HS grad
**2010 grad

	foreach i of numlist 2017/2026 {
		gen degree_4years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/5 {
		local Y = `i'+`z'
		capture noisily: recode degree_4years_`i'_nsc (.=1) if hs_grad_year==`i' & degree_nsc_`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode degree_4years_`i' (.=0) if hs_grad_year==`i'
		la var degree_4years_`i'_nsc "`i' ANY degree in 4years after HS"
	}

	
**Combine
	egen degree_4years_all=rsum(degree_4years_20*)	
	
	****STEM in 4 
	foreach i of numlist 2017/2026 {
		gen STEMdeg_4years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/5 {
		local Y = `i'+`z'
		capture noisily: recode STEMdeg_4years_`i'_nsc (.=1) if hs_grad_year==`i' & DegreeAttainedSTEM`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode STEMdeg_4years_`i' (.=0) if hs_grad_year==`i'
		la var STEMdeg_4years_`i'_nsc "`i' STEM degree in 4years after HS"
	}

	
**Combine
	egen STEMdeg_4years_all=rsum(STEMdeg_4years_20*)	
	
	
	
*Any Degree in 5years post HS grad
**2010 grad

	foreach i of numlist 2017/2026 {
		gen degree_5years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/5 {
		local Y = `i'+`z'
		capture noisily: recode degree_5years_`i'_nsc (.=1) if hs_grad_year==`i' & degree_nsc_`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode degree_5years_`i' (.=0) if hs_grad_year==`i'
		la var degree_5years_`i'_nsc "`i' ANY degree in 5years after grad NSC"
	}

	
**Combine
	egen degree_5years_all=rsum(degree_5years_20*)
		

*Any Degree in 6years post HS grad

	foreach i of numlist 2017/2026 {
		gen degree_6years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
		local Y = `i'+`z'
		capture noisily: recode degree_6years_`i'_nsc (.=1) if hs_grad_year==`i' & degree_nsc_`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode degree_6years_`i' (.=0) if hs_grad_year==`i'
		la var degree_6years_`i'_nsc "`i' ANY degree in 6years after grad NSC"
	}

	
**Combine
	egen degree_6years_all=rsum(degree_6years_20*)		
	
	
*Any Degree in 8years post HS grad
**2010 grad

	foreach i of numlist 2017/2026 {
		gen degree_8years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/8 {
		local Y = `i'+`z'
		capture noisily: recode degree_8years_`i'_nsc (.=1) if hs_grad_year==`i' & degree_nsc_`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode degree_8years_`i' (.=0) if hs_grad_year==`i'
		la var degree_8years_`i'_nsc "`i' ANY degree in 8years after grad NSC"
	}

	
**Combine
	egen degree_8years_all=rsum(degree_8years_20*)	
	
	
	
*Degree attainment  Diploma==1*

	*within 2 years
	foreach i of numlist 2017/2026 {
		gen dipdegree_2years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/2 {
		local Y = `i'+`z'
		capture noisily: recode dipdegree_2years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode dipdegree_2years_`i' (.=0) if hs_grad_year==`i'
		la var dipdegree_2years_`i'_nsc "`i' cohort diploma in 2years NSC"
	}
	
		egen dipdegree_2years_all=rsum(dipdegree_2years_20*) 
		la var dipdegree_2years_all "All cohorts diploma in 2y NSC"

	*within 4 years
	foreach i of numlist 2017/2026 {
		gen dipdegree_4years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/4 {
		local Y = `i'+`z'
		capture noisily: recode dipdegree_4years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode dipdegree_4years_`i' (.=0) if hs_grad_year==`i'
		la var dipdegree_4years_`i'_nsc "`i' cohort diploma in 4years NSC"
	}
	
		egen dipdegree_4years_all=rsum(dipdegree_4years_20*) 
		la var dipdegree_4years_all "All cohorts diploma in 4y NSC"

	*within 6 years
	foreach i of numlist 2017/2026 {
		gen dipdegree_6years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
		local Y = `i'+`z'
		capture noisily: recode dipdegree_6years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==1
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode dipdegree_6years_`i' (.=0) if hs_grad_year==`i'
		la var dipdegree_6years_`i'_nsc "`i' cohort diploma in 6years NSC"
	}
	
		egen dipdegree_6years_all=rsum(dipdegree_6years_20*) 
		la var dipdegree_6years_all "All cohorts diploma in 6y NSC"

*Drop unneeded variables
	drop dipdegree_2years_20* dipdegree_4years_20* dipdegree_6years_20*			

		
*Degree attainment  certificate==2

	*within 2 years
	foreach i of numlist 2017/2026 {
		gen certdegree_2years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/2 {
		local Y = `i'+`z'
		capture noisily: recode certdegree_2years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==2
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode certdegree_2years_`i' (.=0) if hs_grad_year==`i'
		la var certdegree_2years_`i'_nsc "`i' cohort certificate in 2years NSC"
	}
	
		egen certdegree_2years_all=rsum(certdegree_2years_20*) 
		la var certdegree_2years_all "All cohorts certificate in 2y NSC"
		
	*within 4 years
	foreach i of numlist 2017/2026 {
		gen certdegree_4years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/4 {
		local Y = `i'+`z'
		capture noisily: recode certdegree_4years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==2
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode certdegree_4years_`i' (.=0) if hs_grad_year==`i'
		la var certdegree_4years_`i'_nsc "`i' cohort certificate in 4years NSC"
	}
	
		egen certdegree_4years_all=rsum(certdegree_4years_20*) 
		la var certdegree_4years_all "All cohorts certificate in 4y NSC"
			
		
	*within 6 years
	foreach i of numlist 2017/2026 {
		gen certdegree_6years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
		local Y = `i'+`z'
		capture noisily: recode certdegree_6years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==2
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode certdegree_6years_`i' (.=0) if hs_grad_year==`i'
		la var certdegree_6years_`i'_nsc "`i' cohort certificate in 6years NSC"
	}
	
		egen certdegree_6years_all=rsum(certdegree_6years_20*) 
		la var certdegree_6years_all "All cohorts certificate in 6y NSC"		
	
*Drop unneeded variables
	drop certdegree_2years_20* certdegree_4years_20* certdegree_6years_20*	
	
		
*Degree attainment  ASSOCIATES==3*

	*within 2 years
	foreach i of numlist 2017/2026 {
		gen assodegree_2years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/2 {
		local Y = `i'+`z'
		capture noisily: recode assodegree_2years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==3
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode assodegree_2years_`i' (.=0) if hs_grad_year==`i'
		la var assodegree_2years_`i'_nsc "`i' cohort Associates degree in 2years NSC"
	}

	
	*within 3 years
	foreach i of numlist 2017/2026 {
		gen assodegree_3years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/3 {
		local Y = `i'+`z'
		capture noisily: recode assodegree_3years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==3
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode assodegree_3years_`i' (.=0) if hs_grad_year==`i'
		la var assodegree_3years_`i'_nsc "`i' cohort Associates degree in 3years NSC"
	}	
	
	*within 4 years
	foreach i of numlist 2017/2026 {
		gen assodegree_4years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/4 {
		local Y = `i'+`z'
		capture noisily: recode assodegree_4years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==3
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode assodegree_4years_`i' (.=0) if hs_grad_year==`i'
		la var assodegree_4years_`i'_nsc "`i' cohort Associates degree in 3years NSC"
	}	
	
	*within 6 years
	foreach i of numlist 2017/2026 {
		gen assodegree_6years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
		local Y = `i'+`z'
		capture noisily: recode assodegree_6years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==3
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode assodegree_6years_`i' (.=0) if hs_grad_year==`i'
		la var assodegree_6years_`i'_nsc "`i' cohort Associates degree in 6years NSC"
	}	
	

	*within 8 years
	foreach i of numlist 2017/2026 {
		gen assodegree_8years_`i'_nsc=.
	}

	foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/8 {
		local Y = `i'+`z'
		capture noisily: recode assodegree_8years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==3
	}
	}
	}
	
	foreach i of numlist 2017/2026 {
		recode assodegree_8years_`i' (.=0) if hs_grad_year==`i'
		la var assodegree_8years_`i'_nsc "`i' cohort Associates degree in 8years NSC"
	}	
	
	
	
*Associates ever
	gen assodegree_ever_all=.
	
		foreach i of numlist 2017/2026 {
		foreach x of numlist 1 3 4 {
			foreach z of numlist 0/3 {
		local Y = `i'+`z'
	capture noisily: recode assodegree_ever_all (.=1) if degree_NSC`Y'`x'==3
		}
		}
		}
	recode assodegree_ever_all (.=0)
	
	la var assodegree_ever_all "All cohorts Associates degree ever, regardless of time"
		

	
*Combine cohorts into one variable
	egen assodegree_2years_all=rsum(assodegree_2years_20*) 
	
	la var assodegree_2years_all "All cohorts Associates degree in 2y NSC"

	
	egen assodegree_3years_all=rsum(assodegree_3years_20*)

	la var assodegree_3years_all "All cohorts Associates degree in 3 years NSC"

	
	egen assodegree_4years_all=rsum(assodegree_4years_20*)

	la var assodegree_4years_all "All cohorts Associates degree in 4 years NSC"
	

	egen assodegree_6years_all=rsum(assodegree_6years_20*)

	la var assodegree_6years_all "All cohorts Associates degree in 6 years NSC"
	
	
	egen assodegree_8years_all=rsum(assodegree_8years_20*)

	la var assodegree_8years_all "All cohorts Associates degree in 8 years NSC"

*Drop unneeded variables
	drop assodegree_2years_20* assodegree_3years_20* assodegree_4years_20* assodegree_6years_20* assodegree_8years_20*
	
	
*Degree attainment  BACHELORS=4*
			*4 years
			
	foreach i of numlist 2017/2026 {
		gen bachdegree_4years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/4 {
			local Y = `i'+`z'
	capture noisily: recode bachdegree_4years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4
	
			}
			}
			}
			
				foreach i of numlist 2017/2026 {
	recode bachdegree_4years_`i'_nsc (.=0) if cohort==`i'
	la var bachdegree_4years_`i'_nsc "`i' Cohort Bachelors in 4y NSC"
				}
				
				
*Degree attainment  BACHELORS=4*
			*4 years STEM
			
	foreach i of numlist 2017/2026 {
		gen STEMbachdegree_4years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/4 {
			local Y = `i'+`z'
	capture noisily: recode bachdegree_4years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4 & DegreeAttainedSTEM`Y'`x'==1
	
			}
			}
			}
			
				foreach i of numlist 2017/2026 {
	recode STEMbachdegree_4years_`i'_nsc (.=0) if cohort==`i'
	la var STEMbachdegree_4years_`i'_nsc "`i' Cohort STEM Bachelors in 4y NSC"
				}
				
							
				
				
			*5 years
	foreach i of numlist 2017/2026 {
		gen bachdegree_5years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/5 {
			local Y = `i'+`z'
	capture noisily: recode bachdegree_5years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4
			}
			}
			}
			
			foreach i of numlist 2017/2026 {
	recode bachdegree_5years_`i'_nsc (.=0) if cohort==`i'
	la var bachdegree_5years_`i'_nsc "`i' Cohort Bachelors in 5y NSC"
			}
			
			
		*6 years
	foreach i of numlist 2017/2026 {
		gen bachdegree_6years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
			local Y = `i'+`z'
	capture noisily: recode bachdegree_6years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4

			}
			}
			}		

foreach i of numlist 2017/2026 {
	recode bachdegree_6years_`i'_nsc (.=0) if cohort==`i'
	la var bachdegree_6years_`i'_nsc "`i' Cohort Bachelors in 6y NSC"
}


		*6 years STEM
	foreach i of numlist 2017/2026 {
		gen STEMbachdegree_6years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/6 {
			local Y = `i'+`z'
	capture noisily: recode STEMbachdegree_6years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4 & DegreeAttainedSTEM`Y'`x'==1

			}
			}
			}		

foreach i of numlist 2017/2026 {
	recode STEMbachdegree_6years_`i'_nsc (.=0) if cohort==`i'
	la var STEMbachdegree_6years_`i'_nsc "`i' Cohort STEM Bachelors in 6y NSC"
}


			*8 years
	foreach i of numlist 2017/2026 {
		gen bachdegree_8years_`i'_nsc=.
	}
	
	foreach i of numlist 2017/2026 {
			foreach x of numlist 1 3 4 {
			foreach z of numlist 0/8 {
			local Y = `i'+`z'
	capture noisily: recode bachdegree_8years_`i'_nsc (.=1) if cohort==`i' & degree_NSC`Y'`x'==4
			}
			}
			}
			
			foreach i of numlist 2017/2026 {
	recode bachdegree_8years_`i'_nsc (.=0) if cohort==`i'
	la var bachdegree_8years_`i'_nsc "`i' Cohort Bachelors in 8y NSC"
			}
			
	
*Combine all cohorts in one variable
	egen bachdegree_4years_all=rsum(bachdegree_4years_20*) 
	la var bachdegree_4years_all "All cohorts Bachelors in 4y NSC"
	
	egen STEMbachdegree_4years_all=rsum(STEMbachdegree_4years_20*) 
	la var STEMbachdegree_4years_all "STEM Bachelors in 4y NSC"
	
	egen bachdegree_5years_all=rsum(bachdegree_5years_20*) 
	la var bachdegree_5years_all "All cohorts Bachelors in 5y NSC"
	
	egen bachdegree_6years_all=rsum(bachdegree_6years_20*)
	la var bachdegree_6years_all "All cohorts Bachelors in 6y NSC"
	
	egen STEMbachdegree_6years_all=rsum(STEMbachdegree_6years_20*)
	la var STEMbachdegree_6years_all "STEM Bachelors in 6y NSC"

	egen bachdegree_8years_all=rsum(bachdegree_8years_20*)
	la var bachdegree_8years_all "All cohorts Bachelors in 8y NSC"
		
*Drop unneeded variables
	drop bachdegree_4years_20* STEMbachdegree_4years_20* bachdegree_5years_20* bachdegree_6years_20* STEMbachdegree_6years_20* bachdegree_8years_20*

*Any degree in 3

	gen degree_in3=.
	foreach i of numlist 2017/2026 {
	local zero = `i'
	local first = `i'+1
	local second = `i'+2
	local third = `i'+3
	
	foreach x of numlist 1 3 4 {
	noisily capture: recode degree_in3 (.=1) if cohort==`i' & degree_NSC`zero'`x'<5 & degree_NSC`zero'`x'>0
	noisily capture: recode degree_in3 (.=1) if cohort==`i' & degree_NSC`first'`x'<5 & degree_NSC`first'`x'>0
	noisily capture: recode degree_in3 (.=1) if cohort==`i' & degree_NSC`second'`x'<5 & degree_NSC`second'`x'>0
	noisily capture: recode degree_in3 (.=1) if cohort==`i' & degree_NSC`third'`x'<5 & degree_NSC`third'`x'>0
	
	}
	}

	
	drop degree_5years_20*
	drop cohort
	

	duplicates drop

	rename * NSC*

		
*Save
	save "3_Clean Data Files\MEGA_degree_outcomes_NSC.dta", replace
	
