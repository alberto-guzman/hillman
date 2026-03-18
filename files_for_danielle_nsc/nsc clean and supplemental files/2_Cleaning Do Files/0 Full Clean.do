

cd "C:\Users\DJL73\OneDrive - University of Pittsburgh\Desktop\Hillman project\files_for_danielle_nsc\files_for_danielle_nsc\nsc clean and supplemental files"


	*2a Initial NSC clean
do "2_Cleaning Do Files\2a Initial NSC clean.do"


	*2b NSC enrollment cleaning 
do "2_Cleaning Do Files\2b NSC enrollment cleaning.do"


	*2c NSC degree cleaning 
do "2_Cleaning Do Files\2c NSC degree cleaning.do"


	*2d NSC degree cleaning 
do "2_Cleaning Do Files\2d NSC record.do"


	*3 megaset
do "2_Cleaning Do Files\3 megaset.do"


	*4a pse enrollment variables
do "2_Cleaning Do Files\4a PSE enrollment variables.do"


	*4b pse degree attainment new
do "2_Cleaning Do Files\4b PSE degree attainment new.do"


	*4c pse persistence
do "2_Cleaning Do Files\4c PSE persistence.do"



order firstnameNSC lastnameNSC hs_grad_year

keep firstnameNSC lastnameNSC hs_grad_year CollegeCodeBranch first_term_NSC instate_initial_NSC inst_sector_initial_NSC inst_sectorsimple_initial_NSC enrollmentstatus_initial IPEDS_initial_NSC first_major first_major_stem last_term_NSC institution_last_NSC instate_last_NSC inst_sector_last_NSC inst_sectorsimple_last_NSC IPEDS_last_NSC institution_postfirst institution_postlast enrolled_ever_NSC enrolled_ever_stem in_nsc_query transferNSC number_transfersNSC transfer_directionNSC delayed_enrollment_NSC delayed_detail_NSC pse_enrolled_2_NSC pse_enrolled_6_NSC pse_enrolled_8_NSC degree_everNSC degree_ever_stemNSC number_degree_nscNSC degree_sum_nscNSC bach_everNSC assoc_everNSC cert_everNSC dip_everNSC unknown_everNSC diploma_instNSC certificate_instNSC associates_instNSC bachelors_instNSC degree_4years_allNSC STEMdeg_4years_allNSC degree_5years_allNSC degree_6years_allNSC dipdegree_2years_allNSC dipdegree_4years_allNSC dipdegree_6years_allNSC certdegree_2years_allNSC certdegree_4years_allNSC certdegree_6years_allNSC assodegree_ever_allNSC assodegree_2years_allNSC assodegree_3years_allNSC assodegree_4years_allNSC assodegree_6years_allNSC STEMbachdegree_4years_allNSC bachdegree_4years_allNSC bachdegree_5years_allNSC bachdegree_6years_allNSC STEMbachdegree_6years_allNSC reten_fall_enter reten_fall_enter_stem reten_fall_enter2 reten_fall_enterSTEM2 reten_fall_enter3 reten_fall_enterSTEM3 pers_fall_enter pers_fall_enterSTEM pers_fall_enter2 pers_fall_enterSTEM2 pers_fall_enter3 pers_fall_enterSTEM3 degree_in6_grad STEMdegree_in6_grad delayed_enrollment_STEM

rename firstnameNSC firstname 
rename lastnameNSC lastname

	merge m:1 firstname lastname using "3_Clean Data Files\hs_grad.dta", update
	drop if _merge==2
	drop _merge
	

	****making variables make sense for this project 
	gen public4yr_initial = inst_sector_initial_NSC == 1
	gen private4yr_initial = inst_sector_initial_NSC == 2
	gen _4yr_initial = inst_sector_initial_NSC == 1 | inst_sector_initial_NSC == 2
	drop inst_sectorsimple_initial_NSC
	gen firsttime_fulltime = enrollmentstatus_initial == 1
	drop enrollmentstatus_initial
	drop first_major_stem inst_sectorsimple_last_NSC institution_postfirst institution_postlast delayed_detail_NSC number_degree_nscNSC degree_sum_nscNSC
	
	gen seamless_enroll = delayed_enrollment_NSC == 0
	gen seamless_enrollSTEM = delayed_enrollment_STEM == 0
	
	order firstname lastname hs_grad_year seamless_enroll seamless_enrollSTEM enrolled_ever_NSC enrolled_ever_stem degree_everNSC degree_ever_stemNSC reten_fall_enter reten_fall_enter_stem reten_fall_enter2 reten_fall_enterSTEM2 reten_fall_enter3 reten_fall_enterSTEM3 pers_fall_enter pers_fall_enterSTEM pers_fall_enter2 pers_fall_enterSTEM2 pers_fall_enter3 pers_fall_enterSTEM3 degree_in6_grad STEMdegree_in6_grad public4yr_initial private4yr_initial _4yr_initial firsttime_fulltime instate_initial_NSC inst_sector_initial_NSC first_major in_nsc_query transferNSC delayed_enrollment_NSC delayed_enrollment_STEM  
	
	
	
foreach var of varlist  enrolled_ever_NSC-STEMdegree_in6_grad instate_initial_NSC transferNSC instate_last_NSC number_transfersNSC bach_everNSC-unknown_everNSC degree_4years_allNSC-STEMbachdegree_6years_allNSC {
    replace `var' = 0 if missing(`var')
}



label variable hs_grad_year "Grad year"
label variable seamless_enroll "1 for seamless enroll"
label variable seamless_enrollSTEM "1 for seamless enroll in STEM"
label variable enrolled_ever_NSC "Ever enrolled"
label variable enrolled_ever_stem "Ever enrolled in STEM"
label variable degree_everNSC "Any degree ever"
label variable degree_ever_stemNSC "Any STEM degree ever"
label variable reten_fall_enter "Retention into year 2"
label variable reten_fall_enter_stem "STEM retention into year 2"
label variable reten_fall_enter2 "Retention into year 3"
label variable reten_fall_enterSTEM2 "STEM retention into year 3"
label variable reten_fall_enter3 "Retention into year 4"
label variable reten_fall_enterSTEM3 "STEM retention into year 4"
label variable pers_fall_enter "Persistence into year 2"
label variable pers_fall_enterSTEM "STEM persistence into year 2"
label variable pers_fall_enter2 "Persistence into year 3"
label variable pers_fall_enterSTEM2 "STEM persistence into year 3"
label variable pers_fall_enter3 "Persistence into year 4"
label variable pers_fall_enterSTEM3 "STEM persistence into year 4"
label variable degree_in6_grad "Any degree in 6 years post-HS"
label variable STEMdegree_in6_grad "Any STEM degree in 6 years post-hs"
label variable public4yr_initial "First enroll in public 4 yr"
label variable private4yr_initial "First enroll in private 4 yr"
label variable _4yr_initial "First enroll in any 4 year"
label variable firsttime_fulltime "1 for full time student"
label variable instate_initial_NSC "1 for in state school"
label variable transferNSC "1 for transfer"
label variable first_term_NSC "First post-hs college term"
label variable degree_4years_allNSC "Any degree in 4"
label variable STEMdeg_4years_allNSC "Any STEM degree in 4"
label variable degree_5years_allNSC "Any degree in 5"
label variable degree_6years_allNSC "Any degree in 6"


save "3_Clean Data Files\clean_Hillman_2017_2025.dta" , replace



***merge outcomes with high school file 

import delimited "C:\Users\DJL73\OneDrive - University of Pittsburgh\Desktop\Hillman project\files_for_danielle_nsc\files_for_danielle_nsc\merged_df_pa_covars.csv", bindquote(strict) clear 

	*trimming
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

*variable name differences 
rename first_name firstname
rename last_name lastname 

*dropping for ease--string in master, numeric in using, etc.
rename hs_grad_year gradyear

***duplicates in master (multiple years of treatment)
merge m:1 firstname lastname using "3_Clean Data Files\clean_Hillman_2017_2025.dta"

drop if _merge==2 
drop _merge 

replace in_nsc_query = 0 if in_nsc_query==.

save "C:\Users\DJL73\OneDrive - University of Pittsburgh\Desktop\Hillman project\files_for_danielle_nsc\files_for_danielle_nsc\clean_Hillman_PAonly.dta", replace



