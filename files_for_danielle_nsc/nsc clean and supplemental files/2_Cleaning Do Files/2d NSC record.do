

*Import NSC file
clear

*import and merge NSC 

use "3_Clean Data Files\NSC_raw.dta"
		
	
	
	drop if first==""
	drop if last==""
	
	keep first last 
	
	duplicates drop
	
	
*Gen in query
	gen in_nsc_query=1
	
	
	
*Save
	save "3_Clean Data Files\NSC record.dta",replace
	
