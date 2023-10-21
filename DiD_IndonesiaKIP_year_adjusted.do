************************
*EDPA 6002: QUANT METHODS ON EVALUATION ED POLICIES AND PROGRAMS
*

************************************************
* Set the critical parameters of the computing environment.
************************ 
* Specify the version of Stata to be used in the analysis (this may be different on your computer):
    version 17
  
* Clear all computer memory and delete any existing stored graphs and matrices:
    clear all
* Turn off space bar for more results (personal preference): 
	set more off

*Import the data 
use "C:\Users\JoaquinFarina\OneDrive - Teachers College, Columbia University\Desktop\TC\2 - Spring2023\EDPA 6002 QUANT\\FinalProject\hh_ind_2016_to_2022_kapita.dta"

*create Time: pre post policy
* time of survey = march each year 
* time of application to the program (administration process) = january to february 
* time of communication of results of administration (ie you are eligible or not to apply for KIP) = march

* time of application to college = march to may (two batchs)
* time of communication of results = june 

* reflects current academic year
* 2019 (march) data reflects 2018-2019 academic year, which we name 2018 
* 2020 (march) data reflects 2019-2020 academic year, which we name 2019 
* 2021 (march) data reflects 2020-2021 academic year, which we name 2020
* 2022 (march) data reflects 2021-2022 academic year, which we name 2021 

* AVOID r622 r621

drop if year == 2016

gen year_adjusted = year - 1

* generate income decile [LATER]
*xtile kapita_decile_hh = kapita [fw=int(wert)], n(10)

* Policy timing
gen T = 0
replace T = 1 if year_adjusted >=2020

* Requirements
* R1 - r2202: Do you accept Family Welfare Card (KKS)?
* R2 - r2203: Have you ever been a beneficiary of the Family Hope Program (PKH)?
* R3 - r616:	Do you have a Smart Indonesia Card (KIP)?
* R4 - Orphanage data not available
* R5 - income decile below or equal to 4

gen income_decile4_or_less = kapita_decile_hh<=4
gen income_below_4M = kapita <= 4000000
gen income_percapita_below_750k = kapita/hh_members <= 750000


gen n_of_requirements = ( r616 == 1) + (r2203 == 1) + (r2202==1) + (income_decile4_or_less == 1)

gen TREAT = .
*replace TREAT = 1 if inlist(n_of_requirements,3,4) | (n_of_requirements == 2 & (income_below_4M == 1 | income_percapita_below_750k == 1)) 
*replace TREAT = 0 if inlist(n_of_requirements,0,1) & inlist(kapita_decile_hh,5,6,7,8) 
replace TREAT = 1 if inlist(kapita_decile_hh,1,2,3,4) 
replace TREAT = 0 if inlist(kapita_decile_hh,7,8,9,10)


gen TREATxT = TREAT*T

gen poverty_index = (r2001b == 1) + (r2001d == 1) + (r2001e == 1) + (r2001f == 1) + (r2001g == 1)+(r2001h==1)+(r2001i==1)+(r2001j==1)+(r2001k==1)+(r2001l==1)+(r2001m==1)

***********************
***********************
* OUTCOME 1: in_college
***********************
***********************

drop in_college
gen in_college = 0
replace in_college = 1 if r610 == 2 & r612 == 5

*keep if target_group == 1
keep if inlist(r407, 18, 19, 20)
* absorb by current district 
reghdfe in_college TREATxT TREAT T [pweight = fwt], noabsorb vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, replace  dec(3) 
reghdfe in_college TREATxT TREAT T poverty_rate [pweight = fwt], noabsorb vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 i.r407 r105 r404 poverty_index [pweight = fwt],absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 i.r407 r105 r404  poverty_index  [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 i.r407 r105 r404  poverty_index i.year_adjusted [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, append  dec(3)

* absorb by 5 years ago district r604
reghdfe in_college TREATxT TREAT T [pweight = fwt], noabsorb vce(cluster r604)
outreg2 using KIP_eval_previousDistrict.doc, replace  dec(3) 
reghdfe in_college TREATxT TREAT T poverty_rate [pweight = fwt], noabsorb vce(cluster r604)
outreg2 using KIP_eval_previousDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt],absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrict.doc, append  dec(3)
reghdfe in_college TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 i.year_adjusted [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrict.doc, append  dec(3)

gen TREATxTxUrban = TREATxT*r105
gen TREATxUrban = TREAT*r105
gen TxUrban  = T*r105

*triple differences - current district
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrictDDD.doc, replace  dec(3) 
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt],absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban  TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 i.year_adjusted [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval_currentDistrictDDD.doc, append  dec(3)

*triple differences - previous district
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD.doc, replace  dec(3) 
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt],absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD.doc, append  dec(3)
reghdfe in_college TREATxTxUrban TREATxT TREATxUrban TxUrban  TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 i.year_adjusted [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD.doc, append  dec(3)

* gender 
gen TREATxTxFemale = TREATxT*r405
gen TREATxFemale = TREAT*r405
gen TxFemale  = T*r405

*triple differences - previous district
reghdfe in_college TREATxTxFemale TREATxT TREATxFemale TxFemale TREAT T [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD_female.doc, replace  dec(3) 
reghdfe in_college TREATxTxFemale TREATxT TREATxFemale TxFemale TREAT T poverty_rate [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD_female.doc, append  dec(3)
reghdfe in_college TREATxTxFemale TREATxT TREATxFemale TxFemale TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt],absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD_female.doc, append  dec(3)
reghdfe in_college TREATxTxFemale TREATxT TREATxFemale TxFemale TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD_female.doc, append  dec(3)
reghdfe in_college TREATxTxFemale TREATxT TREATxFemale TxFemale  TREAT T poverty_rate r405 r407 r105 r404  poverty_index i.r614 i.year_adjusted [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval_previousDistrictDDD_female.doc, append  dec(3)


*****************
* Parallel trends
*****************
bysort year_adjusted TREAT: egen mean_in_college = mean(in_college)
twoway line mean_in_college year_adjusted if TREAT == 0, sort || ///
line mean_in_college year_adjusted if TREAT == 1, sort lpattern(dash) ///
legend(label(1 "Control") label(2 "Treated")) ///
xline(2019.5)

reg in_college TREAT##ibn.year_adjusted [pweight = fwt] if T == 0, vce(cluster r102) hascons
reg in_college TREAT##ibn.year_adjusted poverty_rate r405 i.r407 r105 r404  poverty_index i.year [pweight = fwt] if T == 0, vce(cluster r102) hascons



*********
* OBSOLETE - regular OLS
****************
 
drop if year <2020
gen in_college_2022 = (year == 2022) & (r612 == 5)
ge in_hs_2020_2021 = ((year == 2020) & (r612 <=3)) | ((year == 2021) & (r612 <= 3)) 


reghdfe in_college_2022 r616, absorb(year) vce(cluster r604)
outreg2 using KIP_eval_OLS_previousDistrict.doc, replace  dec(3) 
reghdfe in_college_2022 r616 in_hs_2020_2021 poverty_rate,  absorb(year) vce(cluster r604)
outreg2 using KIP_eval_OLS_previousDistrict.doc, append  dec(3)
reghdfe in_college_2022 r616 in_hs_2020_2021 poverty_rate r405 r407 r105 r404 poverty_index, absorb(year) vce(cluster r604)
outreg2 using KIP_eval_OLS_previousDistrict.doc, append  dec(3)
reghdfe in_college_2022 r616 in_hs_2020_2021 poverty_rate r405 r407 r105 r404  poverty_index i.r614, absorb(year r101) vce(cluster r604)
outreg2 using KIP_eval_OLS_previousDistrict.doc, append  dec(3)
reghdfe in_college_2022 r616 in_hs_2020_2021 poverty_rate r405 r407 r105 r404  poverty_index i.r614, absorb(year r101) vce(cluster r604)
outreg2 using KIP_eval_OLS_previousDistrict.doc, append  dec(3)

reghdfe in_college_2022 r616 T poverty_rate r405 r407 r105 r404  poverty_index i.r614 i.year_adjusted, absorb(r101) vce(cluster r102)
outreg2 using KIP_eval_currentDistrict.doc, append  dec(3)

tab in_collge year if TREAT == 0, col
tab in_collge year if TREAT == 1, col

***********************
* OUTCOME 2: in_highschool
***********************

gen in_highschool = 0
replace in_highschool = 1 if r610 == 2 & r612 == 3

drop target_age_group
gen target_age_group = 0
replace target_age_group = 1 if inlist(r407,15,16, 17,18)
keep if target_age_group == 1

drop if r614 >=3

* absorb by current district 
reghdfe in_highschool TREATxT TREAT T [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval__highschool_currentDistrict.doc, replace  dec(3) 
reghdfe in_highschool TREATxT TREAT T poverty_rate [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval__highschool_currentDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval__highschool_currentDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index[pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval__highschool_currentDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index  i.year_adjusted [pweight = fwt], absorb(r102) vce(cluster r102)
outreg2 using KIP_eval__highschool_currentDistrict.doc, append  dec(3)

* absorb by 5 years ago district 
reghdfe in_highschool TREATxT TREAT T [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval__highschool_5yearsagotDistrict.doc, replace  dec(3) 
reghdfe in_highschool TREATxT TREAT T poverty_rate [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval__highschool_5yearsagotDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404 poverty_index [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval__highschool_5yearsagotDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval__highschool_5yearsagotDistrict.doc, append  dec(3)
reghdfe in_highschool TREATxT TREAT T poverty_rate r405 r407 r105 r404  poverty_index  i.year_adjusted [pweight = fwt], absorb(r604) vce(cluster r604)
outreg2 using KIP_eval__highschool_5yearsagotDistrict.doc, append  dec(3)


*****************
* Parallel trends
*****************
bysort year_adjusted TREAT: egen mean_in_highschool = mean(in_highschool)
twoway line mean_in_highschool year_adjusted if TREAT == 0, sort || ///
line mean_in_highschool year_adjusted if TREAT == 1, sort lpattern(dash) ///
legend(label(1 "Control") label(2 "Treated")) ///
xline(2019.5)

reg in_highschool TREAT##ibn.year_adjusted [pweight = fwt] if T == 0, vce(cluster r102) hascons
reg in_highschool TREAT##ibn.year_adjusted poverty_rate r405 i.r407 r105 r404  poverty_index i.year [pweight = fwt] if T == 0, vce(cluster r102) hascons
