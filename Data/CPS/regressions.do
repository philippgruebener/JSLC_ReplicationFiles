log using "$figurepath/regressions", replace text

********************************************************************************
* Regression of head's job loss on spousal labor supply 
********************************************************************************

********************************************************************************
* Merge in state unemployment rates
********************************************************************************

//  Generate date identifier
gen help = mdy(month,15,year)
gen date = mofd(help)
format date %tm
drop help

// merge aggregate unemployment rate
merge m:1 date statefip using "$datapath/stateUR.dta"

drop if _merge != 3 // different date or DC
drop _merge

********************************************************************************
* Dummy variables labor market switches for regressions
********************************************************************************

// dummy indicating spousal labor market switch
gen spouse_out = 0 if trans_ind_sp == 33
replace spouse_out = 1 if (trans_ind_sp == 31 | trans_ind_sp == 32)

label var spouse_out "Pr(Spouse enters LF)"

// dummy indicating head labor market switch 
gen head_switch = 0 if trans_ind == 11
replace head_switch = 1 if trans_ind == 12 

label var head_switch "Head switches from E $\rightarrow$ U"

********************************************************************************
* Baseline Regression Full Sample
********************************************************************************

// panel dimension of data
xtset cpsidv date

// plot for entire sample [FIGURE 1]

reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_all", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
reg spouse_out F.head_switch  c.UR i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp [aw = panlwt], r 
estimates store F1
outreg2 using "$figurepath/regression_all", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
reg spouse_out head_switch    c.UR i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_all", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
reg spouse_out L.head_switch  c.UR i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_all", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
reg spouse_out L2.head_switch c.UR i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_all", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags.pdf", replace

// robustness with interaction terms

* baseline contemporaneous regression for whole sample
reg spouse_out i.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
outreg2 using "$figurepath/regression_all_interact", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Baseline)

* interactions
reg spouse_out i.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR i.head_switch##i.sex i.head_switch##i.race i.head_switch##i.college i.head_switch##i.college_sp i.head_switch##c.UR i.head_switch##i.year [aw = panlwt], r
outreg2 using "$figurepath/regression_all_interact", tex append drop(i.month i.statefip i.year i.head_switch##i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Interactions)

********************************************************************************
* Regression By Age
********************************************************************************

* FIGURE 2
preserve
estimates clear
keep if age_sp > 25 & age_sp <=35

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_agegroup1", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_agegroup1", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_agegroup1", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_agegroup1", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_agegroup1", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch) xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_2535.pdf", replace

restore

* FIGURE A1
preserve
estimates clear
keep if age_sp >= 36 & age_sp <=45

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_agegroup2", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_agegroup2", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_agegroup2", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_agegroup2", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_agegroup2", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch) xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_3645.pdf", replace

restore

* FIGURE A1
preserve
estimates clear
keep if age_sp >= 46 & age_sp <=55

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_agegroup3", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_agegroup3", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_agegroup3", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_agegroup3", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_agegroup3", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch) xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_4655.pdf", replace

restore

* FIGURE 2
preserve
estimates clear
keep if age_sp >= 56 & age_sp <=65

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_agegroup4", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_agegroup4", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_agegroup4", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_agegroup4", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_agegroup4", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch) xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_5665.pdf", replace

restore


********************************************************************************
* Regression By Reason for U of Primary Earner
********************************************************************************

** layoff 
preserve
estimates clear
keep if whyunemp == 1 | whyunemp == 0

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_layoff", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_layoff", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_layoff", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_layoff", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_layoff", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_layoff.pdf", replace

restore


** job loser 
preserve
estimates clear
keep if whyunemp == 2 | whyunemp == 0

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_jobloss", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_jobloss", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_jobloss", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_jobloss", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_jobloss", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_jobloser.pdf", replace

restore

** temp. job ended
preserve
estimates clear
keep if whyunemp == 3 | whyunemp == 0

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_tempjob", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_tempjob", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_tempjob", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_tempjob", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_tempjob", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_temjob.pdf", replace

restore

** job leaver
preserve
estimates clear
keep if whyunemp == 4 | whyunemp == 0

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_quit", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_quit", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_quit", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_quit", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_quit", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_jobleaver.pdf", replace

restore


********************************************************************************
* Baseline Regression on same years as SIPP (1995-2016)
********************************************************************************

preserve
estimates clear
drop if year > 2016 | year < 1995

qui reg spouse_out F2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r
estimates store F2
outreg2 using "$figurepath/regression_19952016", tex replace drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
qui reg spouse_out F.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r  
estimates store F1
outreg2 using "$figurepath/regression_19952016", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
qui reg spouse_out head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L0
outreg2 using "$figurepath/regression_19952016", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
qui reg spouse_out L.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L1
outreg2 using "$figurepath/regression_19952016", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
qui reg spouse_out L2.head_switch i.month i.statefip i.year i.sex i.race i.nchild i.college i.college_sp c.UR [aw = panlwt], r 
estimates store L2
outreg2 using "$figurepath/regression_19952016", tex append drop(i.month i.statefip i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch) xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago") 
graph export "$figurepath/directreg_leadslags_19952016.pdf", replace

restore

log close 

