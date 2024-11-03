log using "SIPP", replace text

********************************************************************************
********************************************************************************
* Joint Search over the Life Cycle
* SIPP data analysis
********************************************************************************
********************************************************************************

cls
clear
clear matrix
macro drop _all
set more off

********************************************************************************
* Load prepared data
********************************************************************************

use "", clear

********************************************************************************
* Sample Selection
********************************************************************************

* Age
drop if age < 26
drop if age > 65

* Only couples
drop if spouseid == .							// drop if no spouse
keep if marital == 1							// keep only individuals who are married with spouse present

********************************************************************************
* Prepare panel
********************************************************************************

* Time variable
replace month = refmon if year >= 2014						// month is missing for 2014 panel
replace year = 2013 if panel == 2014 & wave == 1			// year is missing for 2014 panel
replace year = 2014 if panel == 2014 & wave == 2			// year is missing for 2014 panel
replace year = 2015 if panel == 2014 & wave == 3			// year is missing for 2014 panel
replace year = 2016 if panel == 2014 & wave == 4			// year is missing for 2014 panel
gen time = ym(year,month) 									// collapse year and month to one time variable
label var time "Time variable based on year and month"

* Person variable 
egen id = group(panel suid persid)							// sample unit identifier and person identifier identify individual within panel
label var id "Individual identifier based on panel, sample unit, and person identifier"

* Check for duplicates
duplicates report id time

* Declare panel
xtset id time

********************************************************************************
* Employment Status
********************************************************************************

* Individual employment status
gen empstat = .									// employment status
label var empstat "Employment status: 1:E, 2:U, 3:N"
replace empstat = 1 if empstatmonth == 1		// with a job entire month, worked all weeks
replace empstat = 1 if empstatmonth == 2		// with a job all month, absent from work w/out pay 1+ weeks, absence not due to layoff
replace empstat = 1 if empstatmonth == 3		// with job all month, absent from work w/out pay 1+ weeks
replace empstat = 1 if empstatmonth == 4		// with a job at least 1 but not all weeks, no time on layoff and not time looking for work
replace empstat = 2 if empstatmonth == 5		// with a job at least 1 but not all weeks, some weeks on layoff or looking for work
replace empstat = 2 if empstatmonth == 6 		// no job all month, on layoff or looking for work all weeks
replace empstat = 2 if empstatmonth == 7		// no job, at least one but not all weeks on layoff or looking for work 
replace empstat = 3 if empstatmonth == 8		// no job, no time on layoff and no time looking for work
replace empstat = 2 if empstat == 3 & (whynowork == 8 | whynowork == 9)	// unemployed: not working and either unable to find job or on layoff

********************************************************************************
* Correct for classification error
********************************************************************************

replace empstat = 3 if L.empstat == 3 & empstat == 2 & F.empstat == 3
replace empstat = 2 if L.empstat == 2 & empstat == 3 & F.empstat == 2

********************************************************************************
* Employment Transitions
********************************************************************************

* Individual labor market transitions
gen trans_ind = .
label var trans_ind "Individual labor market transitions"
replace trans_ind = 11 if empstat == 1 & L.empstat == 1	// EE transition
replace trans_ind = 12 if empstat == 2 & L.empstat == 1 // EU transition
replace trans_ind = 13 if empstat == 3 & L.empstat == 1 // EO transition
replace trans_ind = 21 if empstat == 1 & L.empstat == 2	// UE transition
replace trans_ind = 22 if empstat == 2 & L.empstat == 2 // UU transition
replace trans_ind = 23 if empstat == 3 & L.empstat == 2 // UO transition
replace trans_ind = 31 if empstat == 1 & L.empstat == 3	// OE transition
replace trans_ind = 32 if empstat == 2 & L.empstat == 3 // OU transition
replace trans_ind = 33 if empstat == 3 & L.empstat == 3 // OO transition

********************************************************************************
* Asset Holdings
********************************************************************************

* CPI
gen cpi = .
replace cpi = 64.2921534460960 if year == 1995
replace cpi = 66.1766877481362 if year == 1996
replace cpi = 67.7236935184677 if year == 1997
replace cpi = 68.7749542578521 if year == 1998
replace cpi = 70.2797689617200 if year == 1999
replace cpi = 72.6530164502968 if year == 2000
replace cpi = 74.7063150181914 if year == 2001
replace cpi = 75.8911808013771 if year == 2002
replace cpi = 77.6139826819736 if year == 2003
replace cpi = 79.6918927053052 if year == 2004
replace cpi = 82.3956368811801 if year == 2005
replace cpi = 85.0536740683861 if year == 2006
replace cpi = 87.4799768230408 if year == 2007
replace cpi = 90.8384208727644 if year == 2008
replace cpi = 90.5154482589856 if year == 2009
replace cpi = 91.9999409325069 if year == 2010
replace cpi = 94.9042333109721 if year == 2011
replace cpi = 96.8681219771859 if year == 2012
replace cpi = 98.2870778608004 if year == 2013
replace cpi = 99.8815134216814 if year == 2014
replace cpi = 100.0000000000000 if year == 2015
replace cpi = 101.2615832057050 if year == 2016
replace cpi = 103.4185663194340 if year == 2017
label var cpi "CPI"

* Interpolate only variables we actually work with (reduce running time of code)
foreach var of varlist aj_homeequity_recod aj_netequ_vec_recod aj_networth_recod {
	by id: ipolate `var' time, generate(`var'_ipol) epolate
	gen `var'_real = `var'_ipol*100/cpi
}

* Net liquid wealth own definition
gen netliqu_own_real = aj_networth_recod_real - aj_homeequity_recod_real - aj_netequ_vec_recod_real 
label var netliqu_own_real "Real net liquid wealth own definition"

* Quartiles of the liquid asset distribution
xtile netliqu_own_quart = netliqu_own_real, nq(4)
label var netliqu_own_quart "Quartiles of the net liquid asset distribution own definition"

* Halves of the liquid asset distribution
xtile netliqu_own_real_half = netliqu_own_real, nq(2)
label var netliqu_own_real_half "Halves of the net liquid wealth own definition"

********************************************************************************
* Income variables
********************************************************************************

* Convert monthly person's earned income into real terms
gen totpersearn_month_real = totpersearn*100/cpi

* real person's earned income for real earnings of at least 100 dollars
gen totpersearn_month_real_pos = totpersearn_month_real if totpersearn_month_real >= 100

********************************************************************************
* Data Targets for Model 
********************************************************************************

// Income profile 

* monthly income min max (conditional on being employed)
sum totpersearn_month_real     [aw = persweight] if empstat == 1, det

sum totpersearn_month_real_pos [aw = persweight] if empstat == 1, det // USED AS MODEL TARGET (5th + 95th pct) (25 January 2024)

* monthly income cross section // USED AS MODEL TARGET (25 January 2024)
sum totpersearn_month_real_pos [aw = persweight] if empstat == 1
sum totpersearn_month_real_pos [aw = persweight] if age >= 25 & age <= 35 & empstat == 1 
sum totpersearn_month_real_pos [aw = persweight] if age > 35 & age <= 45 & empstat == 1 
sum totpersearn_month_real_pos [aw = persweight] if age > 45 & age <= 55 & empstat == 1 
sum totpersearn_month_real_pos [aw = persweight] if age > 55 & age <= 65 & empstat == 1 

sum totpersearn_month_real_pos if empstat == 1
sum totpersearn_month_real_pos if age >= 25 & age <= 35 & empstat == 1 
sum totpersearn_month_real_pos if age > 35 & age <= 45 & empstat == 1 
sum totpersearn_month_real_pos if age > 45 & age <= 55 & empstat == 1 
sum totpersearn_month_real_pos if age > 55 & age <= 65 & empstat == 1 

// income upon re-employment after xx months relative to last income //

xtset id time

* duration since job loss
gen dur_jl = 0 if trans_ind == 12  | trans_ind == 13
foreach num of numlist 1/24 {
replace dur_jl = `num' if L`num'.trans_ind == 12  & dur_jl == . | L`num'.trans_ind == 13 & dur_jl == . 
}

* duration since job find
gen dur_jf = 0 if trans_ind == 21  | trans_ind == 31
foreach num of numlist 1/24 {
replace dur_jf = `num' if L`num'.trans_ind == 21  & dur_jf == . | L`num'.trans_ind == 31 & dur_jf == . 
}

* cut at 5% and 95% of totpersearn_month_real_pos (refers to min and max income in the model)
xtile inc_pct = totpersearn_month_real_pos [weight = persweight] if empstat == 1, nq(20)
gen ind_small = 1 if inc_pct == 1 | inc_pct == 20
replace ind_small = 0 if inc_pct > 1 & inc_pct < 20 

* income drop upon reemployment - up to 3 months
gen incdrop_3     = log(totpersearn_month_real_pos) - log(L4.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 3	& ind_small == 0 & L4.ind_small == 0 

replace incdrop_3 = log(totpersearn_month_real_pos) - log(L2.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 1	& ind_small == 0 & L2.ind_small == 0
replace incdrop_3 = log(totpersearn_month_real_pos) - log(L3.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 2   & ind_small == 0 & L3.ind_small == 0

* income drop upon reemployment - 4-12 months
gen incdrop_12     = log(totpersearn_month_real_pos) - log(L13.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 12 & ind_small == 0 & L13.ind_small == 0 

replace incdrop_12 = log(totpersearn_month_real_pos) - log(L5.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 4 & ind_small == 0 & L5.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L6.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 5 & ind_small == 0 & L6.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L7.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 6 & ind_small == 0 & L7.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L8.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 7 & ind_small == 0 & L8.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L9.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 8 & ind_small == 0 & L9.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L10.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 9 & ind_small == 0 & L10.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L11.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 10 & ind_small == 0 & L11.ind_small == 0 
replace incdrop_12 = log(totpersearn_month_real_pos) - log(L12.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 11 & ind_small == 0 & L12.ind_small == 0 

* income drop upon reemployment - 13 -24 months
gen incdrop_24     = log(totpersearn_month_real_pos) - log(L25.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 24 & ind_small == 0 & L25.ind_small == 0

replace incdrop_24 = log(totpersearn_month_real_pos) - log(L14.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 13 & ind_small == 0 & L14.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L15.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 14 & ind_small == 0 & L15.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L16.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 15 & ind_small == 0 & L16.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L17.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 16 & ind_small == 0 & L17.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L18.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 17 & ind_small == 0 & L18.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L19.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 18 & ind_small == 0 & L19.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L20.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 19 & ind_small == 0 & L20.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L21.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 20 & ind_small == 0 & L21.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L22.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 21 & ind_small == 0 & L22.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L23.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 22 & ind_small == 0 & L23.ind_small == 0
replace incdrop_24 = log(totpersearn_month_real_pos) - log(L24.totpersearn_month_real_pos) if dur_jf == 0 & dur_jl == 23 & ind_small == 0 & L24.ind_small == 0


// USED AS MODEL TARGET (25 January 2024)
sum incdrop_3  [aw = persweight], det
sum incdrop_12 [aw = persweight], det
sum incdrop_24 [aw = persweight], det

sum incdrop_3  , det
sum incdrop_12 , det
sum incdrop_24 , det

// Sample Size
preserve
rangestat empstat_sp = empstat, by(year month suid) interval(persid spouseid spouseid) 	// mean of empstat within (year, month, sample unit), where persid is equal to spouseid

* Keep only one observation per household and time unit 
bysort panel suid time: keep if _n==1

* Overall sample 
count 
* Overall sample, young 
count if age <= 35
* Overall sample, old
count if age > 55
* Only EN couples
count if empstat == 1 & empstat_sp == 3 | empstat_sp == 1 & empstat == 3
* Only EN couples, young 
count if empstat == 1 & empstat_sp == 3 & age <= 35 | empstat_sp == 1 & empstat == 3 & age <= 35
* Only EN couples, old 
count if empstat == 1 & empstat_sp == 3 & age > 55  | empstat_sp == 1 & empstat == 3 & age > 55
restore


// Asset profile
preserve
* collapse to one observation per household and year // USED AS MODEL TARGET (8 April 2024)
collapse (mean) netliqu_own_real persweight age, by(panel suid year)

* Asset levels cross section (net liquid wealth own definition)
sum netliqu_own_real  [aw = persweight], det
sum netliqu_own_real  [aw = persweight] if age <= 35, det
sum netliqu_own_real  [aw = persweight] if age > 35 & age <= 45, det
sum netliqu_own_real  [aw = persweight] if age > 45 & age <= 55, det
sum netliqu_own_real  [aw = persweight] if age > 55 & age <= 65, det

sum netliqu_own_real, det
sum netliqu_own_real if age <= 35, det
sum netliqu_own_real if age > 35 & age <= 45, det
sum netliqu_own_real if age > 45 & age <= 55, det
sum netliqu_own_real if age > 55 & age <= 65, det
restore

// transitions into non-employment by income 

* aggregate transitions
tab trans_ind   [aw = persweight] if  L.empstat == 1 & L.ind_small == 0

* HH = 12 (by income cutoffs) // USED AS MODEL TARGET (25 January 2024)
foreach num of numlist 0/11 {
quietly sum totpersearn_month_real_pos if ind_small == 0 [aw = persweight]
local cutoff (r(max)-(r(min)))/12
di `num'+1
di r(min) + `num' * `cutoff'
di r(min) + (`num'+1) * `cutoff'
tab trans_ind [aw = persweight] if L.empstat == 1 & L.totpersearn_month_real_pos > r(min) + `num' * `cutoff' & L.totpersearn_month_real_pos <= r(min) + (`num'+1) * `cutoff'
}

****************************************************************************
* Joint Initial Distribution: Joint Labor Market States, Income, and Assets
****************************************************************************

* college variable
gen college = 1 if educ >= 44 & educ != .
replace college = 0 if educ < 44 & educ > -1 & educ != .
replace college = 1 if educ == 43 & panel == 2014
label var college "College degree dummy"
label define vcollege 0 "no" 1 "yes"
label value college vcollege

rangestat empstat_sp = empstat, by(year month suid) interval(persid spouseid spouseid) 			// mean of empstat within (year, month, sample unit), where persid is equal to spouseid
rangestat college_sp = college, by(year month suid) interval(persid spouseid spouseid) 			// mean of age within (year, month, sample unit), where persid is equal to spouseid

// 1. Fix distribution across joint labor market states

// 2. Find average earnings by college and spousal college (independent of LS)
bysort college college_sp: sum totpersearn_month_real_pos [weight = persweight] if ind_small == 0 & age <= 30, det

// 3. By joint LS: find college/spousal college distribuiton. Then, by joint LS and college/non-college: find average assets

// EE
tab college college_sp [aw = persweight] if age <= 30 & empstat == 1 & empstat_sp == 1, cell nofreq
bysort college college_sp: sum netliqu_own_real  [weight = persweight] if age <= 30 & empstat == 1 & empstat_sp == 1, det

// EU
tab college college_sp [aw = persweight] if age <= 30 & empstat == 1 & empstat_sp == 2,  cell nofreq
bysort college college_sp: sum netliqu_own_real  [weight = persweight] if age <= 30 & empstat == 1 & empstat_sp == 2, det

// EN
tab college college_sp [aw = persweight] if  age <= 30 & empstat == 1 & empstat_sp == 3,  cell nofreq
bysort college college_sp: sum netliqu_own_real  [weight = persweight] if age <= 30 & empstat == 1 & empstat_sp == 3, det

// UU
tab college college_sp [aw = persweight] if age <= 30 & empstat == 2 & empstat_sp == 2, cell nofreq
bysort college college_sp: sum netliqu_own_real  [weight = persweight] if age <= 30 & empstat == 2 & empstat_sp == 2, det

// UN
tab college college_sp [aw = persweight] if age <= 30 & empstat == 2 & empstat_sp == 3, cell nofreq
bysort college college_sp: sum netliqu_own_real  [weight = persweight] if age <= 30 & empstat == 2 & empstat_sp == 3, det

// NN
tab college college_sp [aw = persweight] if age <= 30 & empstat == 3 & empstat_sp == 3, cell nofreq
bysort college college_sp: sum netliqu_own_real [weight = persweight] if age <= 30 & empstat == 3 & empstat_sp == 3, det

drop empstat_sp college_sp

********************************************************************************
* Prepare control variables
********************************************************************************

* Female indicator (1 yes, 0 no)
gen female = 1 if sex == 2
replace female = 0 if sex == 1
label var female "Female dummy"
label define vfemale 0 "male" 1 "female"
label value female vfemale

* Race
replace race = 0 if race == 2 | race == 3 | race == 4
label var race "Race"

* Children
gen numchild = totchild18 if totchild18 < 3
replace numchild = 3 if totchild18 >= 3 & totchild18 != .
label var numchild "no. of children (censored at 3)"

* State
* For 1996 and 2001 (Maine,Vermont) and (North Dakota, South Dakota, Wyoming) are only available as groups
* Individually available in later years
* For comparability, also group them in later years
replace state = 61 if state == 23 | state == 50
replace state = 62 if state == 38 | state == 46 | state == 56

********************************************************************************
* Copy spousal variables to individual
********************************************************************************

* id of spouse
rangestat id_sp = id, by(year month suid) interval(persid spouseid spouseid) 			// mean of id within (year, month, sample unit), where persid is equal to spouseid

* Spousal employment status
rangestat empstat_sp = empstat, by(year month suid) interval(persid spouseid spouseid) 	// mean of empstat within (year, month, sample unit), where persid is equal to spouseid

* Spousal labor market transitions
rangestat trans_sp = trans_ind, by(year month suid) interval(persid spouseid spouseid)	// mean of transind within (year, month, sample unit), where persid is equal to spouseid

* Spousal age
rangestat age_sp = age, by(year month suid) interval(persid spouseid spouseid) 			// mean of age within (year, month, sample unit), where persid is equal to spouseid

* Spouse collge
rangestat college_sp = college, by(year month suid) interval(persid spouseid spouseid) 			// mean of age within (year, month, sample unit), where persid is equal to spouseid


********************************************************************************
* Added Worker Effect Regressions
********************************************************************************

sort id time
gen spouse_out = 0 if trans_sp == 33
replace spouse_out = 1 if (trans_sp == 31 | trans_sp == 32)
label var spouse_out "Pr(Spouse enters LF)"

gen head_switch = 0 if trans_ind == 11
replace head_switch = 1 if trans_ind == 12 
label var head_switch "Head switches from E $\rightarrow$ U"

// Age groups
gen age_group = .
replace age_group = 1 if age <= 35
replace age_group = 2 if age > 35 & age <= 45
replace age_group = 3 if age > 45 & age <= 55
replace age_group = 4 if age > 55

gen age_group_sp = .
replace age_group_sp = 1 if age_sp <= 35
replace age_group_sp = 2 if age_sp > 35 & age_sp <= 45
replace age_group_sp = 3 if age_sp > 45 & age_sp <= 55
replace age_group_sp = 4 if age_sp > 55


********************************************************************************
* REGRESSIONS
********************************************************************************

// Comparison with CPS -- Pooled Sample, Same Regression as in CPS

reg spouse_out F2.head_switch i.month i.state i.year i.sex i.race i.numchild i.college i.college_sp [aw = persweight], r
estimates store F2
outreg2 using "regression_sipp", tex replace drop(i.month i.state i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lead)
reg spouse_out F.head_switch  i.month i.state i.year i.sex i.race i.numchild i.college i.college_sp [aw = persweight], r
estimates store F1
outreg2 using "regression_sipp", tex append drop(i.month i.state i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lead)
reg spouse_out head_switch i.month i.state i.year i.sex i.race i.numchild i.college i.college_sp [aw = persweight], r
estimates store L0
outreg2 using "regression_sipp", tex append drop(i.month i.state i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Contemporaneous)
reg spouse_out L.head_switch i.month i.state i.year i.sex i.race i.numchild i.college i.college_sp [aw = persweight], r
estimates store L1 
outreg2 using "regression_sipp", tex append drop(i.month i.state i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(One month lag)
reg spouse_out L2.head_switch i.month i.state i.year i.sex i.race i.numchild i.college i.college_sp [aw = persweight], r
estimates store L2
outreg2 using "regression_sipp", tex append drop(i.month i.state i.year) bdec(3) sdec(3) label addtext(Month dummies, YES, Year dummies, YES, State dummies, YES) ctitle(Two month lag)

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(F2.head_switch F.head_switch head_switch L.head_switch L2.head_switch)  xtitle("{&beta} - coefficient with CI", size(vlarge)) ///
 ytitle("Head loses job ... ", size(vlarge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(vlarge)) ylabel(,labsize(vlarge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(large) format(%9.2g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(F2.head_switch = "in two months" F.head_switch = "next month" head_switch = "this month" L.head_switch = "last month" L2.head_switch = "two months ago")  
graph export "directreg_leadslags_SIPP.pdf", replace

********************************************************************************
* AWE Tables 
********************************************************************************

sort id time

// Baseline

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp [aw = persweight]
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp [aw = persweight]
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp [aw = persweight]
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp

* baseline transitions of houshold head
tab trans_ind if L.empstat == 1 & L.empstat_sp == 3 & id_sp == L.id_sp  [aw = persweight]


// By Net Liquid Wealth OWN DEFINITION (Halves) ---  TABLE 3 IN PAPER ---

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1 [aw = persweight]
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2 [aw = persweight]
tab trans_sp if trans_ind == 11 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1 [aw = persweight]
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2 [aw = persweight]
tab trans_sp if trans_ind == 12 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1 [aw = persweight]
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2 [aw = persweight]
tab trans_sp if trans_ind == 13 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2

* baseline transitions of houshold head
tab trans_ind if L.empstat == 1 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 1 [aw = persweight]
tab trans_ind if L.empstat == 1 & L.empstat_sp == 3 & id_sp == L.id_sp & netliqu_own_real_half == 2 [aw = persweight]

* Are the results by net liquid wealth different from each other?
preserve
estimates clear

reg spouse_out head_switch if netliqu_own_real_half == 1 [aw = persweight], r
estimates store A
reg spouse_out head_switch if netliqu_own_real_half == 2 [aw = persweight], r
estimates store B

coefplot (A,  msymbol(x) msize(large) mcolor(black) ciopts(recast(rcap) color(black))  mlabsize(large)  mlabposition(12) mlabgap(*2) mlabcolor(black)) ///
	     (B,  msymbol(x) msize(large) mcolor(maroon) ciopts(recast(rcap) color(maroon)) mlabsize(large) mlabposition(12) mlabgap(*2) mlabcolor(maroon)), ///
 nolabel keep(head_switch)  xtitle("{&beta} - coefficient with CI", size(vlarge)) ///
 ytitle("Added Worker Effect", size(vlarge))  mlabel format(%9.2g) ///
 xlabel(-0.02(0.02).1,labsize(vlarge)) ylabel(,labsize(vlarge)) ///
 graphregion(color(white)) xline(0, lc(maroon))  ///
 coeflabels(head_switch = " ") legend(label(2 "Bottom 50%") label(4 "Top 50%") size(large)) ///
 note("Bottom 50%: Sample size is 400,692, R2 is  0.0007. Top 50%: Sample size is 324,225, R2 is  0.0002.")
graph export "significance_netwealth_SIPP.pdf", replace

restore



********************************************************************************
********************* AGGREGATION OF FREQUENCY *********************************
********************************************************************************
 
* head
bysort id wave panel empstat: gen aux = _n == 1 
bysort id wave panel: egen nvals_empstat = sum(aux)
drop aux

*spouse
bysort id wave panel empstat_sp: gen aux = _n == 1 
bysort id wave panel: egen nvals_empstat_sp = sum(aux)
drop aux

/*
. tab nvals_empstat

nvals_empst |
         at |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |  5,630,985       95.70       95.70
          2 |    239,979        4.08       99.78
          3 |     12,880        0.22      100.00
------------+-----------------------------------
      Total |  5,883,844      100.00


	  
. tab nvals_empstat_sp

nvals_empst |
      at_sp |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |  5,623,941       95.58       95.58
          2 |    247,314        4.20       99.79
          3 |     12,585        0.21      100.00
          4 |          4        0.00      100.00
------------+-----------------------------------
      Total |  5,883,844      100.00


--> given small number of within wave transitions: assign employment status with highest frequency 

*/

drop nvals*

// aggregate employment status == LFS with highest frequency 
// comment: results somewhat sensitive on whether we use minmode or maxmode. 

bysort id wave panel: egen empstat_agg    = mode(empstat), minmode  
bysort id wave panel: egen empstat_sp_agg = mode(empstat_sp), minmode



*********************** AGGREGATE AWE *****************************


// open new frame to work with aggregate data 

save monthly_data.dta, replace

frame create agg_dataframe
frame change agg_dataframe

use monthly_data, clear
erase monthly_data.dta

// collapse to aggregate level 

collapse empstat_agg empstat_sp_agg panel netliqu_own_real id_sp persweight (max) age_sp, by(id wave) // results not sensitive to use min or max age_sp

xtset id wave

xtile netliqu_own_half = netliqu_own_real, nq(2)
label var netliqu_own_half "Halves of the net liquid asset distribution own definition"



// Labor Market Transitions on Aggregate Level 

* head
gen trans_ind_agg = .
label var trans_ind_agg "Aggregated individual labor market transitions"
replace trans_ind_agg = 11 if empstat_agg == 1 & L.empstat_agg == 1	// EE transition
replace trans_ind_agg = 12 if empstat_agg == 2 & L.empstat_agg == 1 // EU transition
replace trans_ind_agg = 13 if empstat_agg == 3 & L.empstat_agg == 1 // EO transition
replace trans_ind_agg = 21 if empstat_agg == 1 & L.empstat_agg == 2	// UE transition
replace trans_ind_agg = 22 if empstat_agg == 2 & L.empstat_agg == 2 // UU transition
replace trans_ind_agg = 23 if empstat_agg == 3 & L.empstat_agg == 2 // UO transition
replace trans_ind_agg = 31 if empstat_agg == 1 & L.empstat_agg == 3	// OE transition
replace trans_ind_agg = 32 if empstat_agg == 2 & L.empstat_agg == 3 // OU transition
replace trans_ind_agg = 33 if empstat_agg == 3 & L.empstat_agg == 3 // OO transition

* spouse 
gen trans_sp_agg = .
label var trans_sp_agg "Aggregated individual labor market transitions"
replace trans_sp_agg = 11 if empstat_sp_agg == 1 & L.empstat_sp_agg == 1 // EE transition
replace trans_sp_agg = 12 if empstat_sp_agg == 2 & L.empstat_sp_agg == 1 // EU transition
replace trans_sp_agg = 13 if empstat_sp_agg == 3 & L.empstat_sp_agg == 1 // EO transition
replace trans_sp_agg = 21 if empstat_sp_agg == 1 & L.empstat_sp_agg == 2 // UE transition
replace trans_sp_agg = 22 if empstat_sp_agg == 2 & L.empstat_sp_agg == 2 // UU transition
replace trans_sp_agg = 23 if empstat_sp_agg == 3 & L.empstat_sp_agg == 2 // UO transition
replace trans_sp_agg = 31 if empstat_sp_agg == 1 & L.empstat_sp_agg == 3 // OE transition
replace trans_sp_agg = 32 if empstat_sp_agg == 2 & L.empstat_sp_agg == 3 // OU transition
replace trans_sp_agg = 33 if empstat_sp_agg == 3 & L.empstat_sp_agg == 3 // OO transition

// Definition for Regressions
gen spouse_out = 0 if trans_sp_agg == 33
replace spouse_out = 1 if (trans_sp_agg == 31 | trans_sp_agg == 32)
label var spouse_out "Pr(Spouse enters LF)"

gen head_switch = 0 if trans_ind_agg == 11
replace head_switch = 1 if trans_ind_agg == 12 
label var head_switch "Head switches from E $\rightarrow$ U"


// By Net Liquid Wealth (Halves) -- OWN DEFINITION -- TABLE C.2 IN PAPER

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 11 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 11 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 11 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 11 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 12 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 12 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 12 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 12 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 13 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 13 & L.empstat_sp_agg == 3 & netliqu_own_half == 1 & id_sp == L.id_sp
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_sp_agg if trans_ind_agg == 13 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp [aw = persweight]
tab trans_sp_agg if trans_ind_agg == 13 & L.empstat_sp_agg == 3 & netliqu_own_half == 2 & id_sp == L.id_sp

** baseline transitions of houshold head
tab trans_ind_agg if L.empstat_agg == 1 & L.empstat_sp_agg == 3 & id_sp == L.id_sp & netliqu_own_half == 1 [aw = persweight]
tab trans_ind_agg if L.empstat_agg == 1 & L.empstat_sp_agg == 3 & id_sp == L.id_sp & netliqu_own_half == 2 [aw = persweight]

* Are the results by net liquid wealth different from each other?
preserve
estimates clear

reg spouse_out head_switch if netliqu_own_half == 1 [aw = persweight], r
estimates store A
reg spouse_out head_switch if netliqu_own_half == 2 [aw = persweight], r
estimates store B

coefplot (A,  msymbol(x) msize(large) mcolor(black) ciopts(recast(rcap) color(black))  mlabsize(large)  mlabposition(12) mlabgap(*2) mlabcolor(black)) ///
	     (B,  msymbol(x) msize(large) mcolor(maroon) ciopts(recast(rcap) color(maroon)) mlabsize(large) mlabposition(12) mlabgap(*2) mlabcolor(maroon)), ///
 nolabel keep(head_switch)  xtitle("{&beta} - coefficient with CI", size(vlarge)) ///
 ytitle("Added Worker Effect", size(vlarge)) mlabel format(%9.2g) ///
 xlabel(-0.02(0.02).1,labsize(vlarge)) ylabel(,labsize(vlarge)) ///
 graphregion(color(white)) xline(0, lc(maroon))  ///
 coeflabels(head_switch = " ") legend(label(2 "Bottom 50%") label(4 "Top 50%") size(large)) ///
 note("Bottom 50%: Sample size is  77,788, R2 is  0.0010. Top 50%: Sample size is 63,601, R2 is  0.0002.")
graph export "significance_netwealth_SIPP_agg.pdf", replace

restore

frame change default

log close
