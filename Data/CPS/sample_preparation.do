********************************************************************************
* Sample Selection
********************************************************************************

* Restriction on age: >= 26, <= 65
* Impose age restrictions before dealing with spouses because age restrictions may delete one partner but not the other (and we want sample of couples both between 26 and 65)
keep if age >= 26
keep if age <= 65

* Keep only household heads and partners
* 101: Head
* 201: Spouse, 202: Opposite sex spouse, 203: Same sex spouse (202 and 203 supersede 201 in 2020)
* 1114: Unmarried partner, 1116: Opposite sex unmarried partner, 1117: Same sex unmarried partner (1116 and 1117 supersede 1114 in 2020)
* Note: Unmarried partner not available in 1994; instead unmarried partner/roommate category, which cannot be separated; disregarded
keep if relate == 0101 | relate == 0201 | relate == 0202 | relate == 0203 | relate == 1114 | relate == 1116 | relate == 1117

* One household can include more than one couple
* Procedure above keeps some partners, who are not the partner of the heads
* Drop them now
egen n_hh_year_month = count(cpsid), by (serial year month)		// identifies how many people are left in HH
drop if n_hh_year_month == 1									// drop if only 1 person is in household
drop if n_hh_year_month > 2 & sploc == 0						// drop if there are more than 2 people in household and partner of this person is not present
* Still leaves some people whose partner is not in the data anymore/ cannot be identified
drop if sploc == 0
* Check again that we only have couples left
drop n_hh_year_month
egen n_hh_year_month = count(cpsid), by (serial year month)		// identifies how many people are left in HH
drop if n_hh_year_month == 1									// drop if only 1 persopn is in household
tab n_hh_year_month
drop n_hh_year_month
* Now we are down to a dataset of just couples, not all of whom are married

********************************************************************************
* Generate labor market status variables
********************************************************************************

* Labor force status with three states: employed, unemployed, out of the labor force
gen empstat3 = .
label var empstat3 "Employment status (3 states)"
* Employed: Armed forces, employed (at work), employed (not at work last week)
* Note: This could be differentiated further using the information on why someone is employed but not at work
replace empstat3 = 1 if empstat == 01 | empstat == 10 | empstat == 12
* Unemployed: unemployed, unemployed (experienced worker), unemployed (new worker)
replace empstat3 = 2 if empstat == 20 | empstat == 21 | empstat == 22
* Not in labor force: housework, unable to work, school, other, unpaid less than 15 hours, retired
replace empstat3 = 3 if inrange(empstat,30,36)

* Adjustment of implausible flows following Elsby, Hobijn, Sahin (2015)
* First adjust months 3 and 7 if applicable
by cpsidv: gen empstat3_lag = L.empstat3 
by cpsidv: gen empstat3_lag2 = L2.empstat3 
by cpsidv: gen empstat3_lead = F.empstat3 
replace empstat3 = 3 if empstat3_lag2 == 3 & empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3	// NNUN to NNNN
replace empstat3 = 3 if empstat3_lag2 == 1 & empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3 // ENUN to ENNN
replace empstat3 = 3 if empstat3_lag2 == . & empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3	// .NUN to .NNN
replace empstat3 = 2 if empstat3_lag2 == 2 & empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 // UUNU to UUUU
replace empstat3 = 2 if empstat3_lag2 == 1 & empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 // EUNU to EUUU
replace empstat3 = 2 if empstat3_lag2 == . & empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 // .UNU to .UUU
drop empstat3_lag empstat3_lag2 empstat3_lead
* Second adjust months 2 and 6 if applicable
* should be (and is) redundant given above because .NUN to .NNN takes care of first three cases and .UNU to .UUU takes care of last three
by cpsidv: gen empstat3_lag = L.empstat3 
by cpsidv: gen empstat3_lead = F.empstat3 
by cpsidv: gen empstat3_lead2 = F2.empstat3 
replace empstat3 = 3 if empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3 & empstat3_lead2 == 3 // NUNN to NNNN
replace empstat3 = 3 if empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3 & empstat3_lead2 == 1 // NUNE to NNNE
replace empstat3 = 3 if empstat3_lag == 3 & empstat3 == 2 & empstat3_lead == 3 & empstat3_lead2 == . // NUN. to NNN.
replace empstat3 = 2 if empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 & empstat3_lead2 == 2 // UNUU to UUUU
replace empstat3 = 2 if empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 & empstat3_lead2 == 1 // UNUE to UUUE
replace empstat3 = 2 if empstat3_lag == 2 & empstat3 == 3 & empstat3_lead == 2 & empstat3_lead2 == . // UNU. to UUU.
drop empstat3_lag empstat3_lead empstat3_lead2

* Generate new variable for why someone is not in the labor forces
gen nilf_reason = .
label var nilf_reason "Reason for why person is NILF"
replace nilf_reason = 1 if empstat == 32				 // unable to work
replace nilf_reason = 2 if empstat == 36    			 // retired
replace nilf_reason = 3 if empstat == 34 & nilfact == 01 // disabled
replace nilf_reason = 4 if empstat == 34 & nilfact == 02 // ill	
replace nilf_reason = 5 if empstat == 34 & nilfact == 03 // in school
replace nilf_reason = 6 if empstat == 34 & nilfact == 04 // taking care of house or family
replace nilf_reason = 7 if empstat == 34 & nilfact == 06 // other

********************************************************************************
* Add partner variables to person records
********************************************************************************

* New variable: Labor force status of spouse
* requires rangestat program (ssc install rangestat)
isid year month serial pernum, sort															// check that year, month, household serial number, and person number uniquely identify obs and sort
rangestat (mean) spempstat3 = empstat3, by(year month serial) interval(pernum sploc sploc)	// copy to head (spouse) the employment status of the spouse (head)
label var spempstat3 "Employment status of spouse (3 states)"

* Also: Age of spouse
rangestat (mean) age_sp = age, by(year month serial) interval(pernum sploc sploc)			// copy to head (spouse) the age of the spouse (head)
label var age_sp "Age of spouse"

* Also: Education
* First: Generate summary education variable (more than two years of college or degree)
gen college = 0
replace college = 1 if educ >= 111															// college: bachelor's degree, master's degree, professional school degree, doctorate
label var college "College"
rangestat (mean) college_sp = college, by(year month serial) interval(pernum sploc sploc)	// copy to head (spouse) the education of the spouse (head)
label var college_sp "College spouse"

* Create lagged variables for employment status and spouse employment status
xtset cpsidv modate
by cpsidv: gen empstat3_lag = L.empstat3 													// lagged employment status
label var empstat3_lag "Lagged employment status (3 states)"
by cpsidv: gen spempstat3_lag = L.spempstat3 												// lagged employment status of spouse
label var spempstat3_lag "Lagged employment status of spouse (3 states)"

* Reason for not being in the labor force for spouses
rangestat (mean) nilf_reason_sp = nilf_reason, by(year month serial) interval(pernum sploc sploc)	// copy to head (spouse) the reason for being out of the labor force of the spouse (head)
label var nilf_reason_sp "Reason for why person's spouse is NILF"
by cpsidv: gen nilf_reason_sp_lag = L.nilf_reason_sp 												// lagged reason for being out of the labor force of the spouse
label var nilf_reason_sp_lag "Lagged employment status of spouse (3 states)"

********************************************************************************
* Generate transition variables
********************************************************************************

* Generate individual transition variables
gen trans_ind = .
label var trans_ind "Individual labor market transitions"
replace trans_ind = 11 if empstat3 == 1 & empstat3_lag == 1	// EE transition
replace trans_ind = 12 if empstat3 == 2 & empstat3_lag == 1 // EU transition
replace trans_ind = 13 if empstat3 == 3 & empstat3_lag == 1 // EN transition
replace trans_ind = 21 if empstat3 == 1 & empstat3_lag == 2	// UE transition
replace trans_ind = 22 if empstat3 == 2 & empstat3_lag == 2 // UU transition
replace trans_ind = 23 if empstat3 == 3 & empstat3_lag == 2 // UN transition
replace trans_ind = 31 if empstat3 == 1 & empstat3_lag == 3	// NE transition
replace trans_ind = 32 if empstat3 == 2 & empstat3_lag == 3 // NU transition
replace trans_ind = 33 if empstat3 == 3 & empstat3_lag == 3 // NN transition

* Instead of constructing trans_ind_sp using spempstat3 we now directly mirror trans_ind
* Only if the spouse is the same in this and last period

* CPSIDV of spouse
rangestat (mean) cpsidv_sp = cpsidv, by(year month serial) interval(pernum sploc sploc)				// copy to head (spouse) the person id of the spouse (head)
label var cpsidv_sp "CPS ID spouse"
by cpsidv: gen double cpsidv_sp_lag = L.cpsidv_sp													// lagged person id of the spouse
label var cpsidv_sp_lag "Last observation period CPS ID spouse"

* Transition of spouse that was also spouse last period
rangestat (mean) trans_ind_sp = trans_ind if cpsidv_sp == cpsidv_sp_lag, by(year month serial) interval(pernum sploc sploc)	// copy to head (spouse) the labor market transition of the spouse (head)
label var trans_ind_sp "Spouse labor market transitions"

********************************************************************************
* Recode some control variables for regression 
********************************************************************************

// race: code as dummy if white or not
replace race = 1 if race == 100
replace race = 0 if race > 100 & race != .
label var race "=1 if white, zero else"

// top code number of children at 3
replace nchild = 3 if nchild > 3 & nchild != .
label var nchild "num of children, topcoded at 3"

save "$datapath/cps_prepared_sample.dta", replace