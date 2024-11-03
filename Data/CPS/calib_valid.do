log using "$figurepath/calib_valid", replace text

********************************************************************************
* Joint Labor Market States
********************************************************************************

* Generate joint labor market states
gen LS_joint = .
label var LS_joint "Joint labor market state"
label define LS_joint_lab 1 "EE" 2 "EU" 3 "UE" 4 "EN" 5 "NE" 6 "UU" 7 "UN" 8 "NU" 9 "NN"
label values LS_joint LS_joint_lab

replace LS_joint = 1 if empstat3 == 1 & spempstat3 == 1		// EE
replace LS_joint = 2 if empstat3 == 1 & spempstat3 == 2		// EU
replace LS_joint = 3 if empstat3 == 2 & spempstat3 == 1		// UE
replace LS_joint = 4 if empstat3 == 1 & spempstat3 == 3		// EN
replace LS_joint = 5 if empstat3 == 3 & spempstat3 == 1		// NE
replace LS_joint = 6 if empstat3 == 2 & spempstat3 == 2		// UU
replace LS_joint = 7 if empstat3 == 2 & spempstat3 == 3		// UN
replace LS_joint = 8 if empstat3 == 3 & spempstat3 == 2		// NU
replace LS_joint = 9 if empstat3 == 3 & spempstat3 == 3		// NN

* For initial distribution
tab LS_joint if age <= 30 & relate == 0101 [aw = hwtfinl]

* Combine symmetric states
gen LS_joint_comb = .
label var LS_joint_comb "Joint labor market state combined"
label define LS_joint_comb_lab 1 "EE" 2 "EU/UE" 3 "EN/NE" 4 "UU" 5 "UN/NU" 6 "NN"
label values LS_joint_comb LS_joint_comb_lab 

replace LS_joint_comb = 1 if LS_joint == 1
replace LS_joint_comb = 2 if LS_joint == 2 | LS_joint == 3
replace LS_joint_comb = 3 if LS_joint == 4 | LS_joint == 5
replace LS_joint_comb = 4 if LS_joint == 6
replace LS_joint_comb = 5 if LS_joint == 7 | LS_joint == 8
replace LS_joint_comb = 6 if LS_joint == 9

* Total population
tab LS_joint_comb if relate == 0101 [aw = hwtfinl]
tab LS_joint_comb if relate == 0101

* By age groups [FIGURE 5]
tab LS_joint_comb if age >= 25 & age <= 35 & relate == 0101 [aw = hwtfinl]
tab LS_joint_comb if age > 35 & age <= 45 & relate == 0101 [aw = hwtfinl]
tab LS_joint_comb if age > 45 & age <= 55 & relate == 0101 [aw = hwtfinl]
tab LS_joint_comb if age > 55 & age <= 65 & relate == 0101 [aw = hwtfinl]

tab LS_joint_comb if age >= 25 & age <= 35 & relate == 0101 
tab LS_joint_comb if age > 35 & age <= 45 & relate == 0101 
tab LS_joint_comb if age > 45 & age <= 55 & relate == 0101 
tab LS_joint_comb if age > 55 & age <= 65 & relate == 0101 

********************************************************************************
* Individual transition probabilities
********************************************************************************

* Full population [TABLE 6]
tab trans_ind if empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if empstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if empstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

tab trans_ind if empstat3_lag == 1 & panlwt != -0.0001 
tab trans_ind if empstat3_lag == 2 & panlwt != -0.0001 
tab trans_ind if empstat3_lag == 3 & panlwt != -0.0001

* By age group [TABLE 9]
tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 1 & panlwt != -0.0001 
tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 2 & panlwt != -0.0001 
tab trans_ind if age > 25 & age <= 35 & empstat3_lag == 3 & panlwt != -0.0001 

tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 1 & panlwt != -0.0001 
tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 2 & panlwt != -0.0001 
tab trans_ind if age > 35 & age <= 45 & empstat3_lag == 3 & panlwt != -0.0001 

tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 1 & panlwt != -0.0001 
tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 2 & panlwt != -0.0001 
tab trans_ind if age > 45 & age <= 55 & empstat3_lag == 3 & panlwt != -0.0001 

tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 1 & panlwt != -0.0001 
tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 2 & panlwt != -0.0001 
tab trans_ind if age > 55 & age <= 65 & empstat3_lag == 3 & panlwt != -0.0001

********************************************************************************
* Men and women as primary earners
********************************************************************************

bysort year: tab sex if LS_joint == 4 [aw = hwtfinl]

********************************************************************************
* Share of NE couples over time 
********************************************************************************

bysort year: tab LS_joint [aw = hwtfinl]

********************************************************************************
* Indivdiual EU rates over time 
********************************************************************************

bysort year: tab trans_ind if empstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]

log close