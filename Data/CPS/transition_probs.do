log using "$figurepath/transition_probs", replace text

********************************************************************************
* Conditional transition probabilities
********************************************************************************

********************************************************************************
* Entire population
********************************************************************************

* Group: Everybody (weighted) [TABLE 1 / TABLE B1)]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & panlwt != -0.0001 [aw = panlwt]

* Group: Everybody (weighted) (years to be comparable with SIPP) [Table C1]]]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & panlwt != -0.0001 & year >= 1995 & year <= 2016 [aw = panlwt]

* Group: Everybody (weighted); split by reason for unemployment [TABLE B4]
* Transition probabilities of spouse conditional on head EU transition and spouse OLF / layoff
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 1) & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 1) & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF / job loser
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 2) & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 2) & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF / temporary job ended
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 3) & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 3) & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF / quit
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 4) & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 4) & panlwt != -0.0001


* Reasons for unemployment by age
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 2) & panlwt != -0.0001 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (whyunemp == 2) & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]

* Head average flow rates for unemployment by age 
* recode trans_ind to label only job loss in as EU, all others as EN
gen trans_ind_aux = 11 if trans_ind == 11
replace trans_ind_aux = 12 if trans_ind == 12 & whyunemp == 2
replace trans_ind_aux = 13 if trans_ind == 13 | trans_ind == 12 & whyunemp != 2

tab trans_ind_aux if (trans_ind_aux == 11 | trans_ind_aux == 12 | trans_ind_aux == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33)  & panlwt != -0.0001 &  age_sp <= 35 [aw = panlwt]
tab trans_ind_aux if (trans_ind_aux == 11 | trans_ind_aux == 12 | trans_ind_aux == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33)  & panlwt != -0.0001 &  age_sp > 55 [aw = panlwt]

drop trans_ind_aux


* Group: Everybody (weighted) -- spouse U [TABLE B2]
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 2 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 2 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 2 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 21 | trans_ind_sp == 22 | trans_ind_sp == 23) & spempstat3_lag == 2 & panlwt != -0.0001 [aw = panlwt]

* Group: Everybody (weighted) -- spouse E [TABLE B3]
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 11 | trans_ind_sp == 12 | trans_ind_sp == 13) & spempstat3_lag == 1 & panlwt != -0.0001 [aw = panlwt]

* Group: Everybody (weighted) -- spouse E [by age]]
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 11 | trans_ind_sp == 12 | trans_ind_sp == 13) & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp <= 35 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 11 | trans_ind_sp == 12 | trans_ind_sp == 13) & spempstat3_lag == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]

* In Table B.3: What fraction of EN transitions is because they retire? (nilf_reason == 2)
* head EE, spouse EN
tab nilf_reason_sp  if trans_ind == 11 & trans_ind_sp == 13 & panlwt != -0.0001 [aw = panlwt], m
* head EN, spouse EN  
tab nilf_reason_sp  if trans_ind == 13 & trans_ind_sp == 13 & panlwt != -0.0001 [aw = panlwt], m  
tab nilf_reason 	if trans_ind == 13 & trans_ind_sp == 13 & panlwt != -0.0001 [aw = panlwt], m  

* In Table B.3: Split by whether head and spouse are in same industry
// code industry of primary earner and spouse
rangestat (mean) ind_sp = ind, by(year month serial) interval(pernum sploc sploc)			
label var ind_sp "Industry of spouse"

tostring ind, replace
gen ind2 = substr(ind, 1, 1)
destring ind2 ind, replace 

tostring ind_sp, replace
gen ind2_sp = substr(ind_sp, 1, 1)
destring ind2_sp ind_sp, replace 

* share of couples in same industry among added workers,  old vs young  
count if age_sp <=35 
count if age_sp <=35 &  ind2 == ind2_sp & ind2 != 0 

count if age_sp > 55
count if age_sp > 55 &  ind2 == ind2_sp & ind2 != 0 

// AWE
xtset cpsidp modate
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & L.ind2 == L.ind2_sp & L.ind2 != 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse U
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 1 & L.ind2 != L.ind2_sp & L.ind2 != 0 & L.ind2_sp != 0 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 11 | trans_ind_sp == 12 | trans_ind_sp == 13) & spempstat3_lag == 1 & panlwt != -0.0001 & L.ind2 == L.ind2_sp & L.ind2 != 0 [aw = panlwt]

********************************************************************************
* Splits by age
********************************************************************************

* Split by age (of spouse) (weighted) [TABLE 2]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001

* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001

* Head average flow rates for above samples
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp > 35 & age_sp <= 45 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp > 45 & age_sp <= 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp > 55 & age_sp <= 65 & panlwt != -0.0001 [aw = panlwt]

********************************************************************************
* Splits by education
********************************************************************************

* Split by education (of spouse) (weighted) [TABLE 4]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001

* Head average flow rates for above samples
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]

* Split by age and education (of spouse) (weighted) [TABLE B8]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp > 55 & college_sp == 1 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp <= 35 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & age_sp > 55 & college_sp == 0 & panlwt != -0.0001 [aw = panlwt]


********************************************************************************
* Splits by children
********************************************************************************

* Split by children (weighted), restricted to younger than 40 [TABLE B6]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nchild == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nchild > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]

* Split by young children (weighted), restricted to under 40 [TABLE B6]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EO transition and spouse OLF
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nchlt5 == 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]
* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nchlt5 > 0 & age_sp < 40 & panlwt != -0.0001 [aw = panlwt]

********************************************************************************
* Splits by reasons for being out of the labor force
********************************************************************************

* Exclude retired, by age (weighted) [TABLE B7]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without retired, young
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without retired, young
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without retired, young
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without retired, old
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without retired, old
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without retired, old
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nilf_reason_sp_lag != 2  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]

* Exclude disabled/ill, by age (weighted) [TABLE B7]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without disabled/ill, young
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4 & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without disabled/ill, young
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without disabled/ill, young
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without disabled/ill, old
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without disabled/ill, old
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4 & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4 & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without disabled/ill, old
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp > 55 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4 & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & nilf_reason_sp_lag != 3  & nilf_reason_sp_lag != 4  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]

* Exclude retired and disabled/ill, by age (weighted) [TABLE B7]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without retired or disabled/ill, young
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4) & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4) & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without retired or disabled/ill, young
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without retired or disabled/ill, young
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp <= 35 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- without retired or disabled/ill, old
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- without retired or disabled/ill, old
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001
* Transition probabilities of spouse conditional on head EN transition and spouse OLF -- without retired or disabled/ill, old
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4) & age_sp <= 35 & panlwt != -0.0001 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & (nilf_reason_sp_lag == 1 | nilf_reason_sp_lag > 4)  & age_sp > 55 & panlwt != -0.0001 [aw = panlwt]

********************************************************************************
* Splits by gender
********************************************************************************

* Split by gender (weighted) -- young [TABLE B5]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EO transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EO transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]

* Split by gender (weighted) -- old [TABLE B5]
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EE transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EO transition and spouse OLF -- primary men
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EO transition and spouse OLF -- primary women 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & sex == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & sex == 2 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]

********************************************************************************
* Splits by cohort
********************************************************************************

* Split by cohort and age (weighted) [TABLE B5]
gen birthy = year - age 
* Transition probabilities of spouse conditional on head EE transition and spouse OLF 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EE transition and spouse OLF 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt != -0.0001 & age_sp > 55

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & birthy >= 1960 & birthy <= 1970 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]

********************************************************************************
* Splits by business cycle
********************************************************************************

* Split by state of the business cycle (weighted) [TABLE B9]
* recession dummy -- NBER Recesssions
gen rec = 1 if year == 2001 & month > =3  & month <=11
replace rec = 1 if year == 2007  & month == 12 | year == 2008 | year == 2009 & month <= 6
replace rec = 1 if year == 2020 & month >=  2
replace rec = 0 if rec != 1

* Transition probabilities of spouse conditional on head EE transition and spouse OLF [TABLE B9]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EE transition and spouse OLF 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 1 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & rec == 1 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]

* Transition probabilities of spouse conditional on head EE transition and spouse OLF [TABLE B9]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EE transition and spouse OLF 
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 11 & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EU transition and spouse OLF 
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 12 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp > 55
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp >= 26 & age_sp <= 35
* Transition probabilities of spouse conditional on head EO transition and spouse OLF 
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp > 55 [aw = panlwt]
tab trans_ind_sp if trans_ind == 13 & spempstat3_lag == 3 & rec == 0 & panlwt != -0.0001 & age_sp > 55

* Head average flow rates for above sample
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp >= 26 & age_sp <= 35 [aw = panlwt]
tab trans_ind if (trans_ind == 11 | trans_ind == 12 | trans_ind == 13) & (trans_ind_sp == 31 | trans_ind_sp == 32 | trans_ind_sp == 33) & spempstat3_lag == 3 & rec == 0 & panlwt!= -0.0001 & age_sp > 55 [aw = panlwt]

log close
