clear
cls

insheet using head_eu_vec_cont_4.csv
rename v1 head_switch3
destring head_switch3, replace force
save "aux_file.dta", replace
clear
insheet using spouse_nl_vec_cont_4.csv
rename v1 spouse_out
destring spouse_out, replace force
merge using "aux_file.dta"

reg spouse_out head_switch3
estimates store L0

clear
insheet using head_eu_vec_lead2_4.csv
rename v1 head_switch1
destring head_switch1, replace force
save "aux_file.dta", replace
clear
insheet using spouse_nl_vec_lead2_4.csv
rename v1 spouse_out
destring spouse_out, replace force
merge using "aux_file.dta"

reg spouse_out head_switch1
estimates store F2

clear
insheet using head_eu_vec_lead1_4.csv
rename v1 head_switch2
destring head_switch2, replace force
save "aux_file.dta", replace
clear
insheet using spouse_nl_vec_lead1_4.csv
rename v1 spouse_out
destring spouse_out, replace force
merge using "aux_file.dta"

reg spouse_out head_switch2
estimates store F1

clear
insheet using head_eu_vec_lag2_4.csv
rename v1 head_switch5
destring head_switch5, replace force
save "aux_file.dta", replace
clear
insheet using spouse_nl_vec_lag2_4.csv
rename v1 spouse_out
destring spouse_out, replace force
merge using "aux_file.dta"

reg spouse_out head_switch5
estimates store L2

clear
insheet using head_eu_vec_lag1_4.csv
rename v1 head_switch4
destring head_switch4, replace force
save "aux_file.dta", replace
clear
insheet using spouse_nl_vec_lag1_4.csv
rename v1 spouse_out
destring spouse_out, replace force
merge using "aux_file.dta"

reg spouse_out head_switch4
estimates store L1

coefplot (F2) (F1) (L0) (L1) (L2), ///
 nolabel legend(off) keep(head_switch1 head_switch2 head_switch3 head_switch4 head_switch5)  xtitle("{&beta} - coefficient with CI", size(huge)) ///
 ytitle("Head loses job ... ", size(huge)) msymbol(x x x x x) mlcolor(black black black black black) ///
 xlabel(-0.02(0.02).1,labsize(huge)) ylabel(,labsize(huge)) ///
 graphregion(color(white)) xline(0, lc(maroon)) ciopts(recast(rcap) color(black)) mlabel mlabsize(huge) format(%6.3g) mlabposition(12) mlabgap(*2) mlabcolor(black) ///
 coeflabels(head_switch1 = "in two months" head_switch2 = "next month" head_switch3 = "this month" head_switch4 = "last month" head_switch5 = "two months ago") 
