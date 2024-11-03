********************************************************************************
********************************************************************************
* Joint Search over the Life Cycle
* SIPP data preparation
********************************************************************************
********************************************************************************


**************************************************************
************************* 1996 Panel *************************
**************************************************************

cd ""

* Save 1996 panel core data files in Stata format
foreach num of numlist 1/12 {
	do sip96l`num'
	log close
	save "sip96w`num'.dta", replace
	clear
}

* Combine 1996 core data files to one dataset
use "sip96w1.dta", clear
foreach num of numlist 2/12 {
	append using "sip96w`num'.dta"
	erase "sip96w`num'.dta"
}

save "sip96.dta", replace
erase "sip96w1.dta"


*** keeping only relevant variables ***

keep ssuid epppnum swave srefmon rhcalmn rhcalyr eentaid eeducate ///
	 elayoff elkwrk ems epnspous erace ersend1 ersend2 ersnowrk esex epdjbthn ///
	 rfnkids rfownkid rfoklt18 rmesr rmhrswk rmwklkg rmwksab rmwkwjb rnotake rtakjob rwkesr1  /// 
	 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rwksperm spanel tage tbyear tejdate1 tejdate2 tfearn  ///
	 thtotinc tpearn tpmsum1 tpmsum2 tpprpinc tptotinc tsjdate1 tsjdate2 whfnwgt wpfinwgt shhadid ///
	 tfipsst tjbocc1 tjbocc2 ejbind1 ejbind2 etenure
	 
*** renaming variables ***
	 
rename ssuid    suid 						// sample unit identifier
rename epppnum  persid 						// person number
rename swave    wave						// wave of data collection
rename srefmon  refmon						// reference month of this record
rename rhcalmn  month						// calendar month for this reference month
rename rhcalyr  year						// calendar year for this reference month
rename eentaid  entadrid					// address id of household where person entered sample
rename eeducate educ						// education: highest degree received
rename elayoff  layoffdum					// spent time on layoff (dummy)
rename elkwrk   lkworkdum					// spent time looking for work (dummy)
rename ems      marital						// marital status
rename epnspous spouseid 					// person number of spouse
rename erace    race 						// race
rename ersend1  whystopwork1				// main reason stopped working
rename ersend2  whystopwork2				// main reason stopped working
rename ersnowrk whynowork					// main reason for not working during reference period
rename esex     sex							// sex
rename epdjbthn paidjobdum					// paid job during the reference period (dummy)			
rename rfnkids  totchild18					// total number of children under 18 in family
rename rfownkid	ownchild					// number of own children in family
rename rfoklt18 ownchild18					// number of own children under 18 in family
rename rmesr    empstatmonth				// employment status recode for month
rename rmhrswk  hourswrk					// usual hours worked per week recode in month
rename rmwklkg  weekslookwork				// number of weeks looking for work/on layoff in month
rename rmwksab  weeksabsent					// number of weeks absent without pay from job in month
rename rmwkwjb  weeksjob					// number of weeks with a job in month
rename rtakjob  couldstartjob				// could have started a job (dummy)
rename rnotake  whynotake					// why couldn't have started new job
rename rwkesr1  empstatwk1					// employment status recode for week 1
rename rwkesr2  empstatwk2					// employment status recode for week 2
rename rwkesr3  empstatwk3					// employment status recode for week 3
rename rwkesr4  empstatwk4					// employment status recode for week 4
rename rwkesr5  empstatwk5					// employment status recode for week 5
rename rwksperm weeksmonth					// number of weeks in this month
rename spanel   panel						// panel year				
rename shhadid  hhadressid  				// household address id
rename tage     age							// age as of last birthday
rename tbyear   birthyr						// year of birth
rename tejdate1 enddatejob1					// ending date of job YYYYMMDD
rename tejdate2 enddatejob2					// ending date of job YYYYMMDD
rename tfearn   totfamearn					// total family earned income for this month
rename thtotinc tothhinc					// total household income
rename tpearn   totpersearn					// total person's earned income for the reference month
rename tpmsum1  jobearn1					// earnings from job received in this month
rename tpmsum2  jobearn2					// earnings from job received in this month
rename tpprpinc propassetinc				// total property asset income
rename tptotinc totpersinc					// total person's income for the reference month
rename tsjdate1 startdatejob1				// starting date of job YYYYMMDD
rename tsjdate2 startdatejob2				// starting date of job YYYYMMDD
rename whfnwgt  hhweight					// household weight					
rename wpfinwgt persweight					// person weight for household reference person
rename tfipsst state						// state
rename tjbocc1 occupation1					// occupation
rename tjbocc2 occupation2					// occupation
rename ejbind1 industry1					// industry
rename ejbind2 industry2					// industry
rename etenure ownhomedum					// home ownership

save "sip96_adj.dta", replace  

//////////////////////////////
/// topical module: assets ///
//////////////////////////////

* Save 1996 panel topical data files with asset information in Stata format
foreach num of numlist 3(3)12 {
	do sip96t`num'
	log close
	save "sip96t`num'.dta", replace
	clear
}

* Combine topical modules
use "sip96t3.dta", clear
foreach num of numlist 6(3)12 {
	append using "sip96t`num'.dta"
	erase "sip96t`num'.dta"
}

save "sip96_assets.dta", replace
erase "sip96t3.dta"

*** keeping only relevant variables ***

keep ealidal ealidao ealidab ealowa taljcha talicha talsbv ///
     ealjdab ealjdal talrb talkb taltb talliev talliv ///
	 evbow1 evbow2 tvbde1 tvbde2 tvbva1 tvbva2 ///
	 timja tiajta tiaita timia emip emjp eoaeq tov2amt ///
	 tov1val tov2val tov1amt ta1amt ta2amt ta3amt tmhval  ///
	 tmhpr tcarval1 tcarval2 tcarval3 tpropval thhbeq  ///
	 thhira thhotast tothreva thhore rhhstk tmor1amt thhtheq ///
	 thhintbk thhintot thhvehcl tmor1pr thhmortg thhtnw ///
	 rhhuscbt thhtwlth thhdebt thhscdbt trtsha ///
	 trjmv trimv trtmv trtpri trjpri tripri ealjdao ///
	 esmjmav esmimav esmjv esmiv ssuid epppnum eentaid swave


*** renaming variables ***	 

/* naming convention of first two letters: 
	d - debt, a - asset, i - indvidual, j - joint */
	
* identifier variables
rename ssuid    suid 
rename epppnum  persid 
rename eentaid  entadrid
rename swave    wave

* checking accounts 
rename talicha  ai_checking
rename taljcha  aj_checking

* saving accounts
rename tiajta   aj_savacc
rename tiaita   ai_savacc

* loans to bank
rename ealidal  di_loanbank
rename ealjdal  dj_loanbank

* credit card debt
rename ealidab  di_ccardbill
rename ealjdab  dj_ccardbill

* retirement accounts
rename talrb    ai_IRA
rename talkb    ai_KEOGH
rename taltb    ai_401k

* business 
rename evbow1   ai_bus1_share
rename evbow2   ai_bus2_share
rename tvbva1   ai_valbus1_befdebt
rename tvbva2   ai_valbus2_befdebt
rename ealowa   ai_businesssale

rename tvbde1   di_bus1
rename tvbde2   di_bus2      

* mortages
rename emip     di_mortage
rename emjp     dj_mortage 

rename tmor1pr  dj_mortgs_loans_all
rename tmor1amt dj_frstscnd_mortg_orig

* property (??) and other real estate
rename tpropval aj_propertyvalue
rename tothreva aj_equ_othrealest

* rental property not attached to residence 
rename trjmv 	aj_rentprop
rename trtmv    aj_rentprop_nospous_totval
rename trimv    ai_rentprop
rename trtsha   aj_rentprop_nospous_sharval

rename trjpri   dj_rentprop
rename trtpri   dj_rentprop_nospouse
rename tripri   di_rentprop

* mobile home 
rename tmhval   aj_mobile
rename tmhpr    dj_mobile

* vehicles 
rename tcarval1 aj_vec1
rename tcarval2 aj_vec2
rename tcarval3 aj_vec3
rename tov1val  aj_othvec1
rename tov2val  aj_othvec2

rename ta1amt   dj_vec1
rename ta2amt   dj_vec2
rename ta3amt   dj_vec3
rename tov1amt  dj_othvec1
rename tov2amt  dj_othvec2

* bonds 
rename timja    aj_bonds
rename timia    ai_bonds
rename talsbv   ai_savbond

* stocks and mutual funds
rename esmjv    aj_stocks_mutfnds
rename esmiv    ai_stocks_mutfnds

rename esmjmav  dj_stocks_mutfnds
rename esmimav  di_stocks_mutfnds

* life insurance 
rename talliev  ai_lifins_empl
rename talliv   ai_lifins_all

* other assets & debt 
rename eoaeq    ai_equ_othass

rename ealidao  di_othdebt
rename ealjdao  dj_othdebt

* recoded variables (provideded by SIPP)
rename thhtnw   aj_networth_recod
rename thhtwlth aj_wealth_recod
rename thhtheq  aj_homeequity_recod 
rename thhbeq   aj_busequity_recod
rename thhvehcl aj_netequ_vec_recod
rename thhotast aj_equ_othass_recod
rename thhira   aj_IRA_KEOGH_recod
rename thhore   aj_equ_realest_nohome_recod
rename thhintbk aj_intrstass_bank_recod
rename thhintot aj_intrstass_oth_recod
rename rhhstk   aj_stocks_mutfnds_recod

rename thhdebt  dj_total_recod
rename thhmortg dj_home_recod
rename rhhuscbt dj_unsec_recod
rename thhscdbt dj_sec_recod


*label var ealidal "amt loans from bank (no car, no home) in own name"
*label var ealidao "amt other debt (not yet mentioned) in own name"
*label var ealowa  "amt owed to you for sale of business"


save "sip96_assets_adj.dta", replace 

/////////////////////////////////////////
/// merge core file with asset module ///
/////////////////////////////////////////

use "sip96_adj.dta", clear

merge m:1 suid persid entadrid wave using sip96_assets_adj.dta


/*  Result                           # of obs.
    -----------------------------------------
    not matched                     2,634,797
        from master                 2,634,795  (_merge==1)
        from using                          2  (_merge==2)

    matched                         1,262,382  (_merge==3)
    -----------------------------------------

tab wave if _merge == 1

SU: Wave of |
       data |
 collection |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |    380,609       14.45       14.45
          2 |    362,190       13.75       28.19
          3 |      2,003        0.08       28.27
          4 |    343,847       13.05       41.32
          5 |    330,716       12.55       53.87
          6 |      1,777        0.07       53.94
          7 |    311,758       11.83       65.77
          8 |    307,877       11.69       77.45
          9 |      1,716        0.07       77.52
         10 |    297,509       11.29       88.81
         11 |    293,347       11.13       99.95
         12 |      1,446        0.05      100.00
------------+-----------------------------------
      Total |  2,634,795      100.00

*/

drop if _merge == 2 // two observations only 


drop if _merge == 1 & wave == 3 | _merge == 1 & wave == 6 | ///
		_merge == 1 & wave == 9 | _merge == 1 & wave == 12
// we do not have asset information for them, so drop. 

drop _merge 

save "sip96_merged.dta", replace 

**************************************************************
************************* 2001 Panel *************************
**************************************************************

cd ""

label drop _all

foreach num of numlist 1/9 {
	do sip01w`num'
	log close
	save "sip01w`num'.dta", replace
	clear
}

use "sip01w1.dta", clear
foreach num of numlist 2/9 {
	append using "sip01w`num'.dta"
	erase "sip01w`num'.dta"
}

save "sip01.dta", replace
erase "sip01w1.dta"


*** keeping only relevant variables ***

keep ssuid epppnum swave srefmon rhcalmn rhcalyr eentaid eeducate ///
	 elayoff elkwrk ems epnspous erace ersend1 ersend2 ersnowrk esex epdjbthn ///
	 rfnkids rfownkid rfoklt18 rmesr rmhrswk rmwklkg rmwksab rmwkwjb rnotake rtakjob rwkesr1  /// 
	 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rwksperm spanel tage tbyear tejdate1 tejdate2 tfearn  ///
	 thtotinc tpearn tpmsum1 tpmsum2 tpprpinc tptotinc tsjdate1 tsjdate2 whfnwgt wpfinwgt shhadid ///
	 tfipsst tjbocc1 tjbocc2 ejbind1 ejbind2 etenure

*** renaming variables ***
	 
rename ssuid    suid 						// sample unit identifier
rename epppnum  persid 						// person number
rename swave    wave						// wave of data collection
rename srefmon  refmon						// reference month of this record
rename rhcalmn  month						// calendar month for this reference month
rename rhcalyr  year						// calendar year for this reference month
rename eentaid  entadrid					// address id of household where person entered sample
rename eeducate educ						// education: highest degree received
rename elayoff  layoffdum					// spent time on layoff (dummy)
rename elkwrk   lkworkdum					// spent time looking for work (dummy)
rename ems      marital						// marital status
rename epnspous spouseid 					// person number of spouse
rename erace    race 						// race
rename ersend1  whystopwork1				// main reason stopped working
rename ersend2  whystopwork2				// main reason stopped working
rename ersnowrk whynowork					// main reason for not working during reference period
rename esex     sex							// sex
rename epdjbthn paidjobdum					// paid job during the reference period (dummy)			
rename rfnkids  totchild18					// total number of children under 18 in family
rename rfownkid	ownchild					// number of own children in family
rename rfoklt18 ownchild18					// number of own children under 18 in family
rename rmesr    empstatmonth				// employment status recode for month
rename rmhrswk  hourswrk					// usual hours worked per week recode in month
rename rmwklkg  weekslookwork				// number of weeks looking for work/on layoff in month
rename rmwksab  weeksabsent					// number of weeks absent without pay from job in month
rename rmwkwjb  weeksjob					// number of weeks with a job in month
rename rtakjob  couldstartjob				// could have started a job (dummy)
rename rnotake  whynotake					// why couldn't have started new job
rename rwkesr1  empstatwk1					// employment status recode for week 1
rename rwkesr2  empstatwk2					// employment status recode for week 2
rename rwkesr3  empstatwk3					// employment status recode for week 3
rename rwkesr4  empstatwk4					// employment status recode for week 4
rename rwkesr5  empstatwk5					// employment status recode for week 5
rename rwksperm weeksmonth					// number of weeks in this month
rename spanel   panel						// panel year				
rename shhadid  hhadressid  				// household address id
rename tage     age							// age as of last birthday
rename tbyear   birthyr						// year of birth
rename tejdate1 enddatejob1					// ending date of job YYYYMMDD
rename tejdate2 enddatejob2					// ending date of job YYYYMMDD
rename tfearn   totfamearn					// total family earned income for this month
rename thtotinc tothhinc					// total household income
rename tpearn   totpersearn					// total person's earned income for the reference month
rename tpmsum1  jobearn1					// earnings from job received in this month
rename tpmsum2  jobearn2					// earnings from job received in this month
rename tpprpinc propassetinc				// total property asset income
rename tptotinc totpersinc					// total person's income for the reference month
rename tsjdate1 startdatejob1				// starting date of job YYYYMMDD
rename tsjdate2 startdatejob2				// starting date of job YYYYMMDD
rename whfnwgt  hhweight					// household weight					
rename wpfinwgt persweight					// person weight for household reference person
rename tfipsst state						// state
rename tjbocc1 occupation1					// occupation
rename tjbocc2 occupation2					// occupation
rename ejbind1 industry1					// industry
rename ejbind2 industry2					// industry
rename etenure ownhomedum					// home ownership

save "sip01_adj.dta", replace  

	 	 
//////////////////////////////
/// topical module: assets ///
//////////////////////////////

label drop _all

foreach num of numlist 3(3)9 {
	do sip01t`num'
	log close
	save "sip01t`num'.dta", replace
	clear
}


use "sip01t3.dta", clear
foreach num of numlist 6(3)9 {
	append using "sip01t`num'.dta"
	erase "sip01t`num'.dta"
}

save "sip01_assets.dta", replace
erase "sip01t3.dta"


*** keeping only relevant variables ***

/* difference to wave 1996:
	
	2001 does not include emip and emjp but includes 
	tmjp tmip   
	from documentation file: seems to be the same question */

keep ealidal ealidao ealidab ealowa taljcha talicha talsbv ///
     ealjdab ealjdal talrb talkb taltb talliev talliv ///
	 evbow1 evbow2 tvbde1 tvbde2 tvbva1 tvbva2 ///
	 timja tiajta tiaita timia eoaeq tov2amt ///
	 tov1val tov2val tov1amt ta1amt ta2amt ta3amt tmhval  ///
	 tmhpr tcarval1 tcarval2 tcarval3 tpropval thhbeq  ///
	 thhira thhotast tothreva thhore rhhstk tmor1amt thhtheq ///
	 thhintbk thhintot thhvehcl tmor1pr thhmortg thhtnw ///
	 rhhuscbt thhtwlth thhdebt thhscdbt trtsha ///
	 trjmv trimv trtmv trtpri trjpri tripri ealjdao ///
	 esmjmav esmimav esmjv esmiv ssuid epppnum eentaid swave ///
	 tmjp tmip 
	 


*** renaming variables ***	 

/* naming convention of first two letters: 
	d - debt, a - asset, i - indvidual, j - joint */
	
* identifier variables
rename ssuid    suid 
rename epppnum  persid 
rename eentaid  entadrid
rename swave    wave

* checking accounts 
rename talicha  ai_checking
rename taljcha  aj_checking

* saving accounts
rename tiajta   aj_savacc
rename tiaita   ai_savacc

* loans to bank
rename ealidal  di_loanbank
rename ealjdal  dj_loanbank

* credit card debt
rename ealidab  di_ccardbill
rename ealjdab  dj_ccardbill

* retirement accounts
rename talrb    ai_IRA
rename talkb    ai_KEOGH
rename taltb    ai_401k

* business 
rename evbow1   ai_bus1_share
rename evbow2   ai_bus2_share
rename tvbva1   ai_valbus1_befdebt
rename tvbva2   ai_valbus2_befdebt
rename ealowa   ai_businesssale

rename tvbde1   di_bus1
rename tvbde2   di_bus2      

* mortages
rename tmip      di_mortage
rename tmjp      dj_mortage 

rename tmor1pr  dj_mortgs_loans_all
rename tmor1amt dj_frstscnd_mortg_orig

* property (??) and other real estate
rename tpropval aj_propertyvalue
rename tothreva aj_equ_othrealest

* rental property not attached to residence 
rename trjmv 	aj_rentprop
rename trtmv    aj_rentprop_nospous_totval
rename trimv    ai_rentprop
rename trtsha   aj_rentprop_nospous_sharval

rename trjpri   dj_rentprop
rename trtpri   dj_rentprop_nospouse
rename tripri   di_rentprop

* mobile home 
rename tmhval   aj_mobile
rename tmhpr    dj_mobile

* vehicles 
rename tcarval1 aj_vec1
rename tcarval2 aj_vec2
rename tcarval3 aj_vec3
rename tov1val  aj_othvec1
rename tov2val  aj_othvec2

rename ta1amt   dj_vec1
rename ta2amt   dj_vec2
rename ta3amt   dj_vec3
rename tov1amt  dj_othvec1
rename tov2amt  dj_othvec2

* bonds 
rename timja    aj_bonds
rename timia    ai_bonds
rename talsbv   ai_savbond

* stocks and mutual funds
rename esmjv    aj_stocks_mutfnds
rename esmiv    ai_stocks_mutfnds

rename esmjmav  dj_stocks_mutfnds
rename esmimav  di_stocks_mutfnds

* life insurance 
rename talliev  ai_lifins_empl
rename talliv   ai_lifins_all

* other assets & debt 
rename eoaeq    ai_equ_othass

rename ealidao  di_othdebt
rename ealjdao  dj_othdebt

* recoded variables (provideded by SIPP)
rename thhtnw   aj_networth_recod
rename thhtwlth aj_wealth_recod
rename thhtheq  aj_homeequity_recod 
rename thhbeq   aj_busequity_recod
rename thhvehcl aj_netequ_vec_recod
rename thhotast aj_equ_othass_recod
rename thhira   aj_IRA_KEOGH_recod
rename thhore   aj_equ_realest_nohome_recod
rename thhintbk aj_intrstass_bank_recod
rename thhintot aj_intrstass_oth_recod
rename rhhstk   aj_stocks_mutfnds_recod

rename thhdebt  dj_total_recod
rename thhmortg dj_home_recod
rename rhhuscbt dj_unsec_recod
rename thhscdbt dj_sec_recod


*label var ealidal "amt loans from bank (no car, no home) in own name"
*label var ealidao "amt other debt (not yet mentioned) in own name"
*label var ealowa  "amt owed to you for sale of business"


save "sip01_assets_adj.dta", replace 

/////////////////////////////////////////
/// merge core file with asset module ///
/////////////////////////////////////////

use "sip01_adj.dta", clear

merge m:1 suid persid entadrid wave using sip01_assets_adj.dta


/*  Result                           # of obs.
    -----------------------------------------
    not matched                     1,758,999
        from master                 1,758,999  (_merge==1)
        from using                          0  (_merge==2)

    matched                           818,943  (_merge==3)
    -----------------------------------------


tab wave if _merge == 1

SU: Wave of |
       data |
 collection |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |    361,046       20.53       20.53
          2 |    289,300       16.45       36.97
          3 |      1,500        0.09       37.06
          4 |    282,062       16.04       53.09
          5 |    277,247       15.76       68.85
          6 |      1,690        0.10       68.95
          7 |    274,651       15.61       84.56
          8 |    269,906       15.34       99.91
          9 |      1,597        0.09      100.00
------------+-----------------------------------
      Total |  1,758,999      100.00


*/

drop if _merge == 2 // zero observations


drop if _merge == 1 & wave == 3 | _merge == 1 & wave == 6 | ///
		_merge == 1 & wave == 9 		
// we do not have asset information for them, so drop. 


drop _merge 

save "sip01_merged.dta", replace 


**************************************************************
************************* 2004 Panel *************************
**************************************************************

cd ""

label drop _all

foreach num of numlist 1/12 {
	do sippl04puw`num'
	log close
	save "sip04w`num'.dta", replace
	clear
}

use "sip04w1.dta", clear
foreach num of numlist 2/12 {
	append using "sip04w`num'.dta"
	erase "sip04w`num'.dta"
}

save "sip04.dta", replace
erase "sip04w1.dta"


*** keeping only relevant variables ***

keep ssuid epppnum swave srefmon rhcalmn rhcalyr eentaid eeducate ///
	 elayoff elkwrk ems epnspous erace ersend1 ersend2 ersnowrk esex epdjbthn ///
	 rfnkids rfownkid rfoklt18 rmesr rmhrswk rmwklkg rmwksab rmwkwjb rnotake rtakjob rwkesr1  /// 
	 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rwksperm spanel tage tbyear tejdate1 tejdate2 tfearn  ///
	 thtotinc tpearn tpmsum1 tpmsum2 tpprpinc tptotinc tsjdate1 tsjdate2 whfnwgt wpfinwgt shhadid ///
	 tfipsst tjbocc1 tjbocc2 ejbind1 ejbind2 etenure

*** renaming variables ***
	 
rename ssuid    suid 						// sample unit identifier
rename epppnum  persid 						// person number
rename swave    wave						// wave of data collection
rename srefmon  refmon						// reference month of this record
rename rhcalmn  month						// calendar month for this reference month
rename rhcalyr  year						// calendar year for this reference month
rename eentaid  entadrid					// address id of household where person entered sample
rename eeducate educ						// education: highest degree received
rename elayoff  layoffdum					// spent time on layoff (dummy)
rename elkwrk   lkworkdum					// spent time looking for work (dummy)
rename ems      marital						// marital status
rename epnspous spouseid 					// person number of spouse
rename erace    race 						// race
rename ersend1  whystopwork1				// main reason stopped working
rename ersend2  whystopwork2				// main reason stopped working
rename ersnowrk whynowork					// main reason for not working during reference period
rename esex     sex							// sex
rename epdjbthn paidjobdum					// paid job during the reference period (dummy)			
rename rfnkids  totchild18					// total number of children under 18 in family
rename rfownkid	ownchild					// number of own children in family
rename rfoklt18 ownchild18					// number of own children under 18 in family
rename rmesr    empstatmonth				// employment status recode for month
rename rmhrswk  hourswrk					// usual hours worked per week recode in month
rename rmwklkg  weekslookwork				// number of weeks looking for work/on layoff in month
rename rmwksab  weeksabsent					// number of weeks absent without pay from job in month
rename rmwkwjb  weeksjob					// number of weeks with a job in month
rename rtakjob  couldstartjob				// could have started a job (dummy)
rename rnotake  whynotake					// why couldn't have started new job
rename rwkesr1  empstatwk1					// employment status recode for week 1
rename rwkesr2  empstatwk2					// employment status recode for week 2
rename rwkesr3  empstatwk3					// employment status recode for week 3
rename rwkesr4  empstatwk4					// employment status recode for week 4
rename rwkesr5  empstatwk5					// employment status recode for week 5
rename rwksperm weeksmonth					// number of weeks in this month
rename spanel   panel						// panel year				
rename shhadid  hhadressid  				// household address id
rename tage     age							// age as of last birthday
rename tbyear   birthyr						// year of birth
rename tejdate1 enddatejob1					// ending date of job YYYYMMDD
rename tejdate2 enddatejob2					// ending date of job YYYYMMDD
rename tfearn   totfamearn					// total family earned income for this month
rename thtotinc tothhinc					// total household income
rename tpearn   totpersearn					// total person's earned income for the reference month
rename tpmsum1  jobearn1					// earnings from job received in this month
rename tpmsum2  jobearn2					// earnings from job received in this month
rename tpprpinc propassetinc				// total property asset income
rename tptotinc totpersinc					// total person's income for the reference month
rename tsjdate1 startdatejob1				// starting date of job YYYYMMDD
rename tsjdate2 startdatejob2				// starting date of job YYYYMMDD
rename whfnwgt  hhweight					// household weight					
rename wpfinwgt persweight					// person weight for household reference person
rename tfipsst state						// state
rename tjbocc1 occupation1					// occupation
rename tjbocc2 occupation2					// occupation
rename ejbind1 industry1					// industry
rename ejbind2 industry2					// industry
rename etenure ownhomedum					// home ownership

save "sip04_adj.dta", replace  

//////////////////////////////
/// topical module: assets ///
//////////////////////////////

 label drop _all


foreach num of numlist 3(3)6 {
	do sippp04putm`num'
	log close
	save "sip04t`num'.dta", replace
	clear
}

use "sip04t3.dta", clear
append using "sip04t6.dta"

save "sip04_assets.dta", replace

foreach num of numlist 3(3)6 {
erase "sip04t`num'.dta"
}

*** keeping only relevant variables ***

keep ealidal ealidao ealidab ealowa taljcha talicha talsbv ///
     ealjdab ealjdal talrb talkb taltb talliev talliv ///
	 evbow1 evbow2 tvbde1 tvbde2 tvbva1 tvbva2 ///
	 timja tiajta tiaita timia eoaeq tov2amt ///
	 tov1val tov2val tov1amt ta1amt ta2amt ta3amt tmhval  ///
	 tmhpr tcarval1 tcarval2 tcarval3 tpropval thhbeq  ///
	 thhira thhotast tothreva thhore rhhstk tmor1amt thhtheq ///
	 thhintbk thhintot thhvehcl tmor1pr thhmortg thhtnw ///
	 rhhuscbt thhtwlth thhdebt thhscdbt trtsha ///
	 trjmv trimv trtmv trtpri trjpri tripri ealjdao ///
	 esmjmav esmimav esmjv esmiv ssuid epppnum eentaid swave ///
	 tmjp tmip 
	 


*** renaming variables ***	 

/* naming convention of first two letters: 
	d - debt, a - asset, i - indvidual, j - joint */
	
* identifier variables
rename ssuid    suid 
rename epppnum  persid 
rename eentaid  entadrid
rename swave    wave

* checking accounts 
rename talicha  ai_checking
rename taljcha  aj_checking

* saving accounts
rename tiajta   aj_savacc
rename tiaita   ai_savacc

* loans to bank
rename ealidal  di_loanbank
rename ealjdal  dj_loanbank

* credit card debt
rename ealidab  di_ccardbill
rename ealjdab  dj_ccardbill

* retirement accounts
rename talrb    ai_IRA
rename talkb    ai_KEOGH
rename taltb    ai_401k

* business 
rename evbow1   ai_bus1_share
rename evbow2   ai_bus2_share
rename tvbva1   ai_valbus1_befdebt
rename tvbva2   ai_valbus2_befdebt
rename ealowa   ai_businesssale

rename tvbde1   di_bus1
rename tvbde2   di_bus2      

* mortages
rename tmip      di_mortage
rename tmjp      dj_mortage 

rename tmor1pr  dj_mortgs_loans_all
rename tmor1amt dj_frstscnd_mortg_orig

* property (??) and other real estate
rename tpropval aj_propertyvalue
rename tothreva aj_equ_othrealest

* rental property not attached to residence 
rename trjmv 	aj_rentprop
rename trtmv    aj_rentprop_nospous_totval
rename trimv    ai_rentprop
rename trtsha   aj_rentprop_nospous_sharval

rename trjpri   dj_rentprop
rename trtpri   dj_rentprop_nospouse
rename tripri   di_rentprop

* mobile home 
rename tmhval   aj_mobile
rename tmhpr    dj_mobile

* vehicles 
rename tcarval1 aj_vec1
rename tcarval2 aj_vec2
rename tcarval3 aj_vec3
rename tov1val  aj_othvec1
rename tov2val  aj_othvec2

rename ta1amt   dj_vec1
rename ta2amt   dj_vec2
rename ta3amt   dj_vec3
rename tov1amt  dj_othvec1
rename tov2amt  dj_othvec2

* bonds 
rename timja    aj_bonds
rename timia    ai_bonds
rename talsbv   ai_savbond

* stocks and mutual funds
rename esmjv    aj_stocks_mutfnds
rename esmiv    ai_stocks_mutfnds

rename esmjmav  dj_stocks_mutfnds
rename esmimav  di_stocks_mutfnds

* life insurance 
rename talliev  ai_lifins_empl
rename talliv   ai_lifins_all

* other assets & debt 
rename eoaeq    ai_equ_othass

rename ealidao  di_othdebt
rename ealjdao  dj_othdebt

* recoded variables (provideded by SIPP)
rename thhtnw   aj_networth_recod
rename thhtwlth aj_wealth_recod
rename thhtheq  aj_homeequity_recod 
rename thhbeq   aj_busequity_recod
rename thhvehcl aj_netequ_vec_recod
rename thhotast aj_equ_othass_recod
rename thhira   aj_IRA_KEOGH_recod
rename thhore   aj_equ_realest_nohome_recod
rename thhintbk aj_intrstass_bank_recod
rename thhintot aj_intrstass_oth_recod
rename rhhstk   aj_stocks_mutfnds_recod

rename thhdebt  dj_total_recod
rename thhmortg dj_home_recod
rename rhhuscbt dj_unsec_recod
rename thhscdbt dj_sec_recod


save "sip04_assets_adj.dta", replace 

/////////////////////////////////////////
/// merge core file with asset module ///
/////////////////////////////////////////

use "sip04_adj.dta", clear

merge m:1 suid persid entadrid wave using sip04_assets_adj.dta


/*      Result                           # of obs.
    -----------------------------------------
    not matched                     3,037,078
        from master                 3,037,078  (_merge==1)
        from using                          0  (_merge==2)

    matched                           771,562  (_merge==3)
    -----------------------------------------

tab wave if _merge == 1


SU: Wave of |
       data |
 collection |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |    441,849       14.55       14.55
          2 |    413,025       13.60       28.15
          3 |      2,474        0.08       28.23
          4 |    391,844       12.90       41.13
          5 |    382,778       12.60       53.73
          6 |      2,691        0.09       53.82
          7 |    371,284       12.23       66.05
          8 |    366,682       12.07       78.12
          9 |    171,188        5.64       83.76
         10 |    167,049        5.50       89.26
         11 |    162,638        5.36       94.61
         12 |    163,576        5.39      100.00
------------+-----------------------------------
      Total |  3,037,078      100.00


*/

drop if _merge == 2 // zero observations

drop if _merge == 1 & wave == 3 | _merge == 1 & wave == 6 	
// we do not have asset information for them, so drop. 


drop _merge 

save "sip04_merged.dta", replace 


**************************************************************
************************* 2008 Panel *************************
**************************************************************

cd ""

label drop _all

foreach num of numlist 1/16 {
	do sippl08puw`num'
	log close
	save "sip08w`num'.dta", replace
	clear
}

use "sip08w1.dta", clear
foreach num of numlist 2/16 {
	append using "sip08w`num'.dta"
	erase "sip08w`num'.dta"
}

save "sip08.dta", replace
erase "sip08w1.dta"

*** keeping only relevant variables ***

keep ssuid epppnum swave srefmon rhcalmn rhcalyr eentaid eeducate ///
	 elayoff elkwrk ems epnspous erace ersend1 ersend2 ersnowrk esex epdjbthn ///
	 rfnkids rfownkid rfoklt18 rmesr rmhrswk rmwklkg rmwksab rmwkwjb rnotake rtakjob rwkesr1  /// 
	 rwkesr2 rwkesr3 rwkesr4 rwkesr5 rwksperm spanel tage tbyear tejdate1 tejdate2 tfearn  ///
	 thtotinc tpearn tpmsum1 tpmsum2 tpprpinc tptotinc tsjdate1 tsjdate2 whfnwgt wpfinwgt shhadid ///
	 tfipsst tjbocc1 tjbocc2 ejbind1 ejbind2 etenure

*** renaming variables ***
	 
rename ssuid    suid 						// sample unit identifier
rename epppnum  persid 						// person number
rename swave    wave						// wave of data collection
rename srefmon  refmon						// reference month of this record
rename rhcalmn  month						// calendar month for this reference month
rename rhcalyr  year						// calendar year for this reference month
rename eentaid  entadrid					// address id of household where person entered sample
rename eeducate educ						// education: highest degree received
rename elayoff  layoffdum					// spent time on layoff (dummy)
rename elkwrk   lkworkdum					// spent time looking for work (dummy)
rename ems      marital						// marital status
rename epnspous spouseid 					// person number of spouse
rename erace    race 						// race
rename ersend1  whystopwork1				// main reason stopped working
rename ersend2  whystopwork2				// main reason stopped working
rename ersnowrk whynowork					// main reason for not working during reference period
rename esex     sex							// sex
rename epdjbthn paidjobdum					// paid job during the reference period (dummy)			
rename rfnkids  totchild18					// total number of children under 18 in family
rename rfownkid	ownchild					// number of own children in family
rename rfoklt18 ownchild18					// number of own children under 18 in family
rename rmesr    empstatmonth				// employment status recode for month
rename rmhrswk  hourswrk					// usual hours worked per week recode in month
rename rmwklkg  weekslookwork				// number of weeks looking for work/on layoff in month
rename rmwksab  weeksabsent					// number of weeks absent without pay from job in month
rename rmwkwjb  weeksjob					// number of weeks with a job in month
rename rtakjob  couldstartjob				// could have started a job (dummy)
rename rnotake  whynotake					// why couldn't have started new job
rename rwkesr1  empstatwk1					// employment status recode for week 1
rename rwkesr2  empstatwk2					// employment status recode for week 2
rename rwkesr3  empstatwk3					// employment status recode for week 3
rename rwkesr4  empstatwk4					// employment status recode for week 4
rename rwkesr5  empstatwk5					// employment status recode for week 5
rename rwksperm weeksmonth					// number of weeks in this month
rename spanel   panel						// panel year				
rename shhadid  hhadressid  				// household address id
rename tage     age							// age as of last birthday
rename tbyear   birthyr						// year of birth
rename tejdate1 enddatejob1					// ending date of job YYYYMMDD
rename tejdate2 enddatejob2					// ending date of job YYYYMMDD
rename tfearn   totfamearn					// total family earned income for this month
rename thtotinc tothhinc					// total household income
rename tpearn   totpersearn					// total person's earned income for the reference month
rename tpmsum1  jobearn1					// earnings from job received in this month
rename tpmsum2  jobearn2					// earnings from job received in this month
rename tpprpinc propassetinc				// total property asset income
rename tptotinc totpersinc					// total person's income for the reference month
rename tsjdate1 startdatejob1				// starting date of job YYYYMMDD
rename tsjdate2 startdatejob2				// starting date of job YYYYMMDD
rename whfnwgt  hhweight					// household weight					
rename wpfinwgt persweight					// person weight for household reference person
rename tfipsst state						// state
rename tjbocc1 occupation1					// occupation
rename tjbocc2 occupation2					// occupation
rename ejbind1 industry1					// industry
rename ejbind2 industry2					// industry
rename etenure ownhomedum					// home ownership

save "sip08_adj.dta", replace  


//////////////////////////////
/// topical module: assets ///
//////////////////////////////

 label drop _all

foreach num of numlist 4(3)10 {
	do sippp08putm`num'
	log close
	save "sip08t`num'.dta", replace
	clear
}

use "sip08t4.dta", clear
foreach num of numlist 7(3)10 {
	append using "sip08t`num'.dta"
	erase "sip08t`num'.dta"
}

save "sip08_assets.dta", replace
erase "sip08t4.dta"


*** keeping only relevant variables ***

/* difference to wave 2004:
	
	2008 does not include:
	ealidal ealjdal ealidao ealjdao ealidab ealjdab 
	eoaeq esmjmav esmimav esmjv esmiv
	
	but includes:
	talidal taljdal talidao taljdao talidab taljdab 
	toaeq tsmjmav tsmimav tsmjv tsmiv
	
	from documentation file: seems to be same questions */


keep ealowa taljcha talicha talsbv ///
     talrb talkb taltb talliev talliv ///
	 evbow1 evbow2 tvbde1 tvbde2 tvbva1 tvbva2 ///
	 timja tiajta tiaita timia tov2amt ///
	 tov1val tov2val tov1amt ta1amt ta2amt ta3amt tmhval  ///
	 tmhpr tcarval1 tcarval2 tcarval3 tpropval thhbeq  ///
	 thhira thhotast tothreva thhore rhhstk tmor1amt thhtheq ///
	 thhintbk thhintot thhvehcl tmor1pr thhmortg thhtnw ///
	 rhhuscbt thhtwlth thhdebt thhscdbt trtsha ///
	 trjmv trimv trtmv trtpri trjpri tripri  ///
	 ssuid epppnum eentaid swave ///
	 tmjp tmip talidal taljdal talidao taljdao ///
	 toaeq tsmjmav tsmimav tsmjv tsmiv taljdab talidab
	 
	 
*** renaming variables ***	 

/* naming convention of first two letters: 
	d - debt, a - asset, i - indvidual, j - joint */
	
* identifier variables
rename ssuid    suid 
rename epppnum  persid 
rename eentaid  entadrid
rename swave    wave

* checking accounts 
rename talicha  ai_checking
rename taljcha  aj_checking

* saving accounts
rename tiajta   aj_savacc
rename tiaita   ai_savacc

* loans to bank
rename talidal  di_loanbank
rename taljdal  dj_loanbank

* credit card debt
rename talidab  di_ccardbill
rename taljdab  dj_ccardbill

* retirement accounts
rename talrb    ai_IRA
rename talkb    ai_KEOGH
rename taltb    ai_401k

* business 
rename evbow1   ai_bus1_share
rename evbow2   ai_bus2_share
rename tvbva1   ai_valbus1_befdebt
rename tvbva2   ai_valbus2_befdebt
rename ealowa   ai_businesssale

rename tvbde1   di_bus1
rename tvbde2   di_bus2      

* mortages
rename tmip      di_mortage
rename tmjp      dj_mortage 

rename tmor1pr  dj_mortgs_loans_all
rename tmor1amt dj_frstscnd_mortg_orig

* property (??) and other real estate
rename tpropval aj_propertyvalue
rename tothreva aj_equ_othrealest

* rental property not attached to residence 
rename trjmv 	aj_rentprop
rename trtmv    aj_rentprop_nospous_totval
rename trimv    ai_rentprop
rename trtsha   aj_rentprop_nospous_sharval

rename trjpri   dj_rentprop
rename trtpri   dj_rentprop_nospouse
rename tripri   di_rentprop

* mobile home 
rename tmhval   aj_mobile
rename tmhpr    dj_mobile

* vehicles 
rename tcarval1 aj_vec1
rename tcarval2 aj_vec2
rename tcarval3 aj_vec3
rename tov1val  aj_othvec1
rename tov2val  aj_othvec2

rename ta1amt   dj_vec1
rename ta2amt   dj_vec2
rename ta3amt   dj_vec3
rename tov1amt  dj_othvec1
rename tov2amt  dj_othvec2

* bonds 
rename timja    aj_bonds
rename timia    ai_bonds
rename talsbv   ai_savbond

* stocks and mutual funds
rename tsmjv    aj_stocks_mutfnds
rename tsmiv    ai_stocks_mutfnds

rename tsmjmav  dj_stocks_mutfnds
rename tsmimav  di_stocks_mutfnds

* life insurance 
rename talliev  ai_lifins_empl
rename talliv   ai_lifins_all

* other assets & debt 
rename toaeq    ai_equ_othass

rename talidao  di_othdebt
rename taljdao  dj_othdebt

* recoded variables (provideded by SIPP)
rename thhtnw   aj_networth_recod
rename thhtwlth aj_wealth_recod
rename thhtheq  aj_homeequity_recod 
rename thhbeq   aj_busequity_recod
rename thhvehcl aj_netequ_vec_recod
rename thhotast aj_equ_othass_recod
rename thhira   aj_IRA_KEOGH_recod
rename thhore   aj_equ_realest_nohome_recod
rename thhintbk aj_intrstass_bank_recod
rename thhintot aj_intrstass_oth_recod
rename rhhstk   aj_stocks_mutfnds_recod

rename thhdebt  dj_total_recod
rename thhmortg dj_home_recod
rename rhhuscbt dj_unsec_recod
rename thhscdbt dj_sec_recod

save "sip08_assets_adj.dta", replace 

/////////////////////////////////////////
/// merge core file with asset module ///
/////////////////////////////////////////

use "sip08_adj.dta", clear

merge m:1 suid persid entadrid wave using sip08_assets_adj.dta


/*   Result                           # of obs.
    -----------------------------------------
    not matched                     4,299,863
        from master                 4,299,863  (_merge==1)
        from using                          0  (_merge==2)

    matched                         1,016,670  (_merge==3)
    -----------------------------------------


tab wave if _merge == 1


SU: Wave of |
       data |
 collection |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |    421,911        9.81        9.81
          2 |    392,702        9.13       18.95
          3 |    380,200        8.84       27.79
          4 |      2,323        0.05       27.84
          5 |    361,559        8.41       36.25
          6 |    352,469        8.20       44.45
          7 |      2,189        0.05       44.50
          8 |    337,766        7.86       52.35
          9 |    329,124        7.65       60.01
         10 |      2,181        0.05       60.06
         11 |    312,606        7.27       67.33
         12 |    309,419        7.20       74.52
         13 |    304,193        7.07       81.60
         14 |    300,067        6.98       88.58
         15 |    290,266        6.75       95.33
         16 |    200,888        4.67      100.00
------------+-----------------------------------
      Total |  4,299,863      100.00



*/

drop if _merge == 2 // zero observations

drop if _merge == 1 & wave == 4 | _merge == 1 & wave == 7 | ///
		_merge == 1 & wave == 10
// we do not have asset information for them, so drop. 


drop _merge 

save "sip08_merged.dta", replace 



**************************************************************
************************* 2014 Panel *************************
**************************************************************

cd ""


/////////////////////////////////////////////////////////////////
// RESTRUCTURING OF SIPP: CREATE VARIABLE CROSSWALK 2008 - 2014// 
/////////////////////////////////////////////////////////////////

clear all

import delimited "sipp_crosswalk_2008_2014.csv", varnames(1)

keep variable_2008 variable_2014


/* ssuid    = ssuid
   epppnum  = pnum  (?????)
   swave    = swave 
   srefmon  =
   rhcalmn  =
   rhcalyr  =
   eentaid  =
   eeducate = EEDUC
   elayoff  = ENJ_LAYOFF
   elkwrk   = ENJ_LKWRK
   ems 		= EMS 
   epnspous = EPNSPOUSE
   erace    = ERACE + TRACE 										// TRACE is more detailed category
   ersend1  = EJB1_RSEND
   ersend2  = EJB2_RSEND
   ersnowrk = ENJ_NOWRK1 - ENJ_NOWRK12 								// dummy variables instead of categorial
   esex     = ESEX
   epdjbthn = EJB1_SCRNR + EJB2_SCRNR 								// job 1 and job 2 
   rfnkids  = RFRELU18
   rmesr    = RMESR
   rmhrswk  = TMWKHRS
   rmwklkg  = RMWKLKG
   rmwksab  = RMWKSAB
   rmwkwjb  = RMWKWJB
   rnotake  =  
   rtakjob  = 
   rwkesr1  = RWKESR1
   rwkesr2  = RWKESR2
   rwkesr3  = RWKESR3
   rwkesr4  = RWKESR4
   rwkesr5  = RWKESR5
   rwksperm = RWKSPERM
   spanel   = spanel
   tage     = TAGE
   tbyear   = TDOB_BYEAR
   tejdate1 =
   tejdate2 =
   tfearn   =
   thtotinc = THTOTINC
   tpearn   = TPEARN
   tpmsum1  =
   tpmsum2  =
   tpprpinc = 
   tptotinc =  
   tsjdate1 =
   tsjdate2 = 
   whfnwgt  = 
   wpfinwgt =
   ealowa   =
   taljcha  = TJSCHKVAL
   talicha  = TOCHKVAL
   talsbv   = TOGOVSVAL
   talrb    = TIRAKEOVAL
   talkb    = 
   taltb    = TTHR401VAL
   talliev  =
   talliv   = TLIFE_CVAL
   evbow1   = EBSJ1PEROWN
   evbow2   = EBSJ2PEROWN
   tvbde1   = TBSJ1DEBTVAL
   tvbde2   = TBSJ2DEBTVAL
   tvbva1   = TBSJ1VAL
   tvbva2   = TBSJ2VAL
   timja    = TJSGOVSVAL + TJSMCBDVAL 								  // govt securities and municipal/corporate bonds sep. 
   tiajta   = TJSCDVAL + TJSICHKVAL + TJSMMVAL + TJSSAVVAL			  // CDs, checking acc., money market acc, saving acc. sep.
   tiaita   = TOCDVAL + TOICHKVAL + TOMMVAL + TOSAVVAL                // CDs, checking acc., money market acc, saving acc. sep.
   timia    = 
   tov2amt	=
   tov1val  = TBOATVAL + TMCYCVAL + TORECVAL + TRVVAL                 // boat, motorcycle, recreational vehicle, RV sp. 
   tov2val  =
   tov3val  = 
   tov1amt  = TBOATDEBTVAL + TMCYCDEBTVAL + TORECDEBTVAL + TRVDEBTVAL // boat, motorcycle, recreational vehicle, RV sp. 
   ta1amt   = TVEH1DEBTVAL	
   ta2amt   = TVEH2DEBTVAL
   ta3amt   = TVEH3DEBTVAL
   tmhval   = TMHVAL
   tmhpr    =
   tcarval1 = 
   tcarval2 = TVEH2VAL
   tcarval3 = TVEH3VAL
   tpropval = TPRVAL
   thhbeq   = THEQ_BUS
   thhira   = THVAL_RET
   thhotast = THVAL_OTH
   tothreva = TJOREVAL + TJSREVAL + TOREVAL							   // for indivdiuals without spouse , with spouse (joint div. by two), in own name
   thhore   = THEQ_RE + THEQ_RENT									   // other real estate and rental property sep.
   rhhstk   = THVAL_STMF
   tmor1amt =
   thhtheq  = THEQ_HOME
   thhintbk = THVAL_BANK
   thhintot = THVAL_BOND
   thhvehcl = THEQ_VEH
   tmor1pr  =
   thhmortg	= THDEBT_HOME
   thhtnw   = THNETWORTH
   rhhuscbt = THDEBT_USEC
   thhtwlth = THVAL_AST
   thhdebt  = THDEBT_AST
   thhscdbt = THDEBT_SEC
   trtsha   = 
   trjmv    = TJSRPVAL
   trimv    = TORPVAL
   trtmv	= TJORPVAL
   trtpri   = TJORPDEBTVAL
   trjpri   = TJSRPDEBTVAL
   tripri   = TORPDEBTVAL	
   tmjp     = 
   tmip     =
   talidal  =
   taljdal  =
   talidao  = TOOTDEBTVAL
   taljdao  = TJSOTDEBTVAL
   toaeq    = TOAEQ
   tsmjmav  =
   tsmimav  =
   tsmjv    = TJSMFVAL + TJSSTVAL									  // mutual funds and stocks sep. 
   tsmiv    = TOMFVAL + TOSTVAL										  // mutual funds and stocks sep. 
   taljdab  = TJSCCDEBTVAL
   talidab  = TOCCDEBTVAL */

////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


*** load 2014 data  

clear all 
 
set maxvar 10000  

foreach num of numlist 1/4 {
use pu2014w`num'.dta, clear

*** keeping only relevant variables ***

keep ssuid pnum swave eeduc enj_layoff enj_lkwrk  ///
	 ems epnspouse erace trace ejb1_rsend ejb2_rsend ///
	 enj_nowrk* esex ejb1_scrnr ejb2_scrnr rfrelu18 rmesr  ///
	 tmwkhrs rmwklkg rmwksab rmwkwjb rwkesr1 rwkesr2 rwkesr3  ///
	 rwkesr4 rwkesr5 rwksperm spanel tage tdob_byear thtotinc tpearn ///
	 tjschkval tochkval togovsval tirakeoval tthr401val tlife_cval ///
	 ebsj1perown ebsj2perown tbsj1debtval tbsj2debtval tbsj1val tbsj2val ///
	 tjsgovsval tjsmcbdval tjscdval tjsichkval tjsmmval tjssavval tocdval ///
	 toichkval tommval tosavval tboatval tmcycval torecval trvval ///
	 tboatdebtval tmcycdebtval torecdebtval trvdebtval tveh1debtval ///
	 tveh2debtval tveh3debtval tmhval tveh2val tveh3val tprval ///
	 theq_bus thval_ret thval_oth tjoreval tjsreval toreval theq_re ///
	 theq_rent thval_stmf theq_home thval_bank thval_bond theq_veh ///
	 thdebt_home thnetworth thdebt_usec thval_ast thdebt_ast ///
	 thdebt_sec tjsrpval torpval tjorpval tjorpdebtval tjsrpdebtval ///
	 torpdebtval tootdebtval tjsotdebtval tjsmfval tjsstval ///
	 tomfval tostval tjsccdebtval toccdebtval tveh1val ///
	 rwksperm wpfinwgt monthcode ///
	 tjb1_ind tjb2_ind etenure shhadid tjb1_occ tjb2_occ tst_intv
	 
save pu2014w`num'_adj.dta, replace
}

use pu2014w1_adj.dta, clear
foreach num of numlist 2/4 {
append using pu2014w`num'_adj.dta
}

*** renaming variables ***
	 
rename ssuid      suid 
rename pnum       persid 
rename swave      wave
rename monthcode   refmon
*rename rhcalmn   month
*rename rhcalyr   year
*rename eentaid   entadrid
rename eeduc      educ
rename enj_layoff layoffdum
rename enj_lkwrk  lkworkdum
rename ems        marital
rename epnspouse  spouseid 
rename erace      race 
rename trace      race_detail
rename ejb1_rsend whystopwork_job1
rename ejb2_rsend whystopwork_job2
foreach num of numlist 1/12 {
rename enj_nowrk`num' whynowork_`num'
}
rename esex       sex
rename ejb1_scrnr paidjobdum1
rename ejb2_scrnr paidjobdum2
rename rfrelu18   totchild18
rename rmesr      empstatmonth
rename tmwkhrs    hourswrk
rename rmwklkg    weekslookwork
rename rmwksab    weeksabsent
rename rmwkwjb    weeksjob
*rename rtakjob   couldstartjob
*rename rnotake   whynotake
rename rwkesr1    empstatwk1
rename rwkesr2    empstatwk2
rename rwkesr3    empstatwk3
rename rwkesr4    empstatwk4
rename rwkesr5    empstatwk5
rename rwksperm   weeksmonth
rename spanel     panel 
rename tage       age
rename tdob_byear birthyr
*rename tejdate1  enddatejob1
*rename tejdate2  enddatejob2
*rename tfearn    totfamearn
rename thtotinc   tothhinc
rename tpearn     totpersearn
*rename tpmsum1   jobearn1
*rename tpmsum2   jobearn2
*rename tpprpinc  propassetinc
*rename tptotinc  totpersinc
*rename tsjdate1  startdatejob1
*rename tsjdate2  startdatejob2
*rename whfnwgt   hhweight
rename  wpfinwgt  persweight
rename shhadid hhadressid
rename tjb1_ind industry1
rename tjb2_ind industry2
rename tjb1_occ occupation1
rename tjb2_occ occupation2
rename etenure ownhomedum
rename tst_intv state						

*** asset data ***	 

/* naming convention of first two letters: 
	d - debt, a - asset, i - indvidual, j - joint */
	

* checking accounts 
rename tochkval    ai_checking
rename tjschkval   aj_checking

* saving accounts
rename tjscdval    aj_cds
rename tjsichkval  aj_check
rename tjsmmval    aj_mnymrkt
rename tjssavval   aj_savings

gen aj_savacc = aj_cds + aj_check + aj_mnymrkt + aj_savings

rename tocdval     ai_cds
rename toichkval   ai_check
rename tommval     ai_mnymrkt
rename tosavval    ai_savings

gen ai_savacc = ai_cds + ai_check + ai_mnymrkt + ai_savings

* loans to bank
*rename talidal     di_loanbank
*rename taljdal     dj_loanbank

* credit card debt
rename toccdebtval  di_ccardbill
rename tjsccdebtval dj_ccardbill

* retirement accounts
rename tirakeoval   ai_ira_keogh // in prev. years --> this is separate (IRA and KEOGH)
*rename talkb       ai_keogh
rename tthr401val   ai_401k

* business 
rename ebsj1perown ai_bus1_share
rename ebsj2perown ai_bus2_share
rename tbsj1val    ai_valbus1_befdebt
rename tbsj2val    ai_valbus2_befdebt
*rename ealowa     ai_businesssale

rename tbsj1debtval di_bus1
rename tbsj2debtval di_bus2      

* mortages
*rename tmip       di_mortage
*rename tmjp       dj_mortage 

*rename tmor1pr    dj_mortgs_loans_all
*rename tmor1amt   dj_frstscnd_mortg_orig

* property (??) and other real estate
rename tprval      aj_propertyvalue
rename tjoreval    aj_equ_othrealest_wspouse  // when using this var, check again
rename tjsreval    aj_equ_othrealest_wospouse
rename toreval     aj_equ_othrealest_own

gen aj_equ_othrealest = aj_equ_othrealest_wspouse + aj_equ_othrealest_wospouse + aj_equ_othrealest_own

* rental property not attached to residence 
rename tjsrpval    aj_rentprop
rename tjorpval    aj_rentprop_nospous_totval
rename torpval     ai_rentprop
*rename trtsha     aj_rentprop_nospous_sharval

rename tjsrpdebtval dj_rentprop
rename tjorpdebtval dj_rentprop_nospouse
rename torpdebtval  di_rentprop

* mobile home 
rename tmhval      aj_mobile
*rename tmhpr      dj_mobile

* vehicles 
rename tveh1val    aj_vec1
rename tveh2val    aj_vec2
rename tveh3val    aj_vec3
rename tboatval    aj_boat
rename tmcycval    aj_mtrcycle
rename torecval    aj_recrveh
rename trvval      aj_rv
*rename tov2val    aj_othvec2

gen aj_othvec1 = aj_boat + aj_mtrcycle + aj_recrveh + aj_rv

rename tveh1debtval  dj_vec1
rename tveh2debtval  dj_vec2
rename tveh3debtval  dj_vec3
rename tboatdebtval  dj_boat
rename tmcycdebtval  dj_mtrcycle
rename torecdebtval  dj_recrveh
rename trvdebtval    dj_rv

gen dj_othvec1 = dj_boat + dj_mtrcycle + dj_recrveh + dj_rv

*rename tov2amt  dj_othvec2

* bonds 
rename tjsgovsval  aj_bonds_gov
rename tjsmcbdval  aj_bonds_municipal

gen aj_bonds = aj_bonds_gov + aj_bonds_municipal

*rename timia      ai_bonds
rename togovsval   ai_savbond

* stocks and mutual funds
rename tjsmfval    aj_mutfnds
rename tjsstval    aj_stocks
rename tomfval     ai_mutfnds
rename tostval     ai_stocks

gen aj_stocks_mutfnds = aj_mutfnds + aj_stocks
gen ai_stocks_mutfnds = ai_mutfnds + ai_stocks

*rename tsmjmav    dj_stocks_mutfnds
*rename tsmimav    di_stocks_mutfnds

* life insurance 
*rename talliev    ai_lifins_empl
rename tlife_cval  ai_lifins_all

* other assets & debt 
*rename toaeq      ai_equ_othass

rename tootdebtval  di_othdebt
rename tjsotdebtval dj_othdebt

* recoded variables (provideded by sipp)
rename thnetworth aj_networth_recod
rename thval_ast  aj_wealth_recod
rename theq_home  aj_homeequity_recod 
rename theq_bus   aj_busequity_recod
rename theq_veh   aj_netequ_vec_recod
rename thval_oth  aj_equ_othass_recod
rename thval_ret  aj_ira_keogh_recod
rename theq_re    aj_equ_othrealest_recod
rename theq_rent  aj_equ_othrent_recod

gen aj_equ_realest_nohome_recod = aj_equ_othrealest_recod + aj_equ_othrent_recod

rename thval_bank aj_intrstass_bank_recod
rename thval_bond aj_intrstass_oth_recod
rename thval_stmf aj_stocks_mutfnds_recod

rename thdebt_ast  dj_total_recod
rename thdebt_home dj_home_recod
rename thdebt_usec dj_unsec_recod
rename thdebt_sec  dj_sec_recod

tostring persid, replace
destring hhadressid, replace
destring industry1, replace
destring industry2, replace
destring occupation1, replace
destring occupation2, replace
destring state, replace

save sip14_merged.dta, replace

**************************************************************
********************* APPEND ALL PANELS **********************
**************************************************************

global path ""

use "$path/1996/sip96_merged.dta", clear

foreach num of numlist 1 4 8 {
append using "$path/200`num'/sip0`num'_merged.dta"
}
append using "$path/2014/sip14_merged.dta"

*** drop some variables to reduce size of dataset

** drop variables only available in 2014 
drop aj_cds aj_check aj_mnymrkt aj_savings ///
	ai_cds ai_check ai_mnymrkt ai_savings aj_equ_othrealest_wspouse ///
	aj_equ_othrealest_wospouse aj_equ_othrealest_own aj_boat ///
	aj_mtrcycle aj_recrveh aj_rv dj_boat dj_mtrcycle dj_recrveh ///
	dj_rv aj_bonds_gov aj_bonds_municipal aj_mutfnds /// 
	aj_stocks ai_mutfnds ai_stocks aj_equ_othrealest_recod ///
	aj_equ_othrent_recod race_detail
	
** recode person ID


destring persid, replace

save "$path/sipp_data_merged.dta", replace

/* ai_checking aj_checking aj_savacc ai_savacc di_loanbank ///
dj_loanbank di_ccardbill dj_ccardbill ai_IRA ai_KEOGH ///
ai_401k ai_bus1_share ai_bus2_share ai_valbus1_befdebt ///
ai_valbus2_befdebt ai_businesssale di_bus1 di_bus2 di_mortage ///
dj_mortage dj_mortgs_loans_all dj_frstscnd_mortg_orig ///
aj_propertyvalue aj_equ_othrealest aj_rentprop aj_rentprop_nospous_totval ///
ai_rentprop aj_rentprop_nospous_sharval dj_rentpropdj_rentprop_nospouse ///
di_rentprop aj_mobile dj_mobile aj_vec1 aj_vec2 aj_vec3 aj_othvec1 aj_othvec2 ///
dj_vec1 dj_vec2 dj_vec3 dj_othvec1 dj_othvec2 aj_bonds ai_bonds ai_savbond ///
aj_stocks_mutfnds ai_stocks_mutfnds dj_stocks_mutfnds di_stocks_mutfnds ///
ai_lifins_empl ai_lifins_all ai_equ_othass di_othdebt dj_othdebt ///
aj_networth_recod aj_wealth_recod aj_homeequity_recod aj_busequity_recod ///
aj_netequ_vec_recod aj_equ_othass_recod aj_IRA_KEOGH_recod ///
aj_equ_realest_nohome_recod aj_intrstass_bank_recod aj_intrstass_oth_recod ///
aj_stocks_mutfnds_recoddj_total_recoddj_home_recoddj_unsec_recoddj_sec_recod */



