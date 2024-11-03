********************************************************************************
* Joint Search over the Life Cycle
* Annika Bacher, Philipp Grübener, Lukas Nord
* CPS data preparation and analysis
********************************************************************************

cls
clear
clear matrix
clear mata
macro drop _all
set more off

********************************************************************************
* Set globals
********************************************************************************

do 0_globals.do

********************************************************************************
* Load data
********************************************************************************

* Data is Current Population Survey of the Bureau of Labor Statistics
* Data downloaded from IPUMS on August 22, 2024

cd "$datapath"

use cps_jslc.dta, clear
format cpsidp %14.0f
format cpsid %14.0f
format cpsidv %15.0f

* Check for duplicates
duplicates report cpsidv year month

********************************************************************************
* Declare as panel
********************************************************************************

gen modate = ym(year, month) 
xtset cpsidv modate

********************************************************************************
* Sample selection and preparation
********************************************************************************

cd "$dopath"
do sample_preparation.do

********************************************************************************
* Calibration/validation targets
********************************************************************************

use "$datapath/cps_prepared_sample.dta", clear

do calib_valid.do

********************************************************************************
* Transition matrices
********************************************************************************

use "$datapath/cps_prepared_sample.dta", clear

do transition_probs.do

********************************************************************************
* Regressions
********************************************************************************

use "$datapath/cps_prepared_sample.dta", clear

do regressions.do






















