# Data
The folder “Data” contains codes to load data from the Current Population Survey (CPS)
and the Survey of Income and Program Participation (SIPP) and to compute the empirical
results reported in the paper.

## CPS
We obtain CPS data from IPUMS. Specifically, we use IPUMS CPS: Version
11.0 (Flood et al. 2023). The exact dataset we use can be downloaded by running the
file download_ipums_cps.do, which requires having an IPUMS account, an IPUMS API
key, and Python integration in Stata (See https://blog.popdata.org/making-ipums-extracts-from-stata/ for details.)
The API key has to be inserted in line 24 of the
program. Alternatively, the data can be downloaded directly from the IPUMS homepage –
the IPUMS samples to be selected can be read off from line 53 of download_ipums_cps.do
and the variables from line 54.

The empirical results can be produced by running cps_main.do, which calls the remaining
do-files. This requires setting paths in 0_globals.do. datapath has to specify
where the dataset is stored, dopath is the location of the do-files, and figurepath is the location
where output will be stored. In the file regressions.do, we additionally merge state
level unemployment rates obtained from FRED, which we include in the same folder as
the codes. This data file has to be stored in datapath.

## SIPP 
SIPP data can be downloaded from the NBER and the Census using the file
download_SIPP.R. The file SIPP_Merge.do extracts the relevant variables and merges
different waves – paths have to be set in this file, depending on where the data is stored,
in lines 13, 326, 651, 964, 1297, and 1705. The file SIPP_AWE.do produces the reported
results using the merged dataset from the previous steps, for which the location has to
be specified in line 20.

# Model
The model codes are written in Fortran and can be compiled as follows: gfortran
-fopenmp -ffree-line-length-none -mcmodel=large Main.f90 -o Main.exe. For the policy experiments
the parameter ϕUS has to be adjusted in line 76 of Globals.f90.

This will produce a number of files that are stored in the Output subfolder. Running "Model_Outputs.m" will produce the majority of the figures in the paper. It requires to set paths to the model solutions for the baseline calibration and the counterfactuals in the beginning. To get the dynamic AWE estimates, first the Matlab code "AWE_Leads_Lags.m" has to be run, setting the age group dummy in line 31 to 1 for the young and to 4 for the old. Afterwards, the figures are produced by running in Stata "Model_Leads_Lags_Young.do" and "Model_Leads_Lags_Old.do."

# References
Flood, Sarah, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel
Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry (2023). “IPUMS CPS: Version 11.0 [dataset]. Minneapolis, MN:
IPUMS, 2023.” Dataset.
