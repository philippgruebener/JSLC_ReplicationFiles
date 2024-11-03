********************************************************************************
* Joint Search over the Life Cycle
* Data Download
* This do-file closely builds on the example provided by Renae Rodgers at https://blog.popdata.org/making-ipums-extracts-from-stata/
* IPUMS sample names from: # https://cps.ipums.org/cps-action/samples/sample_ids
********************************************************************************

clear
clear matrix
clear mata
macro drop _all
set more off
set maxvar 10000

********************************************************************************
* Prepare environment
* For this to work, steps described here have to be done before: https://blog.popdata.org/making-ipums-extracts-from-stata/
********************************************************************************

* Choose Python environment
python query

* Input your IPUMS API key
global MY_API_KEY ""

********************************************************************************
* Python
********************************************************************************

python:

# import necessary libraries
import gzip
import shutil

from ipumspy import IpumsApiClient, CpsExtract
from sfi import Macro

# retrieve live api key from the global macro defined in user's profile.do file
# this is the best substitute I have come up with for a conda environment variable
# in a stata context
my_api_key = Macro.getGlobal("MY_API_KEY")

ipums = IpumsApiClient(my_api_key)

# Define
#	1. An IPUMS data collection
#	2. A list of sample IDs
#	3. A list of variables
#	4. An extract description

ipums_collection="cps"
samples = ["cps1994_01b","cps1994_02s","cps1994_03b","cps1994_04b","cps1994_05b","cps1994_06s","cps1994_07b","cps1994_08b","cps1994_09s","cps1994_10s","cps1994_11s","cps1994_12s","cps1995_01b","cps1995_02s","cps1995_03b","cps1995_04s","cps1995_05s","cps1995_06s","cps1995_07b","cps1995_08s","cps1995_09s","cps1995_10s","cps1995_11b","cps1995_12b","cps1996_01s","cps1996_02s","cps1996_03b","cps1996_04b","cps1996_05s","cps1996_06b","cps1996_07b","cps1996_08b","cps1996_09s","cps1996_10s","cps1996_11s","cps1996_12b","cps1997_01b","cps1997_02s","cps1997_03b","cps1997_04s","cps1997_05s","cps1997_06b","cps1997_07b","cps1997_08b","cps1997_09s","cps1997_10s","cps1997_11b","cps1997_12b","cps1998_01b","cps1998_02s","cps1998_03b","cps1998_04b","cps1998_05b","cps1998_06s","cps1998_07b","cps1998_08s","cps1998_09s","cps1998_10s","cps1998_11s","cps1998_12s","cps1999_01s","cps1999_02s","cps1999_03b","cps1999_04s","cps1999_05s","cps1999_06b","cps1999_07b","cps1999_08b","cps1999_09s","cps1999_10s","cps1999_11b","cps1999_12b","cps2000_01s","cps2000_02s","cps2000_03b","cps2000_04b","cps2000_05s","cps2000_06s","cps2000_07b","cps2000_08s","cps2000_09s","cps2000_10s","cps2000_11s","cps2000_12b","cps2001_01b","cps2001_02s","cps2001_03b","cps2001_04s","cps2001_05s","cps2001_06s","cps2001_07b","cps2001_08s","cps2001_09s","cps2001_10s","cps2001_11s","cps2001_12s","cps2002_01s","cps2002_02s","cps2002_03b","cps2002_04b","cps2002_05b","cps2002_06s","cps2002_07b","cps2002_08s","cps2002_09s","cps2002_10s","cps2002_11s","cps2002_12s","cps2003_01b","cps2003_02s","cps2003_03b","cps2003_04b","cps2003_05b","cps2003_06s","cps2003_07b","cps2003_08s","cps2003_09s","cps2003_10s","cps2003_11s","cps2003_12s","cps2004_01s","cps2004_02b","cps2004_03b","cps2004_04b","cps2004_05s","cps2004_06s","cps2004_07b","cps2004_08b","cps2004_09s","cps2004_10s","cps2004_11s","cps2004_12s","cps2005_01s","cps2005_02s","cps2005_03b","cps2005_04b","cps2005_05s","cps2005_06b","cps2005_07s","cps2005_08s","cps2005_09s","cps2005_10s","cps2005_11s","cps2005_12s","cps2006_01s","cps2006_02b","cps2006_03b","cps2006_04b","cps2006_05s","cps2006_06s","cps2006_07b","cps2006_08s","cps2006_09s","cps2006_10s","cps2006_11s","cps2006_12s","cps2007_01s","cps2007_02b","cps2007_03b","cps2007_04b","cps2007_05b","cps2007_06b","cps2007_07b","cps2007_08s","cps2007_09s","cps2007_10s","cps2007_11b","cps2007_12s","cps2008_01s","cps2008_02b","cps2008_03b","cps2008_04b","cps2008_05s","cps2008_06s","cps2008_07b","cps2008_08s","cps2008_09s","cps2008_10s","cps2008_11s","cps2008_12s","cps2009_01s","cps2009_02b","cps2009_03b","cps2009_04b","cps2009_05b","cps2009_06b","cps2009_07b","cps2009_08s","cps2009_09s","cps2009_10s","cps2009_11s","cps2009_12s","cps2010_01s","cps2010_02b","cps2010_03b","cps2010_04b","cps2010_05s","cps2010_06s","cps2010_07s","cps2010_08s","cps2010_09s","cps2010_10s","cps2010_11s","cps2010_12s","cps2011_01s","cps2011_02b","cps2011_03b","cps2011_04b","cps2011_05s","cps2011_06s","cps2011_07s","cps2011_08s","cps2011_09s","cps2011_10s","cps2011_11s","cps2011_12s","cps2012_01s","cps2012_02b","cps2012_03b","cps2012_04b","cps2012_05s","cps2012_06s","cps2012_07s","cps2012_08s","cps2012_09s","cps2012_10s","cps2012_11s","cps2012_12s","cps2013_01b","cps2013_02s","cps2013_03b","cps2013_04b","cps2013_05b","cps2013_06s","cps2013_07s","cps2013_08s","cps2013_09s","cps2013_10s","cps2013_11s","cps2013_12s","cps2014_01s","cps2014_02s","cps2014_03b","cps2014_04b","cps2014_05b","cps2014_06s","cps2014_07s","cps2014_08s","cps2014_09s","cps2014_10s","cps2014_11s","cps2014_12s","cps2015_01s","cps2015_02s","cps2015_03b","cps2015_04b","cps2015_05s","cps2015_06s","cps2015_07s","cps2015_08s","cps2015_09s","cps2015_10s","cps2015_11b","cps2015_12s","cps2016_01s","cps2016_02s","cps2016_03b","cps2016_04b","cps2016_05b","cps2016_06s","cps2016_07b","cps2016_08s","cps2016_09s","cps2016_10s","cps2016_11s","cps2016_12s","cps2017_01b","cps2017_02s","cps2017_03b","cps2017_04b","cps2017_05s","cps2017_06s","cps2017_07s","cps2017_08s","cps2017_09s","cps2017_10s","cps2017_11s","cps2017_12s","cps2018_01s","cps2018_02s","cps2018_03b","cps2018_04b","cps2018_05s","cps2018_06s","cps2018_07s","cps2018_08s","cps2018_09s","cps2018_10s","cps2018_11s","cps2018_12s","cps2019_01s","cps2019_02s","cps2019_03b","cps2019_04b","cps2019_05s","cps2019_06s","cps2019_07s","cps2019_08s","cps2019_09s","cps2019_10s","cps2019_11s","cps2019_12s","cps2020_01s","cps2020_02s","cps2020_03b","cps2020_04b"]
variables = ["YEAR", "SERIAL", "MONTH","HWTFINL","CPSID","MISH","NUMPREC","STATEFIP","METAREA","STATECENSUS","METFIPS","FAMINC","PERNUM","WTFINL","CPSIDP","CPSIDV","RELATE","AGE","SEX","RACE","MARST","SPLOC","NCHILD","NCHLT5","EMPSTAT","LABFORCE","IND1990","IND","IND1950","CLASSWKR","DURUNEMP","WHYUNEMP","WNFTLOOK","WKSTAT","MULTJOB","NUMJOB", "NILFACT","EDUC","EARNWT","PANLWT","HOURWAGE","PAIDHOUR","EARNWEEK","UHRSWORKORG"]
extract_description = "CRNYU_CPS_EXTRACT"

# use all of this info to create a UsaExtract object
extract = CpsExtract(samples,variables,description=extract_description)
		     
# submit your extract to the IPUMS extract system
ipums.submit_extract(extract)

# wait for the extract to finish
ipums.wait_for_extract(extract, collection=ipums_collection)

# when the extract is finished, download it to your current working directory
ipums.download_extract(extract, stata_command_file=True)

# store the extract project and extract id as local macros
# for later use when reading in data
Macro.setLocal("id", str(extract.extract_id).zfill(5))
Macro.setLocal("collection", extract.collection)

# unzip the extract data file
with gzip.open(f"{ipums_collection}_{str(extract.extract_id).zfill(5)}.dat.gz", 'rb') as f_in:
    with open(f"{ipums_collection}_{str(extract.extract_id).zfill(5)}.dat", 'wb') as f_out:
        shutil.copyfileobj(f_in, f_out)

# exit python
end

* clear the python info*
python clear

* now we should see a data file and a ddi file in our current working directory
ls

* now we can run the do file and read the ipums extract into stata!
qui do `collection'_`id'.do

* Save the data

