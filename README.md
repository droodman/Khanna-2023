# Khanna (2023) reanalysis data and code

This archive holds the data and code used in the [reanalysis](https://arxiv.org/abs/2303.11956) of Khanna (2023), "[Large-Scale Education Reform in General
Equilibrium: Regression Discontinuity Evidence from India](https://doi.org/10.1086/721619)."

It also contains a response to comments from Gaurav Khanna and anonymous reviewers--see [Roodman response to comments.pdf](https://github.com/droodman/Khanna-2023/blob/main/Roodman%20response%20to%20comments.pdf).

## Code
The code consists of a single Stata .do file, `code/K23 2.do`. (K23.do is for the first version of the comment.) The code requires these user-written packages, which are available through Stata's `ssc install` command unless otherwise noted:
* `coefplot`
* `esttab`
* `spmap`
* `shp2dta`
* `blindschemes`
* `grc1leg2` (install in Stata with `net install grc1leg2, from(http://digital.cgdev.org/doc/stata/MO/Misc)`)
* `rdrobust` (install my tweaked version with `net install rdrobust, from(https://raw.github.com/droodman/rdrobust/master/stata) replace`; my tweaks prevent rdplot from crashing when passed labels in quotes, and prevents rdbwselect_2014 from crashing by including some missing Mata files)
* `parallel` (install my tweaked version with `net install parallel, from(https://raw.github.com/droodman/parallel/master) replace`; it distributes workload slightly more evenly across CPU cores)

## Output
The output folder contains figures and tables produced by the code, in PNG and RTF formats.

## Data
The data files:

1. 2009_Distdb.dta and 2009_Distcoord.dta. These are used by the spmap command to make the maps. Commented-out lines in K23.do use the shp2dta command to make these files from the [2009 district shapefile for India hosted by IPUMS International](https://international.ipums.org/international/gis_yrspecific_2nd.shtml).
2. "district info.xlsx". This spreadsheet contains three tabs. The first two document changes in district boundaries. (In India districts often subdivide, and occasionally recombine. This reality poses challenges to researchers wanting to link district-level data sets from different time points.) The "District changes 1991-2001" tab was extracted from [Kumar and Somanathan (2016)](http://www.cdedse.org/pdf/work248.pdf), Tables 7, 8, and 9d. I constructed the "District changes 2001-09" mainly by viewing Wikepedia pages for various districts. Finally, the "DPEP distrcts" tab lists the districts that participated in the District Primary Education Programme according to [this written answer from a minister to a question posed by a member of parliament](https://datais.info/loksabha/question/db0cac20ad912c779f1de1c7b7fd60f3/DISTRICT+PRIMARY+EDUCATION+PROGRAMME)
3. "K23 district-level.dta" contains district-level variables such as female literacy in 1991 and DPEP treatment status, from both the new data set and the Khanna (2023) one. Variables from the Khanna (2023) data are prefixed with "K23_".
4. "K23 individual-level.dta.zip" holds the individual-level [data from the 66th National Sample Survey](http://microdata.gov.in/nada43/index.php/catalog/124/data_dictionary#page=F4&tab=data-dictionary), again in both new and Khanna (2023) variants. It also incorporates the district-level variables, carried forward to the districts as of 2009--10. (NSS data is free with registration.)

The Khanna (2023) data and code are available through the [_Journal of Political Economy_](https://doi.org/10.1086/721619) (gated).

The primary data sources were transformed into the analysis data sets in items 3 and 4 using SQL Server. The database is too large for my Github account, at 1GB. A backup is [here](https://1drv.ms/u/s!Avm4GrhZKgeal8kzGkJvZdmClVUdxg?e=MXZq7U).

