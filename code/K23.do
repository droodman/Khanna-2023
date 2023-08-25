* code for first public version of the comment

// dependencies (on SSC unless otherwise noted):
// 	 coefplot
// 	 esttab
// 	 spmap
// 	 shp2dta
// 	 grc1leg2 (net install grc1leg2, from(http://digital.cgdev.org/doc/stata/MO/Misc))
//   blindschemes
// 	 rdrobust (tweaked rdplot to not strip quote marks from test labels and rdbwselect_2014 to not crash, "net install rdrobust, from(https://raw.github.com/droodman/rdrobust/master/stata) replace")
// 	 parallel (tweaked to distribute work more evenly across cores, "net install parallel, from(https://raw.github.com/droodman/parallel/master) replace")

cd "D:\OneDrive\Documents\Work\Clients & prospects\GiveWell\Education\Khanna\Public"
global shapefilepath D:\OneDrive\Documents\Work\DevDat\India NSS\District shape file 2009  // https://international.ipums.org/international/gis_yrspecific_2nd.shtml
graph set window fontface "LM Roman 9"  // https://www.1001fonts.com/latin-modern-roman-font.html
set scheme plotplain

// code for inserting NSS files into SQL Server. Requires an old Stata version.
// foreach file in `:dir NSS66 files "*.dta"' {
//   use "NSS66/`file'", clear
//   destring *, replace
//   compress
//   odbc insert, dsn(Khanna) table(`=substr("`file'", 1, strlen("`file'")-4)') create
// }


***
*** distict-level maps & stats
***
{
  *  one-time conversion of IPUMS shapefile (international.ipums.org/international/gis_yrspecific_2nd.shtml) to Stata format 
  // shp2dta using "$shapefilepath\geo2_in2009", database($shapefilepath\2009_Distdb) coordinates($shapefilepath\2009_Distcoord) genid(DistrictID) replace
  // use "$shapefilepath\2009_Distdb", clear
  // destring DIST2009, replace
  // save "$shapefilepath\2009_Distdb", replace

  // odbc load, clear dsn(Khanna) exec("SELECT SUM(F_LITERATE)/CAST(SUM(T_F_POPLN-POPLN_F6) AS real) as fl FROM Census1991.[District Primary Census Abstracts]")
  // scalar National_Average_FL = fl[1]  // should be .39286956; also see http://14.139.60.153/bitstream/123456789/1042/1/Census%201991_District%20Literacy_D-10761.pdf#page=4
  scalar National_Average_FL = .39286956

  * district-level stats
  odbc load, exec("SELECT DISTINCT DISTRICT_CODE as DIST2009, DPEP, K23_DPEP, K23_NSS_Sample, Female_Literacy, K23_Female_Literacy, FL_SD FROM Datasets") dsn(Khanna) clear
  saveold "data/K23 district-level", version(13) replace
  use "data/K23 district-level", clear
  merge 1:1 DIST2009 using "$shapefilepath\2009_Distdb", keep(master match using)

  recode DPEP (.5 = .)  // Baksa, Assam, has mixed parentage; code program status as missing

  gen byte NSS_Sample = 1 if _merge != 2
  replace K23_NSS_Sample = . if K23_NSS_Sample==0
  gen byte     T = Female_Literacy < National_Average_FL if Female_Literacy<. & FL_SD<.03  // intent to treat
  gen byte K23_T = K23_Female_Literacy < .39268 if K23_Female_Literacy<.

  spmap K23_T          using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapTOrig, replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data") lab(2 "Above literacy cap") lab(3 "Below literacy cap"))
  spmap     T          using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapTNew , replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapTOrig mapTNew, name(Tmaps, replace) graphregion(margin(zero)) imargin(zero) title(Intention to treat, pos(11) margin(zero)) pos(3)

  spmap K23_DPEP       using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapDOrig, replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))             legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data") lab(2 "No DPEP program") lab(3 "DPEP program"))
  spmap     DPEP       using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapDNew , replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapDOrig mapDNew, name(Dmaps, replace) graphregion(margin(zero)) imargin(zero) title(Treatment, pos(11) margin(zero)) pos(3)

  spmap K23_NSS_Sample using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapNSSOrig, replace) title(Original data set, size(vlarge) pos(6) span) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data                ") lab(2 "Data"))
  spmap     NSS_Sample using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapNSSNew , replace) title(New data set     , size(vlarge) pos(6) span) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapNSSOrig mapNSSNew, name(NSSmaps, replace) graphregion(margin(zero)) imargin(zero) title(Follow-up, pos(11) margin(zero)) pos(3)

  replace K23_NSS_Sample = 2 if K23_NSS_Sample==. & NSS_Sample<.
  spmap K23_NSS_Sample using "$shapefilepath\2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(pos(5) size(medsmall) margin(zero) bmargin(zero) lab(1 "No data                ") lab(2 "Complete data, used in K23") lab(3 "Complete data, not used in K23"))
  graph export output/mapNSSOrig.png, height(1552) replace  // for blog

  graph combine Tmaps Dmaps NSSmaps, imargin(zero) graphregion(margin(zero)) cols(1) xsize(7) ysize(8)
  graph export output/maps.png, replace width(2000)

  tab2 T    K23_T   , missing  // stats on (dis)agreement between original and new program assignment variables
  tab2 DPEP K23_DPEP, missing

  * first row of rdplots
  rdplot K23_DPEP K23_Female_Literacy, p(2) nbins(18 22) c(.39268) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Fraction getting DPEP funding) ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(1 .39268 "Eligibility threshold (39.29%)", place(se)))
  graph export output/DPEPOrig.png, height(1552) replace  // for blog
  rdplot K23_DPEP K23_Female_Literacy, c(.39268) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Fraction getting DPEP funding) ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(1 .39268 "Eligibility threshold (39.29%)", place(se)))
  graph export output/DPEPdefault.png, height(1552) replace  // for blog

  rdplot K23_DPEP K23_Female_Literacy, p(2) nbins(18 22) graph_options(                 legend(off)  graphregion(margin(zero)) nodraw ylab(,format(%3.1f)) name(DPEPOrig, replace) title(Original)) c(.39268)
  rdplot K23_DPEP K23_Female_Literacy, p(2) nbins(18 22) graph_options(xscale(off fill) legend(off)  graphregion(margin(zero)) nodraw ylab(,format(%3.1f)) yscale(range(-.2 1)) name(DPEPOrig2, replace) title("Original, rescaled")) c(.39268)
  rdplot K23_DPEP K23_Female_Literacy,                   graph_options(xscale(off fill) legend(off)  graphregion(margin(zero)) nodraw name(DPEPdefault, replace) title("Original, rdplot defaults") ylab(0(.5)1) yscale(off fill)) c(.39268)            
  rdplot     DPEP     Female_Literacy,                   graph_options(xscale(off fill) legend(off)  graphregion(margin(zero)) nodraw name(DPEPnew    , replace) title("New data, rdplot defaults") ylab(0(.5)1) yscale(off fill)) c(`=National_Average_FL')
  graph combine DPEPOrig DPEPOrig2 DPEPdefault DPEPnew, rows(1) l1title(DPEP treatment, size(small)) graphregion(margin(zero)) nodraw name(DPEP, replace)
}


***
*** Prep individual-level data
***
cap program drop PrepData
program define PrepData
//   odbc load, clear dsn(Khanna) exec("SELECT SUM(F_LITERATE)/CAST(SUM(T_F_POPLN-POPLN_F6) AS real) as fl FROM Census1991.[District Primary Census Abstracts]")
//   scalar National_Average_FL = fl[1]  // should be .39286956; also see http://14.139.60.153/bitstream/123456789/1042/1/Census%201991_District%20Literacy_D-10761.pdf#page=4
  scalar National_Average_FL = .39286956

  cap odbc load, clear dsn(Khanna) exec("select * from Datasets where Age between 17 and 100")
  if !_rc saveold "data/K23 individual-level", version(13) replace
  use "data/K23 individual-level", clear

  recode DPEP (.5 = .)  // Baksa, Assam, has mixed parentage; code program status as missing

  foreach p in "" K23_ {
    gen `p'lnWages  = ln(`p'Wages)   // all wages
    gen `p'lnWages1 = ln(`p'Wages1)  // wages from first-listed activity
  }
  recode General_Education                       (5 = 3) (6 = 5) (7 = 8) (8 = 10) (10 = 12) (11 = 14) (12 = 16) (13 = 18) (nonmiss = 0), gen(Years_Schooling)  // based partly on DOI 10.1177/1464993417716357, p. 4; see http://www.icssrdataservice.in/datarepository/index.php/catalog/89/datafile/F4/V161
  recode K23_General_Education (2 = 1) (3 4 = 2) (5 = 3) (6 = 5) (7 = 8) (8 = 10) (10 = 12) (11 = 14) (12 = 16) (13 = 18) (nonmiss = 0), gen(K23_Years_Schooling)  // assigns some schooling to "without formal schooling"

  gen byte single_parent = FL_SD < .01  // was .03; really districts of relatively unmixed parentage, because they come mostly from one parent, or literacy doesn't vary much across parents
  gen double lnWeekly_Wages = lnWages - ln(Days_worked / 7)  // meaning wage rate rather than total wages
  gen byte Skilled = Years_Schooling>=8

  sum WEIGHT, detail
  gen WTtr = min(WEIGHT, r(p50)+5*(r(p75)-r(p50)))  // trim extreme weights to median + 5 * IQR (Potter and Zheng 2015)
end

PrepData

* compare original and revised variables
corr K23_lnWages1 lnWeekly_Wages
corr K23_Age Age
corr K23_Female_Literacy Female_Literacy
corr K23_Years_Schooling Years_Schooling

* document 4 absent districts
{
  preserve
  collapse Female_Literacy DPEP if Years_Schooling<. & K23_Years_Schooling==. & single_parent, by(DISTRICT_CODE)
  sort Female_Literacy
  list
  restore
}

* finish rdplots
{
  PrepData
  rdplot K23_Years_Schooling K23_Female_Literacy if K23_Age<=35 & K23_Wages1   <. & abs(K23_Female_Literacy-.39268)<.2, nbins(6 7) p(2) c(.39268)                 graph_options(legend(off) xscale(off fill) graphregion(margin(2 0 0 0)) nodraw name(SchoolingOrig, replace))
  rdplot K23_Years_Schooling K23_Female_Literacy if K23_Age<=35 & K23_Wages1   <. & abs(K23_Female_Literacy-.39268)<.2, nbins(6 7) p(2) c(.39268)                 graph_options(legend(off) xlab(0(.2)1) xscale(off fill range(0 1)) ylab(0(5)15, format(%2.0f)) yscale(range(0 15)) graphregion(margin(zero)) nodraw name(SchoolingOrig2, replace))
  rdplot K23_Years_Schooling K23_Female_Literacy if K23_Age<=35 & K23_Wages1   <.                                     ,                 c(.39268)                 graph_options(legend(off) xscale(off fill) yscale(off fill) graphregion(margin(zero)) nodraw name(Schoolingdefault, replace))
  rdplot     Years_Schooling     Female_Literacy if     Age<=35 & lnWeekly_Wages<.                                     , weights(WTtr) c(`=National_Average_FL') graph_options(legend(off) xscale(off fill) yscale(off fill) graphregion(margin(zero)) nodraw name(Schoolingnew, replace))
  graph combine SchoolingOrig SchoolingOrig2 Schoolingdefault Schoolingnew, rows(1) l1title("Years of schooling, age≤35", size(small)) graphregion(margin(zero)) nodraw name(Schooling, replace)

  rdplot K23_Wages1 K23_Female_Literacy if K23_Age<=35 & abs(K23_Female_Literacy-.39268)<.2, nbins(8 8) p(2) c(.39268) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(.2 "20%" .4 "40%" .6 "60%") text(1400 .39268 "Eligibility threshold (39.29%)", place(se)))
  graph export output/WagesOrig.png, height(1552) replace  // for blog
  rdplot K23_Wages K23_Female_Literacy if K23_Age<=35, c(.39268) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(5000 .39268 "Eligibility threshold (39.29%)", place(se)))
  graph export output/Wagesdefault.png, height(1552) replace  // for blog
  rdplot Wages Female_Literacy if Age<=35, c(.39268) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(5000 .39268 "Eligibility threshold (39.29%)", place(se)))
  graph export output/WagesNew.png, height(1552) replace  // for blog

  rdplot K23_lnWages1       K23_Female_Literacy if K23_Age<=35 & abs(K23_Female_Literacy-.39268)<.2, nbins(8 8) p(2) c(.39268) graph_options(legend(off) graphregion(margin(zero)) nodraw name(WagesOrig, replace))
  rdplot K23_lnWages1       K23_Female_Literacy if K23_Age<=35 & abs(K23_Female_Literacy-.39268)<.2, nbins(8 8) p(2) c(.39268) graph_options(legend(off) graphregion(margin(zero)) xlab(0(.2)1) xscale(range(0 1)) ylab(6/9) yscale(range(5.8 9)) nodraw name(WagesOrig2, replace))
  rdplot K23_lnWages1       K23_Female_Literacy if K23_Age<=35                                     ,                 c(.39268) graph_options(legend(off) yscale(off fill) graphregion(margin(2 0 0 0)) nodraw name(Wagesdefault, replace))
  rdplot     lnWeekly_Wages      Female_Literacy if     Age<=35                                     , weights(WTtr) c(`=National_Average_FL') graph_options(legend(off) graphregion(margin(zero)) ylab(6/9) yscale(off fill range(5.8 9)) nodraw name(Wagesnew, replace))
  graph combine WagesOrig WagesOrig2 Wagesdefault Wagesnew, rows(1) l1title("Log wage earnings, age≤35", size(small)) graphregion(margin(zero)) nodraw name(Wages, replace)

  graph combine DPEP Schooling Wages, cols(1) graphregion(margin(zero)) b1title("Female Literacy, 1991")
  graph export output/rdplot.png, replace width(2000)
}


* RDD table in text, saved to RDD.rtf
{
  cap program drop robustify
  program define robustify, eclass  // hack: rotate a stored rdrobust result so the "Robust" estimate is in the "Conventional" slot, for easy inclusion in esttab
    tempname b V
    est restore `1'
    mata st_matrix("`b'", st_matrix("e(b)")[3\1\2])
    mata st_matrix("`V'", st_matrix("e(V)")[3\1\2,3\1\2])
    ereturn repost b=`b' V=`V'
    est store `2'
  end

  PrepData

  rdbwselect_2014 K23_Years_Schooling K23_Female_Literacy if K23_Age<35 & K23_lnWages1<., c(.39286)
  scalar h  = round(e(h_CCT), .001)  // original rounds
  scalar bb = round(e(b_CCT), .001)  // unfortunately, rdrobust deletes the scalar b

  cap erase output/RDD.rtf
  forvalues s=1/2 {
    local sample: word `s' of Age<35 Age<=75&Age>35
    local samplename: word `s' of Young Old

    eststo OriginalSchooling`samplename'    : rdrobust K23_Years_Schooling K23_Female_Literacy if K23_`sample' & K23_Wages1<., scalepar(-1) c(.39286) h(`=h') b(`=bb') all
    robustify OriginalSchooling`samplename' OriginalRobSchooling`samplename'
    eststo OriginalAutoSchooling`samplename': rdrobust K23_Years_Schooling K23_Female_Literacy if K23_`sample' & K23_Wages1<., scalepar(-1) c(.39286) vce(hc3) all //  bwselect(cerrd)  // last option new since first arXiv version
    eststo OriginalClSchooling`samplename'  : rdrobust K23_Years_Schooling K23_Female_Literacy if K23_`sample' & K23_Wages1<., scalepar(-1) c(.39286) h(`=h') b(`=bb') all vce(cluster K23_Female_Literacy)
    eststo OriginalWtSchooling`samplename'  : rdrobust K23_Years_Schooling K23_Female_Literacy if K23_`sample' & K23_Wages1<., scalepar(-1) c(.39286) h(`=h') b(`=bb') all weights(WTtr)
    eststo RevisedNo4Schooling`samplename'  : rdrobust Years_Schooling Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent & !inlist(DISTRICT_CODE,1402,2719,2728,3318), scalepar(-1) c(`=National_Average_FL') h(`=h') b(`=bb') all
    eststo RevisedSchooling`samplename'     : rdrobust Years_Schooling Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent, scalepar(-1) c(`=National_Average_FL') h(`=h') b(`=bb') all
    eststo AllNowtSchooling`samplename'     : rdrobust Years_Schooling Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent, scalepar(-1) c(`=National_Average_FL') all vce(cluster Female_Literacy) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllNowtSchooling`samplename' AllNowtRobSchooling`samplename'
    eststo AllSchooling`samplename'         : rdrobust Years_Schooling Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent, scalepar(-1) c(`=National_Average_FL') all vce(cluster Female_Literacy) weights(WTtr) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllSchooling`samplename' AllRobSchooling`samplename'

    eststo OriginalWages`samplename'    : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', scalepar(-1) c(.39286) h(`=h') b(`=bb') all
    robustify OriginalWages`samplename' OriginalRobWages`samplename'
    eststo OriginalAutoWages`samplename': rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', scalepar(-1) c(.39286) vce(hc3) all //  bwselect(cerrd)  // last option new since first arXiv version
    eststo OriginalClWages`samplename'  : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', scalepar(-1) c(.39286) h(`=h') b(`=bb') all vce(cluster K23_Female_Literacy)
    eststo OriginalWtWages`samplename'  : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', scalepar(-1) c(.39286) h(`=h') b(`=bb') all weights(WTtr)
    eststo RevisedNo4Wages`samplename'  : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent & !inlist(DISTRICT_CODE,1402,2719,2728,3318), scalepar(-1) c(`=National_Average_FL') h(`=h') b(`=bb') all
    eststo RevisedWages`samplename'     : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, scalepar(-1) c(`=National_Average_FL') h(`=h') b(`=bb') all
    eststo AllNowtWages`samplename'     : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, scalepar(-1) c(`=National_Average_FL') all vce(cluster Female_Literacy) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllNowtWages`samplename' AllNowtRobWages`samplename'
    eststo AllWages`samplename'         : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, scalepar(-1) c(`=National_Average_FL') all vce(cluster Female_Literacy) weights(WTtr) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllWages`samplename' AllRobWages`samplename'

    eststo OriginalFRDD`samplename'    : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', c(.39286) h(`=h') b(`=bb') all fuzzy(K23_Years_Schooling)
    robustify OriginalFRDD`samplename' OriginalRobFRDD`samplename'
    eststo OriginalAutoFRDD`samplename': rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', c(.39286) all vce(hc3) fuzzy(K23_Years_Schooling) //  bwselect(cerrd)  // last option new since first arXiv version
    eststo OriginalClFRDD`samplename'  : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', c(.39286) h(`=h') b(`=bb') all vce(cluster K23_Female_Literacy) fuzzy(K23_Years_Schooling)
    eststo OriginalWtFRDD`samplename'  : rdrobust K23_lnWages1 K23_Female_Literacy if K23_`sample', c(.39286) h(`=h') b(`=bb') all weights(WTtr) fuzzy(K23_Years_Schooling)
    eststo RevisedNo4FRDD`samplename'  : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent & !inlist(DISTRICT_CODE,1402,2719,2728,3318), c(`=National_Average_FL') h(`=h') b(`=bb') all fuzzy(Years_Schooling)
    eststo RevisedFRDD`samplename'     : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, c(`=National_Average_FL') h(`=h') b(`=bb') all fuzzy(Years_Schooling)
    eststo AllNowtFRDD`samplename'     : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, c(`=National_Average_FL') all vce(cluster Female_Literacy) fuzzy(Years_Schooling) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllNowtFRDD`samplename' AllNowtRobFRDD`samplename'
    eststo AllFRDD`samplename'         : rdrobust lnWeekly_Wages Female_Literacy if `sample'-(`s'-1) & single_parent, c(`=National_Average_FL') all vce(cluster Female_Literacy) weights(WTtr) fuzzy(Years_Schooling) //  bwselect(cerrd)  // last option new since first arXiv version
    robustify AllFRDD`samplename' AllRobFRDD`samplename'
    
    estadd scalar N_h = e(N_h_l) + e(N_h_r): OriginalSchooling`samplename' OriginalRobSchooling`samplename' OriginalAutoSchooling`samplename' OriginalClSchooling`samplename' OriginalWtSchooling`samplename' RevisedNo4Schooling`samplename' RevisedSchooling`samplename' AllNowtRobSchooling`samplename' AllRobSchooling`samplename' ///
                                             OriginalWages`samplename'     OriginalRobWages`samplename'     OriginalAutoWages`samplename'     OriginalClWages`samplename'     OriginalWtWages`samplename'     RevisedNo4Wages`samplename'     RevisedWages`samplename'     AllNowtRobWages`samplename'     AllRobWages`samplename'     ///
                                             OriginalFRDD`samplename'      OriginalRobFRDD`samplename'      OriginalAutoFRDD`samplename'      OriginalClFRDD`samplename'      OriginalWtFRDD`samplename'      RevisedNo4FRDD`samplename'      RevisedFRDD`samplename'      AllNowtRobFRDD`samplename'      AllRobFRDD`samplename'       

    local mtitles = cond(`s'==1, `"mtitles("Original" "Robust CI" "Regression-specific BW" "Clustered" "Sampling weights" "Revised data x 4 absent districts" "Revised data, all districts" "All changes but weights" "All changes")"', "nomtitles")
    esttab OriginalSchooling`samplename' OriginalRobSchooling`samplename' OriginalAutoSchooling`samplename' OriginalClSchooling`samplename' OriginalWtSchooling`samplename' RevisedNo4Schooling`samplename' RevisedSchooling`samplename' AllNowtRobSchooling`samplename' AllRobSchooling`samplename' using output/RDD.rtf, append style(tab) keep(Conventional) varlabel(Conventional "Estimate") stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes          nogaps star(* .1 ** .05 *** .01) nolines paren noobs `mtitles' fonttbl(\f0\fnil LM Roman 9;) msign("–") b(%5.3g) se(%5.3g) 
    esttab OriginalWages`samplename'     OriginalRobWages`samplename'     OriginalAutoWages`samplename'     OriginalClWages`samplename'     OriginalWtWages`samplename'     RevisedNo4Wages`samplename'     RevisedWages`samplename'     AllNowtRobWages`samplename'     AllRobWages`samplename'     using output/RDD.rtf, append style(tab) keep(Conventional) varlabel(Conventional "Estimate") stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes nonumber nogaps star(* .1 ** .05 *** .01) nolines paren noobs nomtitles fonttbl(\f0\fnil LM Roman 9;) msign("–") b(%5.3g) se(%5.3g) 
    esttab OriginalFRDD`samplename'      OriginalRobFRDD`samplename'      OriginalAutoFRDD`samplename'      OriginalClFRDD`samplename'      OriginalWtFRDD`samplename'      RevisedNo4FRDD`samplename'      RevisedFRDD`samplename'      AllNowtRobFRDD`samplename'      AllRobFRDD`samplename'      using output/RDD.rtf, append style(tab) keep(Conventional) varlabel(Conventional "Estimate") stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes nonumber nogaps star(* .1 ** .05 *** .01) nolines paren noobs nomtitles fonttbl(\f0\fnil LM Roman 9;) msign("–") b(%5.3g) se(%5.3g) 
  }
}


* RDD big table & graph in appendix
{
  PrepData

  * schooling-for-young-based bandwidths for original data set
  rdbwselect_2014 K23_Years_Schooling K23_Female_Literacy if K23_Age<35 & K23_lnWages1<., c(.39286)
  scalar h  = round(e(h_CCT), .001)  // original rounds
  scalar bb = round(e(b_CCT), .001)  // unfortunately, rdrobust deletes b

  rdbwselect K23_Years_Schooling K23_Female_Literacy if K23_Age<35 & K23_lnWages1<., c(.39286) weights(WTtr)  // rdbwselect_2014 can't handle weights
  scalar hWTtr  = e(h_mserd)
  scalar bbWTtr = e(b_mserd)

  rdbwselect K23_Years_Schooling K23_Female_Literacy if K23_Age<35 & K23_lnWages1<., vce(cluster K23_Female_Literacy) c(.39286)  // rdbwselect_2014 can't cluster
  scalar hcl  = e(h_mserd)
  scalar bbcl = e(b_mserd)

  rdbwselect K23_Years_Schooling K23_Female_Literacy if K23_Age<35 & K23_lnWages1<., vce(cluster K23_Female_Literacy) c(.39286) weights(WTtr)
  scalar hclWTtr  = e(h_mserd)
  scalar bbclWTtr = e(b_mserd)

  cap erase output/RDD2.rtf
  forvalues e=1/3 {  // estimation specifications
    local depvar  : word `e' of Years_Schooling lnWages1 lnWages1
    local fuzzy   : word `e' of ""              ""       Years_Schooling
    local scalepar: word `e' of scalepar(-1) scalepar(-1) scalepar(1)

    forvalues s=1/2 {
      local sample: word `s' of Age<35 Age<=75&Age>35
      local samplename: word `s' of Young Old

      foreach wt in "" WTtr {
        eststo Original`samplename'`wt'        : rdrobust K23_`depvar'                          K23_Female_Literacy if K23_`sample'     &    K23_Wages1<.                , weights(`wt') `scalepar' all fuzzy(`fuzzy') c(.39286) h(`=h`wt''  ) b(`=bb`wt''  )
        eststo OriginalCl`samplename'`wt'      : rdrobust K23_`depvar'                          K23_Female_Literacy if K23_`sample'     &    K23_Wages1<.                , weights(`wt') `scalepar' all fuzzy(`fuzzy') c(.39286) h(`=hcl`wt'') b(`=bbcl`wt'') vce(cluster K23_Female_Literacy)
        eststo OriginalAutoBW`samplename'`wt'  : rdrobust K23_`depvar'                          K23_Female_Literacy if K23_`sample'     &    K23_Wages1<.                , weights(`wt') `scalepar' all fuzzy(`fuzzy') c(.39286) vce(hc3) //  bwselect(cerrd)  // last option new since first arXiv version
        eststo OriginalAutoBWCl`samplename'`wt': rdrobust K23_`depvar'                          K23_Female_Literacy if K23_`sample'     &    K23_Wages1<.                , weights(`wt') `scalepar' all fuzzy(`fuzzy') c(.39286) vce(cluster K23_Female_Literacy) //  bwselect(cerrd)  // last option new since first arXiv version
        eststo RevisedAutoBW`samplename'`wt'   : rdrobust `=cond(`e'>1,"lnWeekly_Wages","`depvar'")' Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent, weights(`wt') `scalepar' all fuzzy(`fuzzy') c(`=National_Average_FL') //  bwselect(cerrd)  // last option new since first arXiv version
        eststo RevisedAutoBWCl`samplename'`wt' : rdrobust `=cond(`e'>1,"lnWeekly_Wages","`depvar'")' Female_Literacy if `sample'-(`s'-1) & lnWeekly_Wages<. & single_parent, weights(`wt') `scalepar' all fuzzy(`fuzzy') c(`=National_Average_FL') vce(cluster Female_Literacy) //  bwselect(cerrd)  // last option new since first arXiv version

        foreach mat in bRD seRD bCCT seCCT bCCTCl seCCTCl {
          cap mat drop `mat'`samplename'`wt'
        }
        foreach est in Original OriginalAutoBW RevisedAutoBW {
          if "`est'"=="OriginalAutoBW" & `e'==1 continue

          estadd scalar N_h = e(N_h_l) + e(N_h_r): `est'`samplename'`wt' `est'Cl`samplename'`wt'

          est restore `est'`samplename'`wt'
          mat bRD`samplename'`wt'     = nullmat(bRD`samplename'`wt'    ), e(tau_cl)
          mat seRD`samplename'`wt'    = nullmat(seRD`samplename'`wt'   ), e(se_tau_cl)
          mat bCCT`samplename'`wt'    = nullmat(bCCT`samplename'`wt'   ), e(tau_bc)
          mat seCCT`samplename'`wt'   = nullmat(seCCT`samplename'`wt'  ), e(se_tau_rb)
          est restore `est'Cl`samplename'`wt'
          mat bCCTCl`samplename'`wt'  = nullmat(bCCTCl`samplename'`wt' ), e(tau_bc)
          mat seCCTCl`samplename'`wt' = nullmat(seCCTCl`samplename'`wt'), e(se_tau_rb)
        }
      }
    }

    foreach est in Original OriginalAutoBW RevisedAutoBW {
      if "`est'"=="OriginalAutoBW" & `e'==1 continue
      cap findfile output/RDD2.rtf, path(".")
      local mtitles = cond(_rc, `"mtitles("Young, unweighted" "Old, unweighted" "Young, weighted" "Old, weighted")"', "nomtitles")
      esttab `est'Young   `est'Old   `est'YoungWTtr   `est'OldWTtr   using output/RDD2.rtf, append keep(Conventional Robust)                         stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes nonumber nogaps nostar nolines paren noobs `mtitles' fonttbl(\f0\fnil LM Roman 9;) msign("–") b(%5.3g) se(%5.3g)
      esttab `est'ClYoung `est'ClOld `est'ClYoungWTtr `est'ClOldWTtr using output/RDD2.rtf, append keep(Robust) varlabel(Robust "Robust, clustered") stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes nonumber nogaps nostar nolines paren noobs nomtitles fonttbl(\f0\fnil LM Roman 9;) msign("–") b(%5.3g) se(%5.3g)
    }
   
    preserve
    drop _all  // speeds up "coefplot, gen"
    local xtitle: word `e' of "Impact of intention-to-treat on schooling" "Impact of intention-to-treat on log wages" "Impact of schooling on log wages"
    local xlabels : word `e' of "" "-.4(.2).6" ""
    coefplot (m(bRDYoung       ), se(seRDYoung       ) pstyle(p1)) (m(bRDOld       ), se(seRDOld       ) pstyle(p1) mfcolor(gs14))  ///
             (m(bCCTYoung      ), se(seCCTYoung      ) pstyle(p2)) (m(bCCTOld      ), se(seCCTOld      ) pstyle(p2) mfcolor(gs14))  ///
             (m(bCCTClYoung    ), se(seCCTClYoung    ) pstyle(p4)) (m(bCCTClOld    ), se(seCCTClOld    ) pstyle(p4) mfcolor(gs14)), bylabel(`=cond(`e'==1, "Without sampling weights", `""""')') || ///
             (m(bRDYoungWTtr   ), se(seRDYoungWTtr   ) pstyle(p1)) (m(bRDOldWTtr   ), se(seRDOldWTtr   ) pstyle(p1) mfcolor(gs13))  ///
             (m(bCCTYoungWTtr  ), se(seCCTYoungWTtr  ) pstyle(p2)) (m(bCCTOldWTtr  ), se(seCCTOldWTtr  ) pstyle(p2) mfcolor(gs13))  ///
             (m(bCCTClYoungWTtr), se(seCCTClYoungWTtr) pstyle(p4)) (m(bCCTClOldWTtr), se(seCCTClOldWTtr) pstyle(p4) mfcolor(gs13)), bylabel(`=cond(`e'==1, "With sampling weights", `""""')')    ///
             cismooth(n(20) lwidth(3 3)) ///
             coeflabels(c1=`""Original data," "optimal bandwidth" "for schooling""' c`=(`e'>1)+2'=`""Revised data," "regression-specific" "optimal bandwidth""' c2=`""Original data," "regression-specific" "optimal bandwidth""') ///
             ylab(,labcolor(black)) xtitle("                              `xtitle'", margin(t=-2)) ///
             legend(order(21 42 63 84 105 126) lab(21 "Conventional CI, young subsample") lab(42 "Conventional CI, old subsample") lab(63 "Robust CI, young subsample") lab(84 "Robust CI, old subsample") lab(105 "Robust clustered CI, young subsample") lab(126 "Robust clustered CI, for old") rows(2) colfirst region(fcolor(none) margin(zero)) rowgap(zero)) ///
             scheme(plottig) plotregion(fcolor(gs13)) graphregion(margin(zero)) xlab(`xlabels', labcolor(black) labgap(0pt)) gen nodraw
    global graph `r(graph)'
    if `e'==3 {  // clip extreme fuzzy RDD CIs
      qui forvalues i=1/20 {
        replace __ll`i' = cond(__ll`i'<-1, -1, cond(__ll`i'> 1, ., __ll`i'))
        replace __ul`i' = cond(__ul`i'> 1,  1, cond(__ul`i'<-1, ., __ul`i'))
        replace __b = . if __b<-1 | __b>1
      }
    }
    $graph name(RDD`depvar'`fuzzy', replace)
    restore
  }
  grc1leg2 RDDYears_Schooling RDDlnWages1 RDDlnWages1Years_Schooling, cols(1) xsize(7) ysize(7) imargin(0 0 2 0) graphregion(margin(zero))
  graph export output/RDD.png, replace width(2000)
}


***
*** GE effects & elasticities
***

PrepData
parallel initialize 14  // number of CPU cores to use in bootstrapping
set seed 17615
mata st_global("seeds", invtokens(strofreal(runiformint(1, $PLL_CLUSTERS, -2^31, 2^31-1), "%16.0f")))

* bootstrap estimates of GE effects in given specification and add one-tailed tests of each having sign opposite that estimated
cap program drop bsGE
program define bsGE
  syntax, estname(string) exp(passthru) cmdline(string asis) [cluster(passthru)]
  tempfile tempfile
  eststo `estname': parallel bs, `exp' reps(1500) seeds($seeds) saving(`tempfile') `cluster': `cmdline'
  preserve
  use `tempfile', clear
  tempname b
  mat `b' = e(b)
  forvalues p=1/`e(k_exp)' {  // bootstrap p values for each parameter having sign opposite that estimated
    local param: word `p' of `:colnames `b''
    qui count if `param' * sign(_b[`param']) < 0
    mat `b'[1,`p'] = r(N)/_N
  }
  estadd mat p = `b'  // add e(p) result matrix for esttab to access
  restore    
end

* Original calculations
{
  * options:
  * p() -- prefix: K23_ (original data) or "" (new data)
  * est() -- estimator: cl (Conventional) or bc (bias-corrected, robust)
  * auto -- regression-specific bw computation
  * wt() -- weights
  cap program drop estGEOrig
  program define estGEOrig, rclass
    syntax [if/], [p(string) est(string) auto wt(string)]
    if "`if'"=="" local if 1

    preserve
    keep if Age<35

    cap noi {
      sum `p'Years_Schooling if Skilled & `p'Female_Literacy<.39286-.1 & !`p'DPEP & `if' `=cond("`wt'"=="","","[aw=`wt']")', detail
      scalar S_ys = r(p50)

      sum `p'Years_Schooling if !Skilled & `p'Female_Literacy<.39286-.1 & !`p'DPEP & `if' `=cond("`wt'"=="","","[aw=`wt']")', detail
      scalar S_yu = r(p50)

      rdrobust `p'lnWages1 `p'Female_Literacy if Skilled & `if', fuzzy(`p'DPEP) `=cond("`auto'"!="","vce(hc3)","h(.0408) b(.0917)")' weights(`wt') c(.39286)
      scalar Δw_ys = e(tau_`est')

      rdrobust `p'lnWages1 `p'Female_Literacy if !Skilled & `if', fuzzy(`p'DPEP) `=cond("`auto'"!="","vce(hc3)","h(.074) b(.1326)")' weights(`wt') c(.39286)
      scalar Δw_yu = e(tau_`est')

      rdrobust `p'lnWages1 `p'Female_Literacy if `if', fuzzy(`p'DPEP) `=cond("`auto'"!="","vce(hc3)","h(.112) b(.172)")' weights(`wt') c(.39286)
      scalar Δw_y = e(tau_`est')

      sum Skilled if `p'Female_Literacy <.39286-.1 & `p'DPEP & `p'Wages1<. & `if' `=cond("`wt'"=="","","[aw=`wt']")'
      scalar ℓ_syD1 = r(mean)

      rdrobust Skilled `p'Female_Literacy if `p'Wages1<. & `if', fuzzy(`p'DPEP) `=cond("`auto'"!="","vce(hc3)","h(.0897) b(.138)")' weights(`wt') c(.39286)
      scalar Δℓ_sy = e(tau_`est')

      return scalar Δβ～_y = (Δw_ys - Δw_yu) / (S_ys - S_yu)
      return scalar β～₀_y = (Δw_y - (ℓ_syD1 * Δw_ys + (1-ℓ_syD1) * Δw_yu)) / Δℓ_sy / (S_ys - S_yu)
      return scalar β～₁_y = return(β～₀_y) + return(Δβ～_y)
    }
    restore
  end

  preserve
  keep if K23_DPEP<. & Years_Schooling<=14  // match K23 sample that is the starting point for bootstrapping
  bsGE, estname(GEOrig) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, p(K23_) est(cl)         )
  bsGE, estname(GERob ) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, p(K23_) est(bc)         )
  bsGE, estname(GEAuto) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, p(K23_) est(cl) auto    )
  bsGE, estname(GECl  ) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, p(K23_) est(cl)         ) cluster(DISTRICT_CODE)
  bsGE, estname(GEWt  ) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, p(K23_) est(cl) wt(WTtr)) 
  restore

  preserve
  keep if Years_Schooling<=14
  bsGE, estname(GERevNo4) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig if !inlist(DISTRICT_CODE,1402,2719,2728,3318), est(cl))
  bsGE, estname(GERev   ) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, est(cl))
  bsGE, estname(GEAll   ) exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y)) cmdline(estGEOrig, est(bc) auto) cluster(DISTRICT_CODE)
  restore
}

* revised calculation as described in appendix B
{
  cap program drop estGERev
  program define estGERev, rclass
    syntax, [weight(string)]
    scalar National_Average_FL = .39286956
    cap noi {
      foreach yo in y o {
        foreach su in s u {
          rdrobust `su' x if `yo', fuzzy(D) c(`=National_Average_FL') weights(`weight') all vce(hc3)  // for young, skilled this is fraction switched, Table 3, top left
          scalar Δℓ_`yo'`su' = e(tau_bc)
          sum `su' if abs(x - National_Average_FL)<.1 & `yo' [aw = max(0, 1 - abs(x-National_Average_FL)/e(h_l)) `=cond("`weight'"!="","* `weight'","")'], meanonly
          scalar ℓ_`yo'`su' = r(mean)
          scalar ℓ_`yo'`su'D1 = ℓ_`yo'`su' + Δℓ_`yo'`su' / 2  // estimates of labor shares in program and non-program districts compatible with FRDD estimate of their difference
          scalar ℓ_`yo'`su'D0 = ℓ_`yo'`su' - Δℓ_`yo'`su' / 2

          foreach depvar in w S {
            rdrobust `depvar' x if `su' & `yo', fuzzy(D) c(`=National_Average_FL') weights(`weight') all vce(hc3)
            scalar Δ`depvar'_`yo'`su' = e(tau_bc)
            sum `depvar' if abs(x - National_Average_FL)<.1 & `su' & `yo' [aw = max(0, 1 - abs(x-National_Average_FL)/e(h_l)) `=cond("`weight'"!="","* `weight'","")'], meanonly
            scalar `depvar'_`yo'`su'D1 = r(mean) + Δ`depvar'_`yo'`su' / 2  // estimates of schooling averages in program and non-program districts compatible with FRDD estimate of their difference
            scalar `depvar'_`yo'`su'D0 = r(mean) - Δ`depvar'_`yo'`su' / 2
          }
        }
        scalar β₀_`yo' = w_`yo'sD0 - w_`yo'uD0  // direct computation of skill premia
        scalar β₁_`yo' = w_`yo'sD1 - w_`yo'uD1
        scalar Δβ_`yo' = β₁_`yo' - β₀_`yo'
        return scalar β～₀_`yo' = β₀_`yo' / (S_`yo'sD0 - S_`yo'uD0)
        return scalar β～₁_`yo' = β₁_`yo' / (S_`yo'sD1 - S_`yo'uD1)
        return scalar Δβ～_`yo' = return(β～₁_`yo') - return(β～₀_`yo')
      }
      foreach su in s u {
        rdrobust `su' x, fuzzy(D) c(`=National_Average_FL') weights(`weight') all vce(hc3)
        scalar ΔL_`su' = e(tau_bc)
      }
      return scalar σ_A = - (Δℓ_ys/ℓ_ys - Δℓ_yu/ℓ_yu - Δℓ_os/ℓ_os + Δℓ_ou/ℓ_ou) / (Δβ_y - Δβ_o)
      return scalar σ_E = - (Δw_ys - Δw_yu - (ΔL_s - ΔL_u - Δℓ_ys/ℓ_ys +Δℓ_yu/ℓ_yu) / return(σ_A)) / (ΔL_s - ΔL_u)
    }
  end

  preserve
  keep if lnWeekly_Wages<. & Female_Literacy<. & Years_Schooling<. & Age<=75
  rename (DPEP lnWeekly_Wages Years_Schooling Female_Literacy) (D w S x)
  gen byte s = S>= 8
  gen byte u = !s
  gen byte y = Age < 35
  gen byte o = !y
  keep D w S x s u y o // WTtr

  bsGE, estname(GEAllNew) exp(β～₀_y=r(β～₀_y) β～₁_y=r(β～₁_y) Δβ～_y=r(Δβ～_y) σ_E=r(σ_E) σ_A=r(σ_A)) cmdline(estGERev, weight(/*WTtr*/)) cluster(x) 
  restore
}

* GE & elasticity results table
esttab GEOrig GERob GEAuto GECl GEWt GERevNo4 GERev GEAll GEAllNew using output/GE.rtf, replace ///
  cells(b(fmt(%6.3g)) se(fmt(%9.2g) par) p(fmt(%6.2g) par([ ]))) noobs nonote nonum nogaps fonttbl(\f0\fnil LM Roman 9;) msign("–") keep(β～₀_y β～₁_y Δβ～_y σ_A σ_E) ///
  mtitles("Original" "Robust CI" "Regression-specific BW" "Clustered" "Sampling weights" "Revised data x 4 absent districts" "Revised data, all districts" "All changes" "Revised method & data")
