* Code for Roodman comment on Khanna (2023)

* dependencies (on SSC unless otherwise noted):
* 	 coefplot
* 	 esttab
* 	 spmap
* 	 shp2dta
* 	 grc1leg2 (net install grc1leg2, from(http://digital.cgdev.org/doc/stata/MO/Misc))
*    blindschemes
* 	 rdrobust
* 	 parallel (tweaked to distribute work more evenly across cores, "net install parallel, from(https://raw.github.com/droodman/parallel/master) replace")
*    reghdfejl
*    julia
* Most of these are in this archives' ado folder. But the parallel and julia package installation must be done by the replicator in order to get the proper, machine-specific plug-ins.

*** Need to cd to root of this archive. For example:
cd "/Users/davidroodman/Downloads/Khanna-2023-main"
// cap noi cd "D:/OneDrive - Open Philanthropy Project/Education/Khanna/Public"
// cap noi cd "/Users/davidroodman/Library/CloudStorage/OneDrive-OpenPhilanthropyProject/Education/Khanna/Public"

adopath + ado

if c(os)=="Windows" {
	global font LM Roman 9  // https://www.1001fonts.com/latin-modern-roman-font.html
}
else {
	global font Latin Modern Roman
}

graph set window fontface "$font"
set scheme plotplain

parallel initialize 14  // number of CPU cores to use in bootstrapping; optimal choice depends on computer
set seed 17615
mata st_global("seeds", invtokens(strofreal(runiformint(1, $PLL_CLUSTERS, -2^31, 2^31-1), "%16.0f")))  // replicable seeds for all Stata copies


***
*** Prep individual-level data
***
cap program drop PrepData
program define PrepData
//   odbc load, clear dsn(Khanna) exec("SELECT SUM(F_LITERATE)/CAST(SUM(T_F_POPLN-POPLN_F6) AS real) as fl FROM Census1991.[District Primary Census Abstracts]")
//   scalar National_Average_FL = fl[1]  // should be .39286956; also see http://14.139.60.153/bitstream/123456789/1042/1/Census%201991_District%20Literacy_D-10761.pdf#page=4
  scalar National_Average_FL = 129752482 / 330286606

  cap odbc load, clear dsn(Khanna) exec("select * from Datasets where Age between 17 and 100")
  if !_rc saveold "data/K23 individual-level", version(13) replace
  use "data/K23 individual-level", clear

  recode DPEP (.5 = .)  // Baksa, Assam, has mixed parentage; code program status as missing

  foreach p in "" K23_ {
    gen `p'lnWages  = ln(`p'Wages)   // all wages
    gen `p'lnWages1 = ln(`p'Wages1)  // wages from first-listed activity
  }
  recode General_Education                       (5 = 3) (6 = 5) (7 = 8) (8 = 10) (10 = 12) (11 = 14) (12 = 16) (13 = 18) (nonmiss = 0), gen(Years_Schooling_Formal)  // based partly on DOI 10.1177/1464993417716357, p. 4; see http://www.icssrdataservice.in/datarepository/index.php/catalog/89/datafile/F4/V161
  recode General_Education     (2 = 1) (3 4 = 2) (5 = 3) (6 = 5) (7 = 8) (8 = 10) (10 = 12) (11 = 14) (12 = 16) (13 = 18) (nonmiss = 0), gen(Years_Schooling)
  recode K23_General_Education (2 = 1) (3 4 = 2) (5 = 3) (6 = 5) (7 = 8) (8 = 10) (10 = 12) (11 = 14) (12 = 16) (13 = 18) (nonmiss = 0), gen(K23_Years_Schooling)  // assigns some schooling to "without formal schooling"

  gen byte single_parent = FL_SD < .01  // was .03; really districts of relatively unmixed parentage, because they come mostly from one parent, or literacy doesn't vary much across parents
  gen double lnWeekly_Wages = lnWages - ln(Days_worked / 7)  // meaning wage rate rather than total wages
  gen byte Skilled = Years_Schooling>=8
  gen byte missing4 = inlist(DISTRICT_CODE,1402,2719,2728,3318)

  sum WEIGHT, detail
  gen WTtr = min(WEIGHT, r(p50)+4*(r(p75)-r(p50)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)
end


cap program drop robustify
program define robustify, eclass  // hack: rotate a stored rdrobust result so the "Bias-corrected" or "Robust" estimate is in the "Conventional" slot, for easy inclusion in esttab
  tempname b V
  mat `b' = e(b)
  mat `V' = e(V)
  forvalues i=1/`1' {
    mata st_matrix("`b'", st_matrix("`b'")[2\3\1])
    mata st_matrix("`V'", st_matrix("`V'")[2\3\1,2\3\1])
  }
  ereturn repost b=`b' V=`V'
end


cap program drop RDtable
program define RDtable
  syntax, agegroups(numlist) specifications(numlist) outfile(string) [ik c(string)]
  local ik = upper("`ik'")

  foreach a of numlist `agegroups' {
    forvalues r=1/3 {
      est drop _all
      foreach s of numlist `specifications' {
        local prefix      = cond(`s'==1, "K23_", "")
        local schoolvar   = cond(`s'==1, "Years_Schooling", "Years_Schooling_Formal")
        local wagevar     = cond(`s'==1, "Wages1", "Wages")
        local sample      = cond(`s'==1, "`prefix'`wagevar'<.", cond(`s'==2, "!missing4 & `prefix'ln`wagevar'<. & single_parent", "`prefix'ln`wagevar'<. & single_parent"))
        local _c           = cond(`s'==1, .39286, National_Average_FL)
        local rdrobust_vce= cond(`s'<=3, "", "cluster `prefix'Female_Literacy")
        local methods     = cond(`s'<=4, "CCT `ik'", "mserd")
        local estshift    : word `s' of 0 0 0 0 0 1 2 2  // 0 = conventional, 1 = bias-corrected, 2 = bias-corrected, robust
        local weight     = cond(`s'<=7, "", "WTtr")

        if "`c'"=="" local c `_c'
        
        local depvar        : word `r' of `prefix'`schoolvar' `prefix'ln`wagevar' `prefix'ln`wagevar'
        local fuzzy_scalepar: word `r' of scalepar(-1) scalepar(-1) fuzzy(`prefix'`schoolvar')

        local agerange: word `a' of 0,34 36,75

        cap drop _Female_Literacy
        gen _Female_Literacy = `prefix'Female_Literacy - `c'  // needed for exact IK bandwidth match: subtraction in float rather than double creates a small error

        foreach method in `methods' {
          if `s'>4  {
            local h_`method'`s'
            local b_`method'`s'
          }
          else if `r'`a'==11 {  // use young-schooling BW throughout in first 3 specifications
            rdbwselect_2014 `prefix'`schoolvar' _Female_Literacy if `prefix'Age<35 & `sample', bwselect(`method')
            local h_`method'`s' = round(e(h_`method'), .001)  // original rounds
            local b_`method'`s' = round(e(b_`method'), .001)
          }

          eststo: rdrobust `depvar' `prefix'Female_Literacy if inrange(`prefix'Age,`agerange') & `sample', c(`c') h(`h_`method'`s'') b(`b_`method'`s'') vce(`rdrobust_vce') `fuzzy_scalepar' weights(`weight') all
          eststo est$eststo_counter: robustify `estshift'
        }
      }
      estadd scalar N_h = e(N_h_l) + e(N_h_r): *
      esttab using `outfile', append b(%5.3f) se(%5.3f) rename(Conventional Estimate) keep(Estimate) stat(h_l N_h, labels(BandwidthObservations) fmt( %5.3f %8.0gc)) nomtitles nonum nonotes nogaps star(* .1 ** .05 *** .01) nolines paren noobs fonttbl(\f0\fnil $font;) msign("–")
    }
  }
end


cap program drop estGE
* options:
* prefix() -- prefix: K23_ (original data) or "" (new data)
* est() -- estimator: cl (Conventional) or bc (bias-corrected)
* auto -- regression-specific bw computation
* wt() -- weights
* nobug -- define untreated band as "< threshold + .1" above threshold instead of "< threshold - .1" and treated band as "> threshold - .1", not < "threshold - .1"
* ln -- controls whether certain computations restricted to wages<. or lnwages<.
* wagevar() and schoolvar() specify outcome vars
program define estGE, rclass
  syntax [if/], wagevar(string) schoolvar(string) est(string) [prefix(string) auto wt(string) nobug ln c(string) vce(passthru)]
  local shift = cond("`bug'"=="", -.1, .1)  // bug in lines 80-92 of K23's "GE_welfare_bootstrap_part1": define band on untreated side by FL<.3929-.1 instead of .3929+.1
  if "`c'"=="" local c .39286

  cap noi {
    sum `schoolvar' if  Skilled & `prefix'Female_Literacy<`c'+`shift' & !`prefix'DPEP `=cond("`wt'"=="","","[aw=`wt']")', detail
    scalar S_ys = r(p50)

    sum `schoolvar' if !Skilled & `prefix'Female_Literacy<`c'+`shift' & !`prefix'DPEP `=cond("`wt'"=="","","[aw=`wt']")', detail
    scalar S_yu = r(p50)

    rdrobust `prefix'ln`wagevar' `prefix'Female_Literacy if  Skilled, fuzzy(`prefix'DPEP) `=cond("`auto'"!="","","h(.0408) b(.0917)")' weights(`wt') c(`c') `vce' // bandwidths copied from K23 code
    scalar Δw_ys = e(tau_`est')

    rdrobust `prefix'ln`wagevar' `prefix'Female_Literacy if !Skilled, fuzzy(`prefix'DPEP) `=cond("`auto'"!="","","h(.074 ) b(.1326)")' weights(`wt') c(`c') `vce'
    scalar Δw_yu = e(tau_`est')

    rdrobust `prefix'ln`wagevar' `prefix'Female_Literacy            , fuzzy(`prefix'DPEP) `=cond("`auto'"!="","","h(.112 ) b(.172 )")' weights(`wt') c(`c') `vce'
    scalar Δw_y = e(tau_`est')

    sum Skilled if `prefix'Female_Literacy `=cond("`bug'"=="", "<", ">")' `c'-.1 & `prefix'DPEP & `prefix'`ln'`wagevar'<. `=cond("`wt'"=="","","[aw=`wt']")'
    scalar ℓ_syD1 = r(mean)

    rdrobust Skilled `prefix'Female_Literacy if `prefix'`ln'`wagevar'<., fuzzy(`prefix'DPEP) `=cond("`auto'"!="","","h(.0897) b(.138)")' weights(`wt') c(`c') `vce'
    scalar Δℓ_sy = e(tau_`est')

    return scalar Δβ～_y = (Δw_ys - Δw_yu) / (S_ys - S_yu)
    return scalar β～₀_y = (Δw_y - (ℓ_syD1 * Δw_ys + (1-ℓ_syD1) * Δw_yu)) / Δℓ_sy / (S_ys - S_yu)
    return scalar β～₁_y = return(β～₀_y) + return(Δβ～_y)
    return scalar Δβpct = return(Δβ～_y) / return(β～₀_y) * 100
  }

  di "S_ys = " S_ys
  di "S_yu = " S_yu
  di "Δw_ys = " Δw_ys
  di "Δw_yu = " Δw_yu
  di "Δw_y = " Δw_y
  di "ℓ_syD1 = " ℓ_syD1
  di "Δℓ_sy = " Δℓ_sy
  di "Δβ～_y = " return(Δβ～_y)
  di "β～₀_y = " return(β～₀_y)
  di "β～₁_y = " return(β～₁_y)
end

* bootstrap estimates of GE effects in given specification and add one-tailed tests of each having sign opposite that estimated
cap program drop bsGE
program define bsGE
  syntax, exp(passthru) cmdline(string asis) [cluster(passthru) reps(int 1500)]
  tempfile tempfile
  eststo: parallel bs, nodots `exp' reps(`reps') seeds($seeds) saving(`tempfile') `cluster': `cmdline'
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

cap program drop GEtable
program define GEtable
  syntax, specifications(numlist) outfile(string) [c(passthru)]

  eststo clear
  foreach s of numlist `specifications' {
    local prefix    = cond(`s'<=1, "K23_", "")
    local wagevar   = cond(`s'<=1, "Wages1", "Wages")
    local ln        = cond(`s'<=1, "", "ln")  // whether to restrict samples to Wages<. or lnWages<.
    local schoolvar = cond(`s'<=1, "K23_Years_Schooling", "Years_Schooling_Formal")
    local sample    = cond(`s'==1, "", cond(`s'==2, "& !missing4 & single_parent", "& single_parent"))
    local nobug     = cond(`s'<=1, "", "nobug")  // K23 code defines untreated band as FL<.3929-.1, not FL<.3929+.1
    local cluster   = cond(`s'<=3, "", "`prefix'Female_Literacy")
    local auto      = cond(`s'<=4, "", "auto")
    local est       = cond(`s'<=5, "cl", "bc")  // conventional linear RD model or bias-corrected
    local wt        = cond(`s'<=6, "", "WTtr")
    local vce       = cond("`cluster'"=="","","cluster `cluster'")

    preserve
    keep if `prefix'Age<35 & `prefix'DPEP<. & `schoolvar'<=14 & `prefix'Female_Literacy<. `sample'
    bsGE, exp(β～₀_y=r(β～₀_y) β～₁_y=(r(β～₁_y)) Δβ～_y=r(Δβ～_y) Δβpct=r(Δβpct)) cluster(`cluster') cmdline(estGE, `nobug' schoolvar(`schoolvar') wagevar(`wagevar') `ln' prefix(`prefix') est(`est') `auto' wt(`wt') `c' vce(`vce'))
    restore
  }

  esttab using `outfile', append ///
    cell(b(fmt(%6.3f)) p(fmt(%6.2f) par(( ))) ci_percentile[ll](fmt(a2)    par("[" "")) & ci_percentile[ul](fmt(a2)    par("" "]"))) incelldelimiter(", ") drop(Δβpct) noobs nonote nomtitles nonum nogaps nolines fonttbl(\f0\fnil $font;) msign("–")

  esttab using `outfile', append ///
    cell(b(fmt(%6.0f)) p(fmt(%6.2f) par(( ))) ci_percentile[ll](fmt(%6.0f) par("[" "")) & ci_percentile[ul](fmt(%6.0f) par("" "]"))) incelldelimiter(", ") keep(Δβpct) noobs nonote nomtitles nonum nogaps nolines fonttbl(\f0\fnil $font;) msign("–")
end


// insert NSS files into SQL Server
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
  shp2dta using "shp/District shape file 2009/geo2_in2009", database(shp/District shape file 2009/2009_Distdb) coordinates(shp/District shape file 2009/2009_Distcoord) genid(DistrictID) replace
  use "shp/District shape file 2009/2009_Distdb", clear
  destring DIST2009, replace
  save "shp/District shape file 2009/2009_Distdb", replace

  // odbc load, clear dsn(Khanna) exec("SELECT SUM(F_LITERATE)/CAST(SUM(T_F_POPLN-POPLN_F6) AS real) as fl FROM Census1991.[District Primary Census Abstracts]")
  // scalar National_Average_FL = fl[1]  // should be .39286956; also see http://14.139.60.153/bitstream/123456789/1042/1/Census%201991_District%20Literacy_D-10761.pdf#page=4
  scalar National_Average_FL = 129752482 / 330286606  // .39286956  -- average female literacy in 1991 census

  * district-level stats
  cap noi odbc load, exec("SELECT DISTINCT DISTRICT_CODE as DIST2009, DPEP, K23_DPEP, K23_NSS_Sample, Female_Literacy, K23_Female_Literacy, FL_SD FROM Datasets") dsn(Khanna) clear
  if !_rc saveold "data/K23 district-level", version(13) replace
  
  use "data/K23 district-level", clear
  merge 1:1 DIST2009 using "shp/District shape file 2009/2009_Distdb", keep(master match using)

  recode DPEP (.5 = .)  // Baksa, Assam, has mixed parentage; code program status as missing

  gen byte NSS_Sample = 1 if _merge != 2
  replace K23_NSS_Sample = . if K23_NSS_Sample==0
  gen byte     T = Female_Literacy < National_Average_FL if Female_Literacy<. & FL_SD<.01  // intent to treat
  gen byte K23_T = K23_Female_Literacy < .39286 if K23_Female_Literacy<.

  spmap K23_T          using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapTOrig, replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data") lab(2 "Above literacy cap") lab(3 "Below literacy cap"))
  spmap     T          using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapTNew , replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapTOrig mapTNew, name(Tmaps, replace) graphregion(margin(zero)) imargin(zero) title(Intention to treat, pos(11) margin(zero)) pos(3)

  spmap K23_DPEP       using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapDOrig, replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))             legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data") lab(2 "No DPEP program") lab(3 "DPEP program"))
  spmap     DPEP       using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapDNew , replace) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapDOrig mapDNew, name(Dmaps, replace) graphregion(margin(zero)) imargin(zero) title(Treatment, pos(11) margin(zero)) pos(3)

  spmap K23_NSS_Sample using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapNSSOrig, replace) title(Original data set, size(vlarge) pos(6) span) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(size(medium) margin(zero) bmargin(zero) lab(1 "No data                ") lab(2 "Data"))
  spmap     NSS_Sample using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) name(mapNSSNew , replace) title(New data set     , size(vlarge) pos(6) span) nodraw osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero))
  grc1leg2 mapNSSOrig mapNSSNew, name(NSSmaps, replace) graphregion(margin(zero)) imargin(zero) title(Follow-up, pos(11) margin(zero)) pos(3)

  replace K23_NSS_Sample = 2 if K23_NSS_Sample==. & NSS_Sample<.
  spmap K23_NSS_Sample using "shp/District shape file 2009/2009_Distcoord", id(DistrictID) clmethod(unique) fcolor(blue green) ndfcolor(gs12) osize(.2pt .2pt) ndsize(.2pt) graphregion(margin(zero)) legstyle(3) legend(pos(5) size(medsmall) margin(zero) bmargin(zero) lab(1 "No data                ") lab(2 "Complete data, used in K23") lab(3 "Complete data, not used in K23"))
  graph export output/mapNSSOrig.png, height(1552) replace  // for blog

  graph combine Tmaps Dmaps NSSmaps, imargin(zero) graphregion(margin(zero)) cols(1) xsize(7) ysize(8)
  graph export output/maps.png, replace width(2000)

  di "Table 2: (dis)agreement between original and new program assignment variables"
  table T    K23_T   , missing
  collect export output/Table2.txt, replace
  table DPEP K23_DPEP, missing
  collect export output/Table2.txt, append

  * first row of rdplots
  rdplot K23_DPEP K23_Female_Literacy, p(2) nbins(18 22) graph_options(                 legend(off) graphregion(margin(zero)) nodraw ylab(,format(%3.1f)) name(DPEPOrig, replace) title(Original)) c(.39286)
  rdplot K23_DPEP K23_Female_Literacy,                   graph_options(xscale(off fill) legend(off) graphregion(margin(zero)) nodraw name(DPEPdefault, replace) title("Original data, rdplot defaults")) c(.39286)            
  rdplot     DPEP     Female_Literacy,                   graph_options(xscale(off fill) legend(off) graphregion(margin(zero)) nodraw name(DPEPnew    , replace) title("New data, rdplot defaults")) c(`=National_Average_FL')
  graph combine DPEPOrig DPEPdefault DPEPnew, rows(1) l1title(DPEP treatment, size(small)) graphregion(margin(zero)) nodraw name(DPEP, replace)
}


PrepData

* compare original and revised variables
corr K23_lnWages1 lnWages
corr K23_Age Age
corr K23_Female_Literacy Female_Literacy
corr K23_Years_Schooling Years_Schooling_Formal

* document 4 absent districts
{
  preserve
  collapse Female_Literacy DPEP if Years_Schooling_Formal<. & K23_Years_Schooling==. & single_parent, by(DISTRICT_CODE)
  sort Female_Literacy
  list
  restore
}

* finish rdplots
{
  PrepData
  rdplot K23_Years_Schooling        K23_Female_Literacy if K23_Age<=35 & K23_Wages1    <. & abs(K23_Female_Literacy-.39286)<.2, nbins(6 7) p(2) c(.39286)                 graph_options(legend(off) xscale(off fill) graphregion(margin(2 0 0 0)) nodraw name(SchoolingOrig, replace)) ylab(,format(%3.2f))
  rdplot K23_Years_Schooling        K23_Female_Literacy if K23_Age<=35 & K23_Wages1    <.                                     ,                 c(.39286)                 graph_options(legend(off) xscale(off fill) graphregion(margin(zero))    nodraw name(Schoolingdefault, replace))
  rdplot     Years_Schooling_Formal     Female_Literacy if     Age<=35 & lnWeekly_Wages<.                                     ,                 c(`=National_Average_FL') graph_options(legend(off) xscale(off fill) graphregion(margin(zero))    nodraw name(Schoolingnew, replace))
  graph combine SchoolingOrig Schoolingdefault Schoolingnew, rows(1) l1title("Years of schooling, age≤35", size(small)) graphregion(margin(zero)) nodraw name(Schooling, replace)

  rdplot K23_Wages1 K23_Female_Literacy if K23_Age<=35 & abs(K23_Female_Literacy-.39286)<.2, nbins(8 8) p(2) c(.39286) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(.2 "20%" .4 "40%" .6 "60%") text(1400 .39286 "Eligibility threshold (39.29%)", place(se)))
  rdplot K23_Wages  K23_Female_Literacy if K23_Age<=35                                     , c(.39286) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(5000 .39286 "Eligibility threshold (39.29%)", place(se)))
  rdplot Wages Female_Literacy if Age<=35                                                  , c(.39286) graph_options(legend(off) graphregion(margin(0 2 0 0)) xtitle("Female literacy, 1991") ytitle(Wage earnings of under-35s (rupees/week)) xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") text(5000 .39286 "Eligibility threshold (39.29%)", place(se)))

  rdplot K23_lnWages1       K23_Female_Literacy if K23_Age<=35 & abs(K23_Female_Literacy-.39286)<.2, nbins(8 8) p(2) c(.39286) graph_options(legend(off) graphregion(margin(zero)) nodraw name(WagesOrig, replace))
  rdplot K23_lnWages1       K23_Female_Literacy if K23_Age<=35                                     ,                 c(.39286) graph_options(legend(off) graphregion(margin(2 0 0 0)) nodraw name(Wagesdefault, replace))
  rdplot     lnWages            Female_Literacy if     Age<=35                                     ,                 c(`=National_Average_FL') graph_options(legend(off) graphregion(margin(2 0 0 0)) nodraw name(Wagesnew, replace) ylab(6/9))
  graph combine WagesOrig Wagesdefault Wagesnew, rows(1) l1title("Log wages, age≤35", size(small)) graphregion(margin(zero)) nodraw name(Wages, replace)

  graph combine DPEP Schooling Wages, cols(1) graphregion(margin(zero)) b1title("Female Literacy, 1991", size(small))
  graph export output/rdplot.png, replace width(2000)
}


* Intro table
{
  PrepData

  cap erase output/overview.rtf
  RDtable, agegroups(1) specifications(1 3 4 7) outfile(output/overview.rtf)
  GEtable ,             specifications(1 3 4 6) outfile(output/overview.rtf)
}


* Main RD & GE tables
{
cap erase output/RD.rtf
RDtable, agegroups(1/2) specifications(1/8) ik outfile(output/RD.rtf)

cap erase output/RD41.rtf
RDtable, agegroups(1/2) specifications(1/8) ik outfile(output/RD41.rtf) c(.41)

cap erase output/GE.rtf
GEtable ,                specifications(1/7) outfile(output/GE.rtf)

cap erase output/GE41.rtf  // do at .41 cutoff
GEtable ,                specifications(1/7) c(.41) outfile(output/GE41.rtf)
}

preserve
keep if K23_Age<35 & K23_DPEP<. & K23_Years_Schooling<=14
estGE, schoolvar(K23_Years_Schooling) wagevar(Wages1) prefix(K23_) est(cl)
restore
preserve
keep if Age<35 & DPEP<. & Years_Schooling_Formal<=14 & single_parent==1
estGE, nobug schoolvar(Years_Schooling_Formal) wagevar(Wages) ln est(cl) c(.41)
restore


* FRD regression on new data cited in text as being weakly identified
rdrobust lnWages Female_Literacy if Skilled & Age<35 & Years_Schooling_Formal<=14 & single_parent==1, fuzzy(DPEP) c(`=National_Average_FL') h(.0408) b(.0917) all
* plot of first stage
scalar h = .0408  // K23 bandwidth
// scalar h = .026  // chose by rdrobust
// scalar h = .112  // chosen if vce(cluster Female_Literacy)
scatter DPEP Female_Literacy, jitter(5) || ///
   lfit DPEP Female_Literacy if Female_Literacy<National_Average_FL  [aw=max(0,h-abs(Female_Literacy-National_Average_FL))] || ///
   lfit DPEP Female_Literacy if Female_Literacy>National_Average_FL  [aw=max(0,h-abs(Female_Literacy-National_Average_FL))] || ///
   lpoly DPEP Female_Literacy if Female_Literacy<National_Average_FL [aw=max(0,h-abs(Female_Literacy-National_Average_FL))] || ///
   lpoly DPEP Female_Literacy if Female_Literacy>National_Average_FL [aw=max(0,h-abs(Female_Literacy-National_Average_FL))] || ///
   if Skilled & Age<35 & Years_Schooling_Formal<=14 & lnWages<. & abs(Female_Literacy-National_Average_FL)<h, xline(`=National_Average_FL') legend(off)


***
*** Sensitivity testing
***

* Treatment by percentile
{
preserve
collapse DPEP=DPEP_AS Female_Literacy if single_parent, by(DISTRICT_CODE)
gen ptile = (floor(Female_Literacy*100) + .5) / 100
collapse DPEP, by(ptile)
twoway bar DPEP ptile, barwidth(.01) xtitle("Female literacy, 1991") ytitle("Fraction") name(DPEPbyptile, replace) text(1 `=National_Average_FL' ".3929", place(e) size(small) margin(small)) ///
  xline(`=National_Average_FL') plotregion(margin(zero)) graphregion(margin(0 2 0 2)) legend(off)
graph export output/DPEPbyptile.png, height(1552) replace
restore
}


* checks for discontinuities in predermined variables
{
cap erase output/cutoff_validation.rtf
preserve
collapse Pop Area Houses HHs Ag_Worker_Share Female_Literacy if single_parent, by(DISTRICT_CODE)
foreach c in `=National_Average_FL' .4 .41 {  // three cutoffs
  eststo clear
  foreach depvar in Pop Area Houses HHs Ag_Worker_Share {
    eststo `depvar': rdrobust `depvar' Female_Literacy, c(`c') scalepar(-1) vce(hc3) all
  }
  esttab using output/cutoff_validation.rtf, append keep(Robust) cells(p(fmt(%3.2g))) nonotes nonumber nogaps nolines noobs mlabels() fonttbl(\f0\fnil $font;)
}
restore
}


keep if Age<35 & single_parent & lnWages<.
cap frame drop plot
frame create plot c tau lo hi
scalar clip = 5  // vertical graphing bound

* sensitivity to threshold
{
foreach wt in "" WEIGHT {
  forvalues s=1/4 {
    local title   : word `s' of "DPEP treatment" "Years of schooling, age≤35" "Log wages, age≤35" "Return to schooling"
    local depvar  : word `s' of DPEP Years_Schooling_Formal lnWages lnWages
    local scalepar: word `s' of -1 -1 -1 1
    local fuzzy   : word `s' of "" "" "" Years_Schooling_Formal
    local vce     : word `s' of hc3 "cluster Female_Literacy" "cluster Female_Literacy" "cluster Female_Literacy"
    
    frame plot: drop if 1
    preserve
    if `s'==1 collapse DPEP Female_Literacy (sum) WEIGHT, by(DISTRICT_CODE)
    forvalues c=.38(.001).42 {
      rdrobust `depvar' Female_Literacy, c(`c') vce(`vce') scalepar(`scalepar') fuzzy(`fuzzy') weights(`wt')
      frame post plot (`c') (cond(abs(e(tau_bc))>clip,.,e(tau_bc))) (cond(e(ci_r_rb)<-clip,.,max(-clip,e(ci_l_rb)))) (cond(e(ci_l_rb)>clip,.,min(clip,e(ci_r_rb))))
    }
    restore

    frame plot: twoway rarea hi lo c, lwidth(none) fcolor(%50) || line tau c, lpat(solid) lwidth(vthin) legend(off) ///
      yline(0, lpat(solid) lcolor(gs8) lwidth(vthin)) plotregion(margin(0 0 1 1)) ///
      ylab(#3, notick) xlab(, format(%6.3g) notick) xline(`=National_Average_FL') ///
      xtitle("") `=cond(`s'<4,"xscale(off)","")' xlab(.38 .4 .41 .42 `=National_Average_FL', format(%6.5g) notick) ytitle("`title'", size(small)) ///
      name(s`s', replace) nodraw
  }
  graph combine s1 s2 s3 s4, cols(1) b1title("   Female literacy cutoff", size(medium)) imargin(0 0 1 1) iscale(1.25) name(csens`wt', replace) xsize(2) graphregion(margin(zero))
  graph export output/csens`wt'.png, width(2000) replace
}
}

* sensitivity to bw
{
foreach wt in "" WEIGHT {
  forvalues s=1/4 {
    local title   : word `s' of "DPEP treatment" "Years of schooling, age≤35" "Log wages, age≤35" "Return to schooling"
    local depvar  : word `s' of DPEP Years_Schooling_Formal lnWages lnWages
    local scalepar: word `s' of -1 -1 -1 1
    local fuzzy   : word `s' of "" "" "" Years_Schooling_Formal
    local vce     : word `s' of hc3 "cluster Female_Literacy" "cluster Female_Literacy" "cluster Female_Literacy"
    
    forvalues c=1/2 {
      local cutoff: word `c' of `=National_Average_FL' .41

      frame plot: drop if 1
      preserve
      if `s'==1 collapse DPEP Female_Literacy (sum) WEIGHT, by(DISTRICT_CODE)
      qui rdrobust `depvar' Female_Literacy, c(`cutoff') vce(`vce') scalepar(`scalepar') fuzzy(`fuzzy') weights(`wt')
      scalar r = e(b_l) / e(h_l)  // ratio of bc bw to estimation bw in default case
      local h_l = e(h_l) 
      forvalues h=.01(.001).15 {
        rdrobust `depvar' Female_Literacy, c(`cutoff') h(`h') b(`=`h'*r') vce(`vce') scalepar(`scalepar') fuzzy(`fuzzy') weights(`wt')
        frame post plot (`h') (cond(abs(e(tau_bc))>clip,.,e(tau_bc))) (cond(e(ci_r_rb)<-clip,.,max(-clip,e(ci_l_rb)))) (cond(e(ci_l_rb)>clip,.,min(clip,e(ci_r_rb))))
      }
      restore

      frame plot: twoway rarea hi lo c, lwidth(none) fcolor(%50) || line tau c, lpat(solid) lwidth(vthin) legend(off) ///
        yline(0, lpat(solid) lcolor(gs8) lwidth(vthin)) xline(`h_l') plotregion(margin(0 0 1 1)) ///
        ylab(, notick) xlab(, format(%6.3g) notick) ///
        xtitle("") `=cond(`s'>1,"",`"title(`"Cutoff = 0`:di %-6.4g `cutoff''"')"')' `=cond(`s'<4,"xscale(off)","")' `=cond(`c'==1,`"ytitle("`title'", size(small))"',"yscale(off)")' ///
        name(c`c', replace) nodraw
    }
    graph combine c1 c2, ycommon rows(1) imargin(3 3 0 0) iscale(1) name(s`s', replace) nodraw
  }
  graph combine s1 s2 s3 s4, cols(1) b1title("   Bandwidth", size(small)) imargin(0 0 2 2) name(bsens`wt', replace) xsize(4) graphregion(margin(zero))
  graph export output/bsens`wt'.png, width(2000) replace
}
}

* sensitivity to donut radius
{
foreach wt in "" WEIGHT {
  forvalues s=1/4 {
    local title   : word `s' of "DPEP treatment" "Years of schooling, age≤35" "Log wages, age≤35" "Return to schooling"
    local depvar  : word `s' of DPEP Years_Schooling_Formal lnWages lnWages
    local scalepar: word `s' of -1 -1 -1 1
    local fuzzy   : word `s' of "" "" "" Years_Schooling_Formal
    local vce     : word `s' of hc3 "cluster Female_Literacy" "cluster Female_Literacy" "cluster Female_Literacy"
    
    forvalues c=1/2 {
      local cutoff: word `c' of `=National_Average_FL' .41

      frame plot: drop if 1
      preserve
      if `s'==1 collapse DPEP Female_Literacy (sum) WEIGHT, by(DISTRICT_CODE)
      forvalues r=0(.0001).01 {
        rdrobust `depvar' Female_Literacy if abs(Female_Literacy - `cutoff') > `r', c(`cutoff') vce(`vce') scalepar(`scalepar') fuzzy(`fuzzy') weights(`wt')
        frame post plot (`r') (cond(abs(e(tau_bc))>clip,.,e(tau_bc))) (cond(e(ci_r_rb)<-clip,.,max(-clip,e(ci_l_rb)))) (cond(e(ci_l_rb)>clip,.,min(clip,e(ci_r_rb))))
      }
      restore

      frame plot: twoway rarea hi lo c, lwidth(none) fcolor(%50) || line tau c, lpat(solid) lwidth(vthin) legend(off) ///
        yline(0, lpat(solid) lcolor(gs8) lwidth(vthin)) plotregion(margin(0 0 1 1)) ///
        ylab(, notick) xlab(, format(%6.3g) notick) ///
        xtitle("") `=cond(`s'>1,"",`"title(`"Cutoff = 0`:di %-6.4g `cutoff''"')"')' `=cond(`s'<4,"xscale(off)","")' `=cond(`c'==1,`"ytitle("`title'", size(small))"',"yscale(off)")' ///
        name(c`c', replace) nodraw
    }
    graph combine c1 c2, ycommon rows(1) imargin(3 3 0 0) iscale(1) name(s`s', replace) nodraw
  }
  graph combine s1 s2 s3 s4, cols(1) b1title("   Donut hole radius", size(small)) imargin(0 0 2 2) name(dsens`wt', replace) xsize(4) graphregion(margin(zero))
  graph export output/dsens`wt'.png, width(2000) replace
}
}



***
*** GE effects & elasticities
***

* revised calculation as described in appendix B
{
  cap program drop estGENew
  program define estGENew, rclass
    scalar National_Average_FL = 129752482 / 330286606
    syntax, [c(real `=National_Average_FL') weight(string)]
    set tracedepth 1
    cap noi {
      foreach yo in y o {
        foreach su in s u {
          rdrobust `su' x if `yo', fuzzy(D) c(`c') weights(`weight') all vce(cluster x) // for young, skilled this is fraction switched, Table 3, top left
          scalar Δℓ_`yo'`su' = e(tau_bc)
          sum `su' if abs(x - National_Average_FL)<.1 & `yo' [aw = max(0, e(h_l) - abs(x-National_Average_FL)) `=cond("`weight'"!="","* `weight'","")'], meanonly
          scalar ℓ_`yo'`su' = r(mean)
          scalar ℓ_`yo'`su'D1 = ℓ_`yo'`su' + Δℓ_`yo'`su' / 2  // estimates of labor shares in program and non-program districts compatible with FRD estimate of their difference
          scalar ℓ_`yo'`su'D0 = ℓ_`yo'`su' - Δℓ_`yo'`su' / 2

          di "ℓ_`yo'`su' = " ℓ_`yo'`su'
          di "ℓ_`yo'`su'D1 = " ℓ_`yo'`su'D1
          di "ℓ_`yo'`su'D0 = " ℓ_`yo'`su'D0

          foreach depvar in w S {
            rdrobust `depvar' x if `su' & `yo', fuzzy(D) c(`c') weights(`weight') all vce(cluster x)
            scalar Δ`depvar'_`yo'`su' = e(tau_bc)
            sum `depvar' if abs(x - National_Average_FL)<.1 & `su' & `yo' [aw = max(0, e(h_l) - abs(x-National_Average_FL)) `=cond("`weight'"!="","* `weight'","")'], meanonly
            scalar `depvar'_`yo'`su'D1 = r(mean) + Δ`depvar'_`yo'`su' / 2  // estimates of schooling averages in program and non-program districts compatible with FRD estimate of their difference
            scalar `depvar'_`yo'`su'D0 = r(mean) - Δ`depvar'_`yo'`su' / 2
            
            di "Δ`depvar'_`yo'`su' = " Δ`depvar'_`yo'`su'
            di "`depvar'_`yo'`su'D1 = " `depvar'_`yo'`su'D1
            di "`depvar'_`yo'`su'D0 = " `depvar'_`yo'`su'D0
          }
        }
        scalar β₀_`yo' = w_`yo'sD0 - w_`yo'uD0  // direct computation of skill premia
        scalar β₁_`yo' = w_`yo'sD1 - w_`yo'uD1
        scalar Δβ_`yo' = β₁_`yo' - β₀_`yo'
        return scalar β～₀_`yo' = β₀_`yo' / (S_`yo'sD0 - S_`yo'uD0)
        return scalar β～₁_`yo' = β₁_`yo' / (S_`yo'sD1 - S_`yo'uD1)
        return scalar Δβ～_`yo' = return(β～₁_`yo') - return(β～₀_`yo')
        return scalar Δβpct_`yo' = return(Δβ～_`yo') / return(β～₀_`yo') * 100

        di "β₀_`yo' = " β₀_`yo'
        di "β₁_`yo' = " β₁_`yo'
        di "Δβ_`yo' = " Δβ_`yo'
        di "β～₀_`yo' = " return(β～₀_`yo')
        di "β～₁_`yo' = " return(β～₁_`yo')
        di "Δβ～_`yo' = " return(Δβ～_`yo')
      }
      foreach su in s u {
        rdrobust `su' x, fuzzy(D) c(`c') weights(`weight') all vce(cluster x)
        scalar ΔL_`su' = e(tau_bc)
      }
      return scalar σ_A = - (Δℓ_ys/ℓ_ys - Δℓ_yu/ℓ_yu - Δℓ_os/ℓ_os + Δℓ_ou/ℓ_ou) / (Δβ_y - Δβ_o)
      return scalar σ_E = - (Δw_ys - Δw_yu - (ΔL_s - ΔL_u - Δℓ_ys/ℓ_ys +Δℓ_yu/ℓ_yu) / return(σ_A)) / (ΔL_s - ΔL_u)
      est clear  // so parallel bs doesn't look for e(b), which might not be present if a replication crashes?
    }
  end

  PrepData
  
  preserve
  keep if lnWages<. & Female_Literacy<. & Years_Schooling<. & Age<=75 & DPEP<.
  rename (DPEP lnWages Years_Schooling Female_Literacy) (D w S x)
  gen byte s = S>= 8
  gen byte u = !s
  gen byte y = Age < 35
  gen byte o = !y
  keep D w S x s u y o WTtr

  eststo clear
  bsGE, exp(β～₀_y=r(β～₀_y) β～₁_y=r(β～₁_y) Δβ～_y=r(Δβ～_y) Δβpct_y=r(Δβpct_y) σ_E=r(σ_E) σ_A=r(σ_A)) cmdline(estGENew,                    )
  bsGE, exp(β～₀_y=r(β～₀_y) β～₁_y=r(β～₁_y) Δβ～_y=r(Δβ～_y) Δβpct_y=r(Δβpct_y) σ_E=r(σ_E) σ_A=r(σ_A)) cmdline(estGENew,        weight(WTtr))
  bsGE, exp(β～₀_y=r(β～₀_y) β～₁_y=r(β～₁_y) Δβ～_y=r(Δβ～_y) Δβpct_y=r(Δβpct_y) σ_E=r(σ_E) σ_A=r(σ_A)) cmdline(estGENew, c(.41)             )
  bsGE, exp(β～₀_y=r(β～₀_y) β～₁_y=r(β～₁_y) Δβ～_y=r(Δβ～_y) Δβpct_y=r(Δβpct_y) σ_E=r(σ_E) σ_A=r(σ_A)) cmdline(estGENew, c(.41) weight(WTtr))
  esttab using output/GEnew.rtf, replace ///
    cell(b(fmt(%6.3f)) p(fmt(%6.2f) par(( ))) ci_percentile[ll](fmt(a2)    par("[" "")) & ci_percentile[ul](fmt(a2) par("" "]"))) incelldelimiter(", ") drop(Δβpct_y) noobs nonote nomtitles nonum nogaps nolines fonttbl(\f0\fnil $font;) msign("–")
  esttab using output/GEnew.rtf, append ///
    cell(b(fmt(%6.0f)) p(fmt(%6.2f) par(( ))) ci_percentile[ll](fmt(%6.0f) par("[" "")) & ci_percentile[ul](fmt(%6.0f) par("" "]"))) incelldelimiter(", ") keep(Δβpct_y) noobs nonote nomtitles nonum nogaps nolines fonttbl(\f0\fnil $font;) msign("–")

  restore
}



* Versions of Khanna report Tables 1 & 2 with better inference, in response to JPE reports
{
cap erase output/KhannaReport.rtf
forvalues s=1/3 {
  local depvar: word `s' of Years_Schooling_Formal lnWages lnWages
  local fuzzy: word `s' of "" "" Years_Schooling_Formal
  local scalepar: word `s' of -1 -1 1
  eststo clear
  eststo: rdrobust `depvar' Female_Literacy                                                                      , all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar')  vce(cluster Female_Literacy)  // Table 1
  cap drop d
  gen d = abs(National_Average_FL - Female_Literacy)
  sum d, meanonly
  eststo: rdrobust `depvar' Female_Literacy if d!=r(min)                                                         , all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar') vce(cluster Female_Literacy)  // Table 2, Panel A
  eststo: rdrobust `depvar' Female_Literacy if d > .004                                                          , all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar') vce(cluster Female_Literacy) // Table 2, Panel B
  eststo: rdrobust `depvar' Female_Literacy if d > .004 | !inlist(District_Name,"Almora","Bageshwar")            , all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar') vce(cluster Female_Literacy) // Table 2, Panel C
  eststo: rdrobust `depvar' Female_Literacy if d > .004 | !inlist(District_Name,"Almora","Bageshwar","Cuddalore"), all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar') vce(cluster Female_Literacy) // Table 2, Panel D
  eststo: rdrobust `depvar' Female_Literacy, kernel(uniform)                                                       all c(`=National_Average_FL') fuzzy(`fuzzy') scalepar(`scalepar') vce(cluster Female_Literacy) // Table 2, Panel D

  estadd scalar N_h = e(N_h_l) + e(N_h_r), replace: *
  esttab using output/KhannaReport.rtf, append keep(Robust) se stat(h_l N_h, labels(Bandwidth Observations) fmt(%5.3g %8.0gc)) nonotes nonumber nogaps nolines nomtitles star(* .1 ** .05 *** .01) msign("–") fonttbl(\f0\fnil $font;)
}
}

* vary bandwidth in DID: used in response to JPE reports
{
scalar clip = 3
PrepData
keep if Age<=75 & Female_Literacy < . & lnWages<.
gen byte Young = Age<35
forvalues i=1/2 {
  cap drop IT
  gen byte IT = cond(`i'==1, DPEP, Female_Literacy < National_Average_FL)
  forvalues s=1/3 {
    local title: word `s' of Schooling "Log wages" "Return to schooling"
    local depvar: word `s' of Years_Schooling_Formal lnWages lnWages
    local iv: word `s' of "" "" iv
    local indepvar: word `s' of 1.Young#1.IT 1.Young#1.IT "(Years_Schooling_Formal = 1.Young#1.IT)"
    cwf default
    cap frame drop DID
    frame create DID double bw b se
    forvalues bw=.075(.025).25 {
      reghdfejl `depvar' `indepvar' if abs(Female_Literacy - National_Average_FL)<`bw', cluster(DISTRICT_CODE) a(Age DISTRICT_CODE)
      frame post DID (`bw') (e(b)[1,1]) (sqrt(e(V)[1,1]))
    }
    cwf DID
    gen lo = max(-clip, b - 1.96*se)
    gen hi = min( clip, b + 1.96*se)
    twoway rspike lo hi bw , yline(0) ytitle("`title'") xtitle("Bandwidth") || scatter b bw if abs(b)<clip, msym(o) legend(off) name(DID`i'`s', replace)
    cwf default
  }
}
graph combine DID11 DID21 DID12 DID22 DID13 DID23, cols(2) imargin(zero) title("           Using treatment                                         Using intention to treat") graphregion(margin(zero))
graph export output/DID.png, replace width(2000)
}


