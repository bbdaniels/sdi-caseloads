// Figures/tables for paper

// Summary table: Country-facility crowding for small facilities
use "${git}/constructed/capacity-fac.dta" , clear // if hf_provs <10

  expand 2 , gen(check)
    replace country = 1 if check == 1

    bys country: gen count = _N

  mean count, over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = a

  mean hf_staff_op, over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  mean hf_provs , over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  mean cap , over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  mean cap [aweight=cap] , over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  mean cap_prov [fweight=hf_provs], over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  gen w2 = cap_prov*hf_provs
  mean cap_prov [aweight=w2], over(country)
    mat a = r(table)'
    mat a = a[1...,1]
    mat results = results, a

  matlist results
  mat results_STARS = J(rowsof(results),colsof(results),0)

  outwrite results ///
    using "${git}/outputs/main/t-summary-capacity.xlsx" ///
  , replace ///
    rownames("Total" "Kenya" "Madagascar" "Malawi" "Mozambique" "Niger" ///
             "Nigeria" "Sierra Leone" "Tanzania" "Togo" "Uganda") ///
    colnames("Facilities (N)" "Providers per Facility" "Providers Present" ///
             "Patients per Facility Day" "(Average Patient)" ///
             "Patients per Provider Day" "(Average Patient)")


// Summary figure: Country-facility variation for small facilities

use "${git}/constructed/capacity.dta" , clear // if hf_staff_op <=10

  gen c2 = hf_outpatient/60
  drop if hf_outpatient == . | cap == .
  egen tag = tag(fid)

  replace cap = 200 if cap > 200
    lab var cap "Provider Experience: Daily Outpatients"
  graph hbox cap , over(country , sort(1)) noout saving("${git}/outputs/temp/g1.gph" , replace) nodraw note(" ") ///
      box(1 , lc(black) lw(thin)) ///
      marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
      inten(0) cwhi lines(lw(thin) lc(black))

  lab var c2 "Daily Outpatients Per Facility"
    replace c2 = 200 if c2 > 200
  graph hbox c2 if tag == 1 , over(country , sort(1)) noout saving("${git}/outputs/temp/gf.gph" , replace) nodraw note(" ") ///
      box(1 , lc(black) lw(thin)) ///
      marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
      inten(0) cwhi lines(lw(thin) lc(black))

  lab var cap "Patient Experience: Daily Outpatients at Provider"
  graph hbox cap [aweight=cap], over(country , sort(1)) noout saving("${git}/outputs/temp/g2.gph" , replace) nodraw note(" ") ///
      box(1 , lc(black) lw(thin)) ///
      marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
      inten(0) cwhi lines(lw(thin) lc(black))

  graph combine ///
    "${git}/outputs/temp/gf.gph" ///
    "${git}/outputs/temp/g1.gph" ///
    "${git}/outputs/temp/g2.gph" ///
  , ysize(6) c(1) xcom imargins(none) altshrink

  graph export "${git}/outputs/main/f-summary-capacity.png" , replace

// Summary figure: Country-facility variation for (sm)all facilities

use "${git}/constructed/capacity-fac.dta" , clear //if hf_staff_op <=10

  replace hf_staff_op = 11 if hf_staff_op > 10

  tw (scatter  hf_provs hf_staff_op if hf_staff_op > 0 & hf_provs < 10  ///
      , jitter(7) msize(vtiny) m(.) mc(black)) ///
     (scatter hf_provs hf_staff_op if hf_staff_op > 0 & hf_provs >= 10  ///
         , jitter(7) msize(vtiny) m(.) mc(red)) ///
  , xlab(0 " " 1(1)9 10 11 ">10" 12 " ") ylab(0 " "  1(1)10 11 " ")

  graph export "${git}/outputs/main/f-facility-capacity.png" , replace

// Summary figure: Correlations in data between caseload and competence

use "${git}/constructed/capacity.dta" , clear // if hf_staff_op <=10

  egen treat = rowmean(treat?)

  levelsof country, local(c)
  local x = 0
  local legend ""
  qui foreach country in `c' {
    local ++x
    local ++x
    local label : label (country) `country'
    reg cap treat if country == `country' , cl(fid)
      local b = _b[treat]
        local b : di %3.2f `b'
      local p = r(table)[4,1]
        local p : di %3.2f `p'

    local legend `"`legend' `x' "`label': `b' (p=`p')"  "'
  }

  binsreg  cap treat, by(country) polyreg(1) ///
    legend(on symxsize(small) pos(12) c(2) ///
      order(`legend') size(vsmall)) ///
    dotsplotopt(m(.)) ysize(6) xoverhang ///
    xlab(${pct}) xtit("Vignettes Correctly Treated") ///
    ytit("Patients per Provider Day")

    graph export "${git}/outputs/main/f-raw-correlations.png" , replace

// Summary figure: Correlations in data between caseload and competence (Optimized)
use "${git}/constructed/capacity.dta", clear

  bys country: gen weight = 1/_N
  keep if cap != . & irt != .
  drop if hf_type == .

  qui forv i = 1/7 {
    logit treat`i' c.irt##i.country
      predict p`i' , pr
    gen x`i' = !missing(p`i')
  }

  egen x = rowtotal(x?)
  egen p = rowtotal(p?)
   gen treat = p/x

  mean treat [pweight=cap*weight]
    mat a = r(table)
    local old = a[1,1]

  keep treat country hf_type weight irt cap

  bys country hf_type (irt): gen srno = _n
    tempfile irtrank
    save `irtrank'
  drop srno
  ren (irt treat cap) (irt_old treat_old cap_old)

  bys country hf_type (cap_old): gen srno = _n
    merge 1:1 country hf_type srno using `irtrank'

  mean treat [pweight=cap_old*weight]
   mat a = r(table)
   local new = a[1,1]

  tw (histogram treat , w(.0625) start(0) gap(10) lw(none) fc(gs12) yaxis(2) percent) ///
    (lowess cap_old treat , lc(black) lw(thick)) ///
    (lowess cap_old treat_old , lp(dash) lw(thick) lc(black)) ///
    (pci 0 `new' 20 `new' , yaxis(2) lc(black) lw(thick)) ///
    (pci 0 `old' 20 `old' , yaxis(2) lc(black) lw(thick) lp(dash)) ///
  ,  yscale(alt) yscale(alt  axis(2)) ytitle("Percentage of Providers (Histogram)" , axis(2)) ///
    ytitle("Patients per Provider Day") xtit("Share of Vignettes Correct (Vertical Lines = Means)") ///
    legend(on pos(12) order(3 "Observed in Survey" 2 "Optimal Provider Allocation") size(small) region(lp(blank))) ///
    ylab(0(100)600) ylab(0(5)20 , axis(2)) xlab(${pct})

    graph export "${git}/outputs/main/f-optimization.png" , replace

  tw ///
    (kdensity treat_old [aweight=cap_old*weight] , lc(black) lw(thick) lp(dash)) ///
    (kdensity treat [aweight=cap_old*weight] , lc(black) lw(thick)) ///
    (pci 0 `new' 3 `new' , yaxis(2) lc(black) lw(thick)) ///
    (pci 0 `old' 3 `old' , yaxis(2) lc(black) lw(thick) lp(dash)) ///
    , legend(on pos(12) size(small) order(1 "Observed in Survey" 2 "Optimal Patient Distribution")) ///
      ytit("Density of Patient Distribution") xtit("Share of Vignettes Correct (Vertical Lines = Means)") xlab(${pct}) xoverhang

      graph export "${git}/outputs/main/f-optimization-pat.png" , replace

// Cumulative Capacity
use "${git}/constructed/capacity.dta", clear

  gen c2 = .
  forv i = 2/11 {
    xtile temp = cap if country == `i' , n(100)
    replace c2 = temp if country == `i'
    drop temp
  }

  gen val = 1
    replace val = 2 if c2 < 81
    replace val = 3 if c2 < 61

  collapse (sum) cap , by(country val)
    bys country : egen tot = sum(cap)
    gen pct = cap/tot

  graph hbar pct , over(val) over(country) asy stack ///
    legend(on pos(12) symxsize(small) order (1 "20% Busiest Providers" 2 "Next 20%" 3 "Bottom 60%") c(1)) ///
    ytitle("Share of Total Outpatients") ylab(${pct}) blab(bar, pos(center) format(%9.2f) ) ///
    bar(1 , fc(gs12) lc(black)) bar(2 , fc(gs14) lc(black)) bar(3 , fc(gs16) lc(black))

    graph save "${git}/outputs/temp/f-distribution-cap.gph" , replace

use "${git}/constructed/capacity.dta", clear

  gen c2 = .
  forv i = 2/11 {
    xtile temp = irt if country == `i' , n(100)
    replace c2 = temp if country == `i'
    drop temp
  }

  gen val = 1
    replace val = 2 if c2 < 81
    replace val = 3 if c2 < 61

  collapse (sum) cap , by(country val)
    bys country : egen tot = sum(cap)
    gen pct = cap/tot

  graph hbar pct , over(val) over(country) asy stack ///
    legend(on pos(12) symxsize(small) order (1 "20% Best Providers" 2 "Next 20%" 3 "Bottom 60%") c(1)) ///
    ytitle("Share of Total Outpatients") ylab(${pct}) blab(bar, pos(center) format(%9.2f) ) ///
    bar(1 , fc(gs12) lc(black)) bar(2 , fc(gs14) lc(black)) bar(3 , fc(gs16) lc(black))

    graph save "${git}/outputs/temp/f-distribution-irt.gph" , replace


  graph combine ///
    "${git}/outputs/temp/f-distribution-cap.gph" ///
    "${git}/outputs/temp/f-distribution-irt.gph" ///
  , r(1)

    graph export "${git}/outputs/main/f-distribution-pat.png" , replace


**************************************************
// Figure: Vizualizations for correctness
**************************************************

use "${git}/constructed/capacity-comparison.dta", clear

  drop smean
  ren irt_unrest smean
  keep irt smean country x
  reshape wide irt smean, i(country) j(x) string
  decode country, gen(ccode)

  append using "${git}/constructed/capacity-optimized.dta"
  egen correct = rowmean(treat?)

  tw (fpfitci correct irt_old ///
      if irt_old > -1 & irt_old < 3 , lc(black) fc(gray) alc(white%0)) ///
    (pcarrow irtCorrect irtKnowledge smeanCorrect smeanKnowledge ///
      ,  ml(country) mlabang(20) lc(red) mc(red) mlabc(black) mlabpos(2)) ///
    , ytit("Vignettes Correct Before and After Reallocation") ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%") ///
      xtit("Average Interaction Competence Before and After Reallocation") ///
      legend(on r(1) pos(7) order(2 "Overall Mean" 3 "Actual by Country") ring(0)) ///
      title("Quality Gains from Provider Reallocation")

    graph export "${git}/outputs/main/f-optimization-change.png" , replace


* ------- NEW EOF --------
--------------------------


  // keep if hf_staff_op < 10
  replace hf_staff_op = hf_op_count * (hf_absent) if hf_staff_op == 10

  gen hf_outpatient_day = hf_outpatient/90
  clonevar cap_old = hf_outpatient_day
  clonevar irt_old = irt

  collapse (mean) hf_outpatient_day hf_staff_op irt_old ///
    (rawsum) cap_old , by(country hf_level)

    expand 2 , gen(check)
      replace country = 0 if check == 1

  collapse (mean) hf_outpatient_day hf_staff_op irt_old ///
    (rawsum) cap_old , by(country hf_level)

    drop if hf_level == . | cap_old == 0
    recode hf_level (1=1 "Health Post")(2=3 "Hospital")(3=2 "Clinic") , gen(level)
    sort country level

  gen c2 = hf_outpatient_day/hf_staff_op
  egen temp = sum(cap_old) , by(country)
  gen n = cap_old/temp
  gen t = c2 * (6.8/60)

  lab var n "Outpatient Share"
  lab var irt_old "Mean Competence"
  lab var hf_outpatient_day "Daily Outpatients per Facility"
  lab var hf_staff_op "Outpatient Staff"
  lab var c2 "Outpatients per Staff Day"
  lab var level "Level"
  lab var t "Hours per Provider Day"

  export excel ///
    country level hf_outpatient_day hf_staff_op c2 t n irt_old  ///
  using "${git}/outputs/main/t-summary-capacity.xlsx" ///
  , replace first(varl)

// Tables of comparative statistics
  use "${git}/constructed/capacity-comparison.dta" , replace

  foreach var in irt smean dmean {
    preserve
    use "${git}/constructed/capacity.dta", clear
      egen correct = rowmean(treat?)
      ren irt `var'
      reg correct `var' i.country
    restore
    predict `var'_c
  }
    gen sdifc = smean_c - irt_c if x == "Knowledge"
      bys country: egen temp = mean(sdifc)
      replace sdifc = temp
      drop temp
      replace sdifc = . if x == "Knowledge"
    gen ddifc = dmean_c - irt_c if x == "Knowledge"
      bys country: egen temp = mean(ddifc)
      replace ddifc = temp
      drop temp
      replace ddifc = . if x == "Knowledge"

  gsort -x country

  export excel country x ///
    irt smean sdif sdifc irt_unrest irt_cadres irt_public irt_levels irt_rururb irt_hftype ///
    using "${git}/output/t-optimize-quality-s.xlsx" ///
  , replace first(var)

  export excel country x ///
    irt dmean ddif ddifc irt_biggco irt_biggse irt_bigg20 irt_bigg30 irt_bigg40 irt_bigg50 ///
    using "${git}/output/t-optimize-quality-d.xlsx" ///
  , replace first(var)

// Figure. Descriptive statistics for facilities
use "${git}/constructed/capacity.dta", clear
  drop if hf_outpatient == . | hf_outpatient == 0 | hf_staff_op == 0

  collapse (mean) hf_outpatient hf_staff_op (count) n = hf_outpatient  ///
      , by(country hf_id) fast

  replace hf_outpatient = hf_outpatient/(60)
  gen hf_outpatient_staff = hf_outpatient/(hf_staff_op)

  bys country: gen weight = 1/_N
  replace hf_outpatient = 200 if hf_outpatient >200 & hf_outpatient != .
  replace hf_outpatient_staff = 200 if hf_outpatient_staff >200 & hf_outpatient_staff != .

  graph box hf_outpatient [pweight=weight] ///
  , over(country , axis(noline) sort(1)) ///
    hor ylab(,angle(0)) box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    ytit(" ") inten(0) cwhi lines(lw(thin) lc(black)) note(" ") title("Per Facility")

    graph save "${git}/temp/fac.gph" , replace

  graph box hf_outpatient_staff [pweight=weight] ///
  , over(country , axis(noline) sort(1)) ///
    hor ylab(,angle(0)) box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    ytit(" ") inten(0) cwhi lines(lw(thin) lc(black)) note(" ") title("Per Provider")

    graph save "${git}/temp/pro.gph" , replace

  graph combine "${git}/temp/fac.gph" "${git}/temp/pro.gph"  , ysize(6) c(1) imargin(none)
    graph export "${git}/output/f-descriptives.png" , replace


// End
