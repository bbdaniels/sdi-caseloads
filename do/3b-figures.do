// Figures for paper

// Figure 1. Country-facility variation for small facilities

use "${git}/constructed/capacity.dta" , clear // if hf_staff_op <=10

  drop if hf_outpatient == . | cap == .

  clonevar weight = cap
  expand 2 , gen(pat)

  replace weight = 1 if pat == 0

  // replace weight = 200 if weight > 200 & weight != .
  replace cap = 200 if cap > 200 & cap != .

  lab def pat 0 "Providers" 1 "Patients"
    lab val pat pat

  graph box cap [aweight=weight] , hor ///
    over(pat,  axis(noline))  over(country , sort(1)) ///
    noout note(" ")  ///
    box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black))  ///
    ytit("Total Daily Outpatients Encountered")

  graph export "${git}/outputs/main/f-summary-capacity.png" , replace

// Figure 2. Queueing Caseloads

// Figure 3. Queueing Distribution

// Figure 4. Caseload and Competence Distribution
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

// Figure 5. Correlations in data between caseload and competence

use "${git}/constructed/capacity.dta" , clear

  egen treat = rowmean(treat?)

  levelsof country, local(c)
  local x = 0
  local legend ""
  qui foreach country in `c' {
    local ++x
    local ++x
    local label : label (country) `country'
    reg cap treat if country == `country' , cl(fid)
      local b = _b[treat] / 100
        local b : di %3.2f `b'
      local p = r(table)[4,1]
        local p : di %3.2f `p'

    local legend `"`legend' `x' "`label': `b' (p=`p')"  "'
  }

  binsreg  cap treat, by(country) polyreg(1) ///
    legend(on symxsize(small) pos(12) c(2) ///
      order(`legend') size(vsmall) title("Increase in Daily Patients per Percent Correct" , size(small))) ///
    dotsplotopt(m(.)) ysize(6) xoverhang ///
    xlab(${pct}) xtit("Vignettes Correctly Treated") ///
    ytit("Patients per Provider Day")

    graph export "${git}/outputs/main/f-raw-correlations.png" , replace

// Figure 6,7. Correlations in data between caseload and competence (Optimized)
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

// Figure 8: Optimal allocation impacts

use "${git}/constructed/capacity.dta" , clear
  drop if hf_outpatient == . | cap == .

  egen vig = rowmean(treat?)
  reg vig c.irt##i.country##i.hf_type
  drop vig
    predict vig
    replace vig = 0 if vig < 0

  preserve
  gsort country hf_type -irt
    keep country hf_type irt vig
    ren vig vig_hftype
    ren irt irt_hftype
    gen ser_hftype = _n
    tempfile irt
    save `irt' , replace
  restore

  gsort country hf_type -cap
    gen ser_hftype = _n
    merge 1:1 ser_hftype using `irt' , nogen

  clonevar weight = cap
  expand 2 , gen(pat)

  replace vig = vig_hftype if pat == 1

  lab def pat 0 "Observed" 1 "Reallocated"
    lab val pat pat

  graph box vig [aweight=weight] , hor ///
    over(pat,  axis(noline))  over(country , sort(1)) ///
    noout note(" ")  ///
    box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black)) ///
    ylab(${pct}) ///
    ytit("Vignettes Correctly Treated by Provider")

  graph export "${git}/outputs/main/f-optimization-change.png" , replace

// End
