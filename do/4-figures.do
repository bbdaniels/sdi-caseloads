// Figure 1. Country-facility variation for all facilities
use "${git}/constructed/capacity.dta" , clear

  clonevar weight = cap

  replace weight = (hf_provs/hf_provs_vig)

  graph box cap [aweight=weight] , hor ///
    over(country , sort(1)) ///
    noout note(" ")  ///
    box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black))  ///
    ytit("Daily Outpatient Caseload per Provider")

  graph export "${git}/outputs/main/f-summary-capacity.png" , replace

// Figure 2. Queuing pattern simulations at various caseloads
clear

  local x = 1
  set seed 123396
  foreach pats in 15 20 30 40 {
    qui q_up 360 `pats' 10
      return list
        local temp = 6 - `r(idle_time)' * 6

        local idle : di %3.1f `temp'
        local wait : di %3.1f `r(mean_wait)'
        local pati = `r(total_pats)'

    egen check = rownonmiss(q*)
    replace period = period/60
    gen zero = 0
    tw (rarea check zero  period , lc(white%0) fc(gray) connect(stairstep)) ///
       (line check period , lc(black) connect(stairstep)) ///
       (scatter check period if service != . & check == 0 , mc(black) m(.)) ///
       (scatter check period if service == . , mc(red) m(.)) ///
     , ytit("Patients Waiting in Queue") title("`pats' Patients per 6-Hour Day") yscale(r(0)) ylab(0(1)8) ///
       xtit("Provider Work Hours: `idle' | Mean Patient Wait: `wait' Min.") ///
       xlab(0 "Hours {&rarr}" 1 2 3 4 5 6 "Close") xoverhang ///
       legend(on order(3 "Serving Patients" 4 "No Patients" 1 "Queue Length") ///
              r(1)  pos(12) ring(1) symxsize(small))

      graph save "${git}/outputs/temp/queue-`x'.gph" , replace
        local ++x
  }

  grc1leg ///
    "${git}/outputs/temp/queue-1.gph" ///
    "${git}/outputs/temp/queue-2.gph" ///
    "${git}/outputs/temp/queue-3.gph" ///
    "${git}/outputs/temp/queue-4.gph" ///
  , altshrink ycom

    graph draw, ysize(6)
  graph export "${git}/outputs/main/f-queue-crowding.png" , width(3000) replace

// Figure 3. Distribution of queuing patterns at various caseloads
clear
tempfile results
save `results' , emptyok

  set seed 836503
  foreach pats in  15 20 30 40 {

    simulate ///
      wait = r(mean_wait) idle = r(idle_time) ///
      , reps(100) ///
      : q_up 360 `pats' 10

      gen pats = `pats'

      append using `results'
        save `results' , replace

  }

  replace wait = 1.25 if wait < 1.25
  tw (scatter idle wait if pats == 15 , mfc(none) mlc(black) mlw(medthin) msize(medium)) ///
     (scatter idle wait if pats == 20 , m(t) mfc(none) mlc(blue) mlw(medthin) msize(medium)) ///
     (scatter idle wait if pats == 30 , m(S) mfc(none) mlc(green) mlw(medthin) msize(medium)) ///
     (scatter idle wait if pats == 40 , m(D) mfc(none) mlc(red) mlw(medthin) msize(medium)) ///
  , legend(on pos(2) c(1) ring(0) ///
    order(1 "15 Patients/Day" 2 "20 Patients/Day" 3 "30 Patients/Day" 4 "40 Patients/Day")) ///
    xtit("Mean Waiting Time for Serviced Patients (Minutes)") xscale(log) ///
    xlab(1.25 "No Wait" 2.5 5 10 20 40 80) ///
    ytit("Excess Capacity for Provider") ylab(1 "100%" .75 "75%" .5 "50%" .25 "25%" 0 "0%")

    graph export "${git}/outputs/main/f-queue-simulations.png" , width(3000) replace

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
    legend(on pos(12) symxsize(small) order (0 "By Caseload:" 1 "Top 20% of Providers" 2 "Next 20%" 3 "Bottom 60%") c(1)) ///
    ytitle("Proportion of Total Outpatients") ylab(0(0.25)1) blab(bar, pos(center) format(%9.2f) ) ///
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
    legend(on pos(12) symxsize(small) order (0 "By Competence:" 1 "Top 20% of Providers" 2 "Next 20%" 3 "Bottom 60%") c(1)) ///
    ytitle("Proportion of Total Outpatients") ylab(0(0.25)1) blab(bar, pos(center) format(%9.2f) ) ///
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

    bys country: gen weight = 1/_N

    reg cap treat i.country  b0.hf_type [pweight=weight] , cl(fid)
    local b = _b[treat] / 100
      local totalb : di %3.2f `b'
    local p = r(table)[4,1]
      local totalp : di %3.2f `p'

      drop if country ==

  levelsof country, local(c)
  local x = 0
  local legend ""
  qui foreach country in `c' {
    local ++x
    local ++x
    local label : label (country) `country'
    reg cap treat if country == `country' [pweight=weight] , cl(fid)
      local b = _b[treat] / 100
        local b : di %3.2f `b'
      local p = r(table)[4,1]
        local p : di %3.2f `p'

    local legend `"`legend' `x' "`label': `b' ({it:p}=`p')"  "'
  }

  binsreg  cap treat [aweight=hf_provs/hf_provs_vig], by(country) polyreg(1) ///
    legend(on symxsize(small) pos(12) c(2) ///
      order(`legend')  size(vsmall) ///
      title("Overall Caseload Increase per Percent Vignettes Correct: `totalb' ({it:p}=`totalp')" , size(vsmall))) ///
    dotsplotopt(m(.)) ysize(6) xoverhang ///
    xlab(${pct}) xtit("Vignettes Correctly Treated") ///
    ytit("Patients per Provider Day")

    graph export "${git}/outputs/main/f-raw-correlations.png" , replace

// Figure 6. Correlations in data between caseload and competence (Optimized)
use "${git}/constructed/capacity.dta" , clear

  bys country: egen check = sum(cap)
    gen check2 = cap/check

    bys country: gen pweight = 1/_N

  bys country: gen weight = check2/_N

  egen treat = rowmean(treat?)
  reg treat c.irt##i.country##i.hf_type
  drop treat
    predict treat
    replace treat = 0 if treat < 0

  mean treat [pweight=weight]
    mat a = r(table)
    local old = a[1,1]

  keep treat country hf_type pweight weight irt cap hf_provs hf_provs_vig

  bys country hf_type (irt): gen srno = _n
    tempfile irtrank
    save `irtrank'
  drop srno
  ren (irt treat cap) (irt_old treat_old cap_old)

  bys country hf_type (cap_old): gen srno = _n
    merge 1:1 country hf_type srno using `irtrank'

  mean treat [pweight=weight]
   mat a = r(table)
   local new = a[1,1]

  tw (histogram treat , w(.0625) start(0) gap(10) lw(none) fc(gs12) yaxis(2) percent) ///
    (lowess cap_old treat , lc(black) lw(thick)) ///
    (lowess cap_old treat_old , lp(dash) lw(thick) lc(black)) ///
  ,  by(country, c(2) yrescale legend(pos(12)) note(" ") ixaxes imargin(0)) ///
    yscale(alt) yscale(alt  axis(2)) ytitle("Percentage of Providers (Histogram)" , axis(2)) ///
    ytitle("") xtit("{&uarr} L Axis: Patients/Day (Lines) | X Axis: Vignettes Correct | R Axis: % of Providers (Histogram) {&uarr}", size(vsmall)) ///
    legend(on pos(12) order(3 "Observed in Data" 2 "Optimally Reallocated") size(small) region(lp(blank))) ///
    ylab(#4, axis(2)) xlab(${pct}) legend(off) ysize(6) subtitle(,fc(none) lc(none))

    graph export "${git}/outputs/main/f-optimization.png" , replace


// End
