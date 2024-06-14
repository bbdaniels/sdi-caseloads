
// Summary figure: Country-facility variation for (sm)all facilities

use "${git}/constructed/capacity-fac.dta" , clear

  replace hf_staff_op = 11 if hf_staff_op > 10
  drop if hf_staff_op == 0

  tw (scatter  hf_provs_vig hf_staff_op if hf_staff_op > 0 & hf_provs_vig <= 10  ///
      , jitter(7) msize(vtiny) m(.) mc(black)) ///
     (scatter hf_provs_vig hf_staff_op if hf_staff_op > 0 & hf_provs_vig > 10  ///
         , jitter(7) msize(vtiny) m(.) mc(red)) ///
     (lfit hf_provs_vig hf_staff_op  ///
         , lc(red) lw(thick)) ///
  , xlab(0 " " 1(1)9 10 11 "11+" 12 " " , notick) ylab(0 " "  1(1)10 11 " " , notick) ///
    xtit("Staff Serving Outpatients in Roster") ///
    ytit("Staff Observed in Vignettes Sample") ///
    xline(1.5(1)10.5 , lw(thin) lc(gray)) yline(1.5(1)10.5 , lw(thin) lc(gray))

  graph export "${git}/outputs/appendix/af-facility-capacity.png" , replace

// Figure. Caseload by facility
use "${git}/constructed/capacity-fac.dta", clear
  decode country , gen(country_string)

  levelsof country_string, local(cs)
  local graphs ""
  foreach country in `cs' {
    tempfile `country'
    graph hbox cap if country_string == "`country'" ///
      , over(hf_type)  nodraw saving(``country'') ///
        ytit("") note("") title("`country'") ///
        marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
        inten(0) cwhi lines(lw(thin) lc(black)) box(1 , lc(black) lw(thin))

      local graphs `"`graphs' "\``country''"  "'
  }

  graph combine `graphs' , c(2) ysize(5) imargin(none)

  graph export "${git}/outputs/appendix/af-caseload.png" , width(3000) replace


// Queueing Figure. Clumping
clear
  local x = 1
  foreach seed in 969264 089365 739579 8029288 {
    set seed `seed'
    qui q_up 360 30 10
      return list
        local idle : di %3.2f `r(idle_time)'
        local wait : di %3.1f `r(mean_wait)'

      egen check = rownonmiss(q*)
      replace period = period/60
      gen zero = 0
      tw (rarea check zero  period , lc(white%0) fc(gray) connect(stairstep))(line check period , lc(black) connect(stairstep))(scatter check period if service == . , mc(red) m(.)) ///
        , ytit("Patients in Queue") yscale(r(0)) ylab(#6) ///
          xtit("Idle Share: `idle' | Mean Wait: `wait' Min.") xlab(0 "Hours {&rarr}" 1 2 3 4 5 6 "Close") xoverhang ///
          legend(on order(3 "No Patients" 2 "Serving Patients" 1 "Patients Waiting") r(1)  pos(12) ring(1) symxsize(small))

         graph save "${git}/outputs/temp/queue-`x'.gph" , replace
         local ++x
  }

  grc1leg ///
  "${git}/outputs/temp/queue-1.gph" ///
  "${git}/outputs/temp/queue-2.gph" ///
  "${git}/outputs/temp/queue-3.gph" ///
  "${git}/outputs/temp/queue-4.gph" ///
   , altshrink

   graph draw, ysize(6)
   graph export "${git}/outputs/appendix/queue-clumping.png" , width(3000) replace

// Allocation among doctors only when present
use "${git}/constructed/capacity.dta", clear
  gen doctor = cadre == 1
  bys country hf_id: egen hasdoc = max(doctor)
  egen tag = tag(country hf_id) // 37%

  bys country hf_id: egen tot = sum(cap)
    drop if hasdoc & !doctor
    drop cap
    bys country hf_id: gen cap = tot/_N

  append using "${git}/constructed/capacity.dta", gen(old)
    lab def old 0 "Doctor-adjusted" 1 "Original equal split"
      lab val old old
    graph hbox cap , over(old) over(country , sort(1)) ///
      noout note(" ")  ///
      box(1 , lc(black) lw(thin)) ///
      marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
      inten(0) cwhi lines(lw(thin) lc(black)) ///
      ytit("Outpatients per Provider Working Day")

      graph export "${git}/outputs/appendix/af-doctors.png" , width(3000) replace

// Figure: Provider upskilling
use "${git}/constructed/optimize-doctors-done.dta" , clear

  replace f = f*100
  graph box vig_new ///
  , over(f) noout ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black)) box(1 , lc(black) lw(thin)) ///
  by(country, c(2) iyaxes yrescale note("") scale(0.7)) ysize(6) ///
    ytit("Average Interaction Competence") note("") ylab(#3)

    graph export "${git}/outputs/appendix/af-docs-upskill.png" , replace

// Figure.

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

  keep treat country hf_type pweight weight irt cap

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

  tw ///
    (kdensity treat_old [aweight=weight]  , lc(black) lw(thick) lp(dash)) ///
    (kdensity treat [aweight=weight] , lc(black) lw(thick)) ///
    (pci 0 `new' 3 `new' , yaxis(2) lc(black) lw(thick)) ///
    (pci 0 `old' 3 `old' , yaxis(2) lc(black) lw(thick) lp(dash)) ///
    , yscale(off  axis(2)) legend(on pos(12) size(small) order(1 "Observed in Survey" 2 "Optimal Patient Distribution")) ///
      ytit("Density of Patient Distribution") xtit("Share of Vignettes Correct (Vertical Lines = Means)") xlab(${pct}) xoverhang

      graph export "${git}/outputs/appendix/af-optimization-pat.png" , replace

// Figure 8: Optimal allocation impacts

use "${git}/constructed/capacity.dta" , clear

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

  graph export "${git}/outputs/appendix/af-optimization-change.png" , replace

// TABLES

// Summary table: Country-facility crowding
use "${git}/constructed/capacity-fac.dta" if hf_staff_op < 11 , clear

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

  tabstat cap , by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  tabstat cap [aweight=cap], by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  tabstat cap_prov [fweight=hf_provs], by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  tabstat cap_prov [aweight=hf_provs*cap_prov], by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  matlist results
  mat results_STARS = J(rowsof(results),colsof(results),0)

  outwrite results ///
    using "${git}/outputs/appendix/at-summary-capacity.xlsx" ///
  , replace ///
    rownames("Kenya" "Madagascar" "Malawi" "Mozambique" "Niger" ///
             "Nigeria" "Sierra Leone" "Tanzania" "Togo" "Uganda") ///
    colnames("Facilities (N)" "Providers per Facility" "Providers Present" ///
             "Patients per Facility Day" "(Average Patient)" ///
             "Patients per Provider Day" "(Average Patient)")

// Table: Regressions
  use "${git}/constructed/capacity-fac.dta" if hf_staff_op < 11 , clear

    replace hf_type = 0 if hf_type == 6

    bys country: gen weight = 1/_N
    reg cap          b0.hf_type  [pweight=weight] , a(country)
      est sto reg11
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg11
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg11

    reg cap hf_provs b0.hf_type [pweight=weight] , a(country)
      est sto reg21
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg21
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg21

    reg cap_prov hf_provs b0.hf_type  [pweight=weight] , a(country)
      est sto reg31
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg31
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg31

  use "${git}/constructed/capacity.dta" if hf_staff_op < 11 , clear
  bys country: gen weight = 1/_N
     drop if provider_mededuc1 == 1

    egen vig = rowmean(treat?)
    reg vig c.irt##i.country##i.hf_type
    drop vig
      predict vig
      replace vig = 0 if vig < 0
      replace vig = 100*vig

    replace hf_type = 0 if hf_type == 6
    replace cadre = 0 if cadre == 4
    egen fuid = group(country fid)

    areg cap b0.cadre i.provider_mededuc1 [pweight=weight] , a(country) cl(fuid)
    est sto reg1

    areg cap hf_provs b0.hf_type b0.cadre  i.provider_mededuc1 [pweight=weight] , a(country) cl(fuid)
    est sto reg2
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg2
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg2

    areg cap vig hf_provs b0.hf_type b0.cadre  i.provider_mededuc1 [pweight=weight] , a(country) cl(fuid)
    est sto reg3
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg3
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg3

  outwrite reg3 reg11 reg21 reg31 reg1 reg2 reg3      ///
    using "${git}/outputs/appendix/at-regs-capacity.xlsx" ///
  , replace stats(N r2 hos cli) ///
    colnames("X" "1" "2" "3" "4" "5" "6")


// Tables of comparative statistics
use "${git}/constructed/capacity.dta" if hf_staff_op < 11 , clear

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

    expand 2 , gen(fake)
    replace hf_rural = 2 if fake == 1

  collapse ///
    (p25) cap25 = cap vig25 = vig ///
    (p75) cap75 = cap vig75 = vig ///
    (mean) vig vig_hftype fake ///
    [aweight = cap] , by(country hf_rural)

    gen gain = vig_hftype - vig
    gen g2 = gain / vig

    gen caprat = cap75/cap25
    gen vigdif = vig75-vig25

  sort fake hf_rural country

  export excel ///
    country hf_rural vig vig_hftype gain g2 cap25 cap75 caprat vig25 vig75 vigdif  ///
    using "${git}/outputs/appendix/at-optimize-quality.xlsx" ///
  , replace first(var)


// End
