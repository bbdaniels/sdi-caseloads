
// Summary figure: Country-facility variation for (sm)all facilities

use "${git}/constructed/capacity-fac.dta" , clear //if hf_staff_op <=10

  replace hf_staff_op = 11 if hf_staff_op > 10

  tw (scatter  hf_provs hf_staff_op if hf_staff_op > 0 & hf_provs < 10  ///
      , jitter(7) msize(vtiny) m(.) mc(black)) ///
     (scatter hf_provs hf_staff_op if hf_staff_op > 0 & hf_provs >= 10  ///
         , jitter(7) msize(vtiny) m(.) mc(red)) ///
  , xlab(0 " " 1(1)9 10 11 ">10" 12 " ") ylab(0 " "  1(1)10 11 " ")

  graph export "${git}/outputs/main/af-facility-capacity.png" , replace

// Figure. Caseload by facility-provider w absenteeism
use "${git}/constructed/capacity.dta", clear
  drop if hf_outpatient == . | hf_outpatient == 0 | hf_staff_op == 0
  duplicates drop country hf_id , force

  gen x = hf_outpatient/(60*hf_staff_op)
  gen y = hf_outpatient/(60*hf_staff_op*(1-hf_absent))

  graph hbox x y if y != . ///
    , over(hf_type) noout note("") ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black)) ///
    box(1 , lc(black) lw(thick)) box(2 , lc(red) lw(thick)) ///
    legend(on order(1 "Unadjusted" 2 "Absenteeism-Adjusted"))

    graph export "${git}/appendix/af-absenteeism.png" , width(3000) replace


// Figure. Caseload by facility
use "${git}/constructed/capacity.dta", clear
  drop if hf_outpatient == . | hf_outpatient == 0 | hf_staff_op == 0
  duplicates drop country hf_id , force
  gen x = hf_outpatient/90

  levelsof country_string, local(cs)
  foreach country in `cs' {
    tempfile `country'
    graph hbox x if country_string == "`country'" ///
      , over(hf_type) noout nodraw saving(``country'') ///
        ytit("") note("") title("`country'") ///
        marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
        inten(0) cwhi lines(lw(thin) lc(black)) box(1 , lc(black) lw(thin))

      local graphs `"`graphs' "\``country''"  "'
  }
  graph combine `graphs' , c(2) ysize(5) imargin(none)

  graph export "${git}/appendix/af-caseload.png" , width(3000) replace

// Figure. Descriptive statistics for facilities by sector

use "${git}/constructed/capacity.dta", clear
  drop if hf_outpatient == . | hf_outpatient == 0 | hf_staff_op == 0
    gen hf_outpatient_day = hf_outpatient/60

  labelcollapse (mean) irt hf_absent hf_outpatient hf_inpatient hf_staff hf_staff_op hf_type hf_level ///
      , by(country hf_id) vallab(hf_type)

  replace hf_outpatient = hf_outpatient/(60)
    lab var hf_outpatient "Daily Outpatients"
  gen hf_outpatient_staff = hf_outpatient/hf_staff_op
    lab var hf_outpatient_staff "Daily Outpatients per Staff"

    lab var irt "Mean Provider Knowledge"

  bys country hf_type: gen weight = 1/_N
  replace hf_outpatient = 200 if hf_outpatient > 200
  graph box hf_outpatient [pweight=weight] ///
  , over(hf_type , axis(noline) sort(1))  ///
    hor ylab(,angle(0)) box(1 , lc(black) lw(thin)) ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    ytit(" ") inten(0) cwhi lines(lw(thin) lc(black)) note(" ")

  graph export "${git}/appendix/af-descriptives.png" , width(3000) replace

// Calculate new quality
use "${git}/constructed/capacity-optimized.dta", clear
tempfile all

  preserve
    collapse (mean) irt_old (rawsum) cap_old [aweight=cap_old] , by(country hf_type)
    save `all' , replace
  restore

  foreach type in unrest hftype levels rururb public {
    preserve
      collapse irt_`type' [aweight=cap_`type'] , by(country hf_type)
      merge 1:1 country hf_type using `all' , nogen
      save `all' , replace
    restore
  }

  use `all' , clear

  egen temp = sum(cap_old) , by(country)
  gen n = cap_old/temp

  // Visualize sectoral changes

  local style msize(small)
  tw ///
    (pcarrow irt_old n irt_hftype n if hf_type == 1 , `style' mang(30) lc(black) mc(black)) ///
    (pcarrow irt_old n irt_hftype n if hf_type == 2 , `style' mang(60) lc(black) mc(black)) ///
    (pcarrow irt_old n irt_hftype n if hf_type == 3 , `style' mang(90) lc(black) mc(black)) ///
    (pcarrow irt_old n irt_hftype n if hf_type == 4 , `style' mang(30) lc(red) mc(red)) ///
    (pcarrow irt_old n irt_hftype n if hf_type == 5 , `style' mang(60) lc(red) mc(red)) ///
    (pcarrow irt_old n irt_hftype n if hf_type == 6 , `style' mang(90) lc(red) mc(red)) ///
    if irt_old != irt_hftype ///
  , by(country , r(2) rescale ixaxes note(" ")  ///
       legend(ring(0) pos(12))) subtitle(,bc(none)) xsize(6) ///
    xtit("National Share of Outpatients") ytit("Average Provider Competence") ///
    xlab(0 "0%" .25 "25%" .5 "50%" .75 "75%") xscale(noline) ///
    yline(0 , lc(black) lw(thin)) ylab(-2 "-2 SD" -1 "-1 SD" 1 "+1 SD" 2 "+2 SD" 3 "+3 SD" 0 "Mean") yscale(noline) ///
    legend(size(small) symxsize(small) c(4) ///
      order(0 "Rural:" 1 "Hospital" 2 "Clinic" 3 "Health Post" ///
            0 "Urban:" 4 "Hospital" 5 "Clinic" 6 "Health Post" ))

    graph export "${git}/appendix/af-optimize-differences.png" , width(3000) replace

// Figure: Provider upskilling
use "${git}/constructed/optimize-doctors-done.dta" , clear

  replace f = f*100
  graph box irt_new ///
  , over(f) noout ///
    marker(1, m(p) mc(black) msize(tiny)) medtype(cline) medline(lc(red) lw(medthick)) ///
    inten(0) cwhi lines(lw(thin) lc(black)) box(1 , lc(black) lw(thin)) ///
  by(country, c(2) iyaxes yrescale note("") scale(0.7)) ysize(6) ///
    ytit("Average Interaction Competence") note("")

    graph export "${git}/appendix/af-docs-upskill.pdf" , replace

// End
