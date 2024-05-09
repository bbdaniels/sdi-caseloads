// Figures for paper

// Summary table: Sectoral shares and statistics
use "${git}/constructed/capacity.dta", clear

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

// Cumulative Capacity
use "${git}/constructed/capacity.dta", clear

  gen hf_outpatient_day = hf_outpatient/60
  gen hf_inpatient_day = hf_inpatient/60
  clonevar cap_old = hf_outpatient_day
  clonevar theta_mle = irt
  gen check = hf_outpatient_day/hf_staff_op

  keep check country_string

  replace check = 1 if check < 1
  replace check = 160 if check >= 160 & check != .

  cdfplot check  ///
  , by(country_string) xlog xscale(log) xlab(1 2 5 10 20 40 80 160, labsize(small) notick) ///
    legend(on c(3) pos(6) size(small) )  ysize(5) scale(0.75) xscale(noline ) yscale(noline ) ///
    ylab(0 "100%" .25 "75%" .5 "50%" .75 "25%" 1 "0%" , notick) ///
    ytit("Share of providers seeing...") xtit("... more than X patients daily") ///
      xline(2 5 10 40 80 , lc(gs14) lw(thin)) ///
      yline(.25 .75 , lc(gs14) lw(thin)) ///
      xline(1 20 160 , lc(black) lw(thin)) ///
      yline(0 .5 1 , lc(black) lw(thin)) ///
    opt1( yscale(reverse)  ///
      lc(blue cranberry cyan dkgreen dkorange emerald gold lavender magenta maroon navy red )) ///
    legend( region(lc(none)) ring(0) c(1) pos(1) ///
      order(1 "Kenya" 2 "Madagascar" 3 "Malawi" 4 "Mozambique" 5 "Niger" ///
            6 "Nigeria" 7 "Sierra Leone" 8 "Tanzania" 9 "Togo" 10 "Uganda"))

      graph export "${git}/output/f-capacity-staff.png" , replace

// Setup: Current comparator for optimization
use "${git}/constructed/capacity.dta", clear

  gen hf_outpatient_day = hf_outpatient/60
  gen hf_inpatient_day = hf_inpatient/60
  clonevar cap_old = hf_outpatient_day
  clonevar theta_mle = irt
  gen check = hf_outpatient_day/hf_staff_op
  bys country: gen weight = 1/_N

  keep if check != . & theta_mle != .
  keep theta_mle check country hf_type weight

  mean theta_mle [pweight=check*weight]
    mat a = r(table)
    local old = a[1,1]

  bys country hf_type (theta_mle): gen srno = _n
    tempfile irtrank
    save `irtrank'
  drop srno
  ren theta_mle theta_old
  bys country hf_type (check): gen srno = _n
    merge 1:1 country hf_type srno using `irtrank'

  mean theta_mle [pweight=check*weight]
   mat a = r(table)
   local new = a[1,1]

  tw (histogram theta_mle , w(0.5) start(-5) gap(10) lw(none) fc(gs12) yaxis(2) percent) ///
    (fpfit check theta_mle [pweight=weight], lc(black) lw(thick)) ///
    (fpfit check theta_old [pweight=weight], lp(dash) lw(thick) lc(black)) ///
    (pci 0 `new' 25 `new' , yaxis(2) lc(black) lw(thick)) ///
    (pci 0 `old' 25 `old' , yaxis(2) lc(black) lw(thick) lp(dash)) ///
  ,  yscale(alt) yscale(alt  axis(2)) ytitle("Percentage of Providers (Histogram)" , axis(2)) ///
    ytitle("Average Patients per Day") xtit("Vignettes Competence (Vertical Lines = Means)") ///
    legend(on pos(12) order(3 "Original Assignment" 2 "Reallocated by Country/Sector") size(small) region(lp(blank))) ///
    xlab(-5(1)5) ylab(0 50 100 150 200 250 300) ylab(0(5)25 , axis(2))

    graph export "${git}/output/f-optimize-providers.png" , replace

**************************************************
// Figure: Vizualizations for correctness
**************************************************

use "${git}/constructed/capacity-comparison.dta", clear

  keep irt dmean smean country x
  reshape wide irt dmean smean, i(country) j(x) string
  decode country, gen(ccode)

  append using "${git}/constructed/capacity-optimized.dta"
  egen correct = rowmean(treat?)

  tw (fpfitci correct irt_old ///
      if irt_old > -1 & irt_old < 3 , lc(black) fc(gray) alc(white%0)) ///
    (pcarrow irtCorrect irtKnowledge dmeanCorrect dmeanKnowledge ///
      , ml(country) mlabang(20) lc(red) mc(red) mlabc(black) mlabpos(2)) ///
    , ytit("Vignettes Correct Before and After Reallocation") ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%") ///
      xtit("Average Interaction Competence Before and After Reallocation") ///
      legend(on r(1) pos(7) order(2 "Theoretical" 3 "Actual") ring(0)) ///
      title("Demand-Side Patient Reallocation")

      graph save "${git}/temp/f-optimization-demand.gph" , replace

  tw (fpfitci correct irt_old ///
      if irt_old > -1 & irt_old < 3 , lc(black) fc(gray) alc(white%0)) ///
    (pcarrow irtCorrect irtKnowledge smeanCorrect smeanKnowledge ///
      ,  ml(country) mlabang(20) lc(red) mc(red) mlabc(black) mlabpos(2)) ///
    , ytit("Vignettes Correct Before and After Reallocation") ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%") ///
      xtit("Average Interaction Competence Before and After Reallocation") ///
      legend(on r(1) pos(7) order(2 "Theoretical" 3 "Actual") ring(0)) ///
      title("Supply-Side Provider Reallocation")

      graph save "${git}/temp/f-optimization-supply.gph" , replace

    graph combine ///
      "${git}/temp/f-optimization-supply.gph" ///
      "${git}/temp/f-optimization-demand.gph" ///
    , ysize(6) c(1) scale(0.7) xcom

    graph export "${git}/output/f-optimization-x.png" , replace


**************************************************
// Figure: Vizualizations for sectoral restriction
**************************************************

// Create optimized allocation images
use "${git}/constructed/capacity-optimized.dta", clear

  binsreg  cap_old irt_old if irt_old > -2 & irt_old < 2 ///
    , polyreg(2) by(country) ysize(8) nbins(10) ///
      xscale(noline) yscale(noline) xtit("Provider Competence") title("Actual") ytit("Daily Outpatient Caseload") ///
      bysymbols(o o o o o o o o o o ) bycolors(blue cranberry cyan dkgreen dkorange emerald gold lavender magenta maroon navy red) ///
      legend(on pos(3) c(5) region(lc(none)) size(small) ///
        order(1 "Kenya" 3 "Madagascar" 5 "Malawi" 7 "Mozambique" 9 "Niger" ///
              11 "Nigeria" 13 "Sierra Leone" 15 "Tanzania" 17 "Togo" 19 "Uganda") )

    graph save "${git}/outputs/temp/f-optimization-1.gph" , replace

  binsreg  cap_hftype irt_hftype if irt_hftype > -2 & irt_hftype < 2 ///
    , polyreg(2) by(country) legend(on pos(3) c(1)) ysize(8) nbins(10) ///
      xscale(noline) yscale(noline) xtit("Provider Competence") title("Reallocated") ytit("Daily Outpatient Caseload") ///
      bysymbols(o o o o o o o o o o )  bycolors(blue cranberry cyan dkgreen dkorange emerald gold lavender magenta maroon navy red)

    graph save "${git}/outputs/temp/f-optimization-2.gph" , replace

  grc1leg ///
    "${git}/outputs/temp/f-optimization-1.gph" ///
    "${git}/outputs/temp/f-optimization-2.gph" , ycom

    graph draw, ysize(6)

    graph export "${git}/output/f-optimization.png" , replace


**************************************************
// Figure: Meta-analytical comparison
**************************************************
use "${git}/constructed/capacity.dta", clear
  egen correct = rowmean(treat?)
  ren irt irt_new
  reg correct irt_new i.country

use "${git}/constructed/optimize-doctors-done.dta" , clear
  keep if f == float(0.95)
  predict correct
  collapse (mean) effect_size = correct (p95) _meta_ciu = correct (p5) _meta_cil = correct , by(country)
  gen region = " 95% Doctoral Training Simulation"
  tempfile docs
    save `docs'

use "${git}/constructed/capacity-comparison.dta" , clear

  keep if x == "Knowledge"
  gen region = "  Average Across Reallocation Simulations"

  egen _meta_ciu = rowmax(irt_public irt_rururb irt_levels irt_hftype irt_unrest)
  egen _meta_cil = rowmin(irt_public irt_rururb irt_levels irt_hftype irt_unrest)

  foreach var in irt smean _meta_ciu _meta_cil  {
    preserve
    use "${git}/constructed/capacity.dta", clear
      egen correct = rowmean(treat?)
      ren irt `var'
      reg correct `var' i.country
    restore
    predict `var'_c
  }
    gen effect_size = smean_c if x == "Knowledge"
    replace _meta_ciu = _meta_ciu_c
    replace _meta_cil = _meta_cil_c

  append using `docs'
    bys country : egen temp = min(irt_c)
    replace irt_c = temp if irt_c == .
    drop temp

  replace _meta_ciu = _meta_ciu - irt_c
  replace _meta_cil = _meta_cil - irt_c
  replace effect_size = effect_size - irt_c

  replace _meta_ciu = _meta_ciu * 100
  replace _meta_cil = _meta_cil * 100
  replace effect_size = effect_size * 100

  keep country region _meta_ciu _meta_cil effect_size

  append using "${git}/constructed/comparison.dta" , gen(sgroup)

  decode country, gen(temp)
    replace CountryName = temp if temp !=""
  meta set effect_size _meta_cil _meta_ciu,  studylabel(CountryName) civartolerance(100)

  replace CountryName = "Tanzania" if strpos(CountryName,"Tanzania")

  replace region = "African RCTs with Comparable (%) Outcomes" if WHO_Region2 == "AFRO"
  replace region = "Americas" if WHO_Region2 == "AMRO"
  replace region = "Eastern Mediterranean" if WHO_Region2 == "EMRO"
  replace region = "South Asia" if WHO_Region2 == "SEARO"
  replace region = "Western Pacific" if WHO_Region2 == "WPRO"
  drop if WHO_Region2 == "EURO"

  replace Outcome_definition = strtrim(Outcome_definition)
  lab var Outcome_definition "Outcome"
  replace Outcome_definition = "Simulation Predicted Increase in General Correct Management" if Outcome_definition == ""

  replace Outcome_definition = "Correct Drug" if studyid == 1000001
  replace Outcome_definition = "Inhaled Corticosteroid" if studyid == 1900001
  replace Outcome_definition = "SPs with Correct Treatment" if studyid == 5100001
  replace Outcome_definition = "Diarrhea Patients Treated by STGs" if studyid == 17190101
  replace Outcome_definition = "Patients Given Correct Chloroquine Dose" if studyid == 17300001
  replace Outcome_definition = "Consultations Where HCP Gives First Dose to Child" if studyid == 27600001
  replace Outcome_definition = "URI Prescriptions with No Drugs" if studyid == 67200001
  replace Outcome_definition = "Patients Managed per Standing Orders" if studyid == 144200001
  replace Outcome_definition = "Injections Appropriate" if studyid == 149300001
  replace Outcome_definition = "Medications With Dose Stated" if studyid == 149600001
  replace Outcome_definition = "Medications With Amodiaquine" if studyid == 205400001
  replace Outcome_definition = "ORS and Zinc for Uncomplicated Diarrhea" if studyid == 258300001
  replace Outcome_definition = "Appropriate Follow-Ups for Injectable Contraceptives" if studyid == 258900001
  replace Outcome_definition = "Patients With HCP Observing First Dose" if studyid == 261500001
  replace Outcome_definition = "Correctly Treated Malaria" if studyid == 274300001
  replace Outcome_definition = "Malaria-Negative Patients With Artemether-Lumefantrine" if studyid == 275200001
  replace Outcome_definition = "Carer Reported Correct Child Care for Fever/Diarrhea/Pneumonia" if studyid == 279400001
  replace Outcome_definition = "Non-Menstruating Patients Denied Contraceptives" if studyid == 279500001
  replace Outcome_definition = "HIV-Positive Pregnant Women Receiving ARVs" if studyid == 286500001
  replace Outcome_definition = "SPs with Correct Artemether-Lumefantrine Treatment" if studyid == 287400001
  replace Outcome_definition = "Patients Prescribed Co-trimoxazole Prophylaxis" if studyid == 289600001
  replace Outcome_definition = "Patients Prescribed Antimalarials" if studyid == 291100001
  replace Outcome_definition = "Patients Prescribed Antibiotics or Antimalarials" if studyid == 296600001
  replace Outcome_definition = "Prescriptions with Injection" if studyid == 298000001

  meta forest _id Outcome_definition _esci _plot if effect_size < 50 ///
    & (region == "  Average Across Reallocation Simulations" | region == " 95% Doctoral Training Simulation") ///
  , subgroup(region) sort(CountryName) ///
    nowmark noghet nogwhomt noohomtest noohetstats nullrefline ///
    bodyopts(size(large)) mark(msize(small) mcolor(black) msymbol(O) ) ///
    ciopts(lc(gs12) mstyle(none)) nooverall

   graph export "${git}/output/f-lit-review.png" , replace

  meta forest _id Outcome_definition _esci _plot if effect_size < 50 ///
    & !(region == "  Average Across Reallocation Simulations" | region == " 95% Doctoral Training Simulation" | region == "Africa") ///
  , subgroup(region) sort(effect_size) ///
    nowmark noghet nogwhomt noohomtest noohetstats nullrefline ///
    bodyopts(size(small)) mark(msize(small) mcolor(black) msymbol(O) ) ///
    ciopts(lc(gs12) mstyle(none)) nooverall

    graph export "${git}/appendix/f-lit-2.png" , replace

  // Save for comparison

  gen check = 1
  replace effect_size=effect_size*100
  save "${git}/constructed/optimize-comparison.dta" , replace

// End
