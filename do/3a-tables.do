
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

//
