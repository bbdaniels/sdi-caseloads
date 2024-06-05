// Summary table: Country-facility crowding
use "${git}/constructed/capacity-fac.dta" , clear

 drop if cap == . | hf_type == .

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

// Table: Regressions
  use "${git}/constructed/capacity-fac.dta" , clear

    bys country: gen weight = 1/_N
    reg cap          b3.hf_type  [pweight=weight] , a(country)
      est sto reg11
    reg cap hf_provs b3.hf_type [pweight=weight] , a(country)
      est sto reg21
    reg cap_prov hf_provs b3.hf_type  [pweight=weight] , a(country)
      est sto reg31

  use "${git}/constructed/capacity.dta" , clear
  bys country: gen weight = 1/_N
     drop if provider_mededuc1 == 1

    egen vig = rowmean(treat?)
    reg vig c.irt##i.country##i.hf_type
    drop vig
      predict vig
      replace vig = 0 if vig < 0
      replace vig = 100*vig

    reg cap b4.cadre i.provider_mededuc1 [pweight=weight] , a(country)
    est sto reg1

    reg cap hf_provs b3.hf_type b4.cadre  i.provider_mededuc1 [pweight=weight] , a(country)
    est sto reg2

    reg cap vig hf_provs b3.hf_type b4.cadre  i.provider_mededuc1 [pweight=weight] , a(country)
    est sto reg3

  outwrite reg3 reg2 reg1 reg31 reg21 reg11 ///
    using "${git}/outputs/main/t-provs-capacity.xlsx" ///
  , replace stats(N r2) ///
    colnames("General" "Providers per Facility" "Competence" "Providers per Facility" "General")


-- // NEW EOF


// Tables of comparative statistics
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

  collapse ///
    (p25) cap25 = cap vig25 = vig ///
    (p75) cap75 = cap vig75 = vig ///
    (mean) vig vig_hftype ///
    [aweight = cap] , by(country)

    gen gain = vig_hftype - vig
    gen g2 = gain / vig

  export excel ///
    country cap25 cap75 vig25 vig75 vig vig_hftype gain g2 ///
    using "${git}/outputs/main/t-optimize-quality.xlsx" ///
  , replace first(var)


//
