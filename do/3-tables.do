// Summary table: Country-facility crowding
use "${git}/constructed/capacity-fac.dta" , clear

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

  tabstat cap_prov [aweight=hf_provs], by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  tabstat cap_prov [aweight=hf_provs], by(country) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  tabstat cap_prov [aweight=hf_provs*cap_prov], by(country) s(med) not save
    mat a = [r(Stat1) , r(Stat2) , r(Stat3) , r(Stat4) , r(Stat5) , r(Stat6) , r(Stat7) , r(Stat8) , r(Stat9) , r(Stat10)]'
    mat results = results, a

  matlist results
  mat results_STARS = J(rowsof(results),colsof(results),0)

  outwrite results ///
    using "${git}/outputs/main/t-summary-capacity.xlsx" ///
  , replace ///
    rownames("Kenya" "Madagascar" "Malawi" "Mozambique" "Niger" ///
             "Nigeria" "Sierra Leone" "Tanzania" "Togo" "Uganda") ///
    colnames("Facilities (N)" "Providers per Facility" "Providers Present" ///
             "Patients per Facility Day"  ///
             "Patients per Provider Day" "Mean Provider" "Median Patient")

// Table: Regressions
  use "${git}/constructed/capacity-fac.dta" , clear

    replace hf_type = 0 if hf_type == 6

    bys country: gen weight = 1/_N
    reg cap          b0.hf_type  [pweight=weight] , a(country)
      est sto reg11
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg11
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg11

    reg cap b0.hf_type hf_provs [pweight=weight] , a(country)
      est sto reg21
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg21
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg21


    areg cap_prov hf_provs b0.hf_type [pweight=weight] , a(country)
    est sto reg1
    test 1.hf_type - 4.hf_type = 0
      estadd scalar hos = `r(p)' : reg1
    test 2.hf_type - 5.hf_type = 0
      estadd scalar cli = `r(p)' : reg1


  use "${git}/constructed/capacity.dta" , clear
  drop if provider_mededuc1 == 1

  qui areg cap  hf_provs b0.hf_type b0.cadre  i.provider_mededuc1  , a(country)
    drop if !e(sample)

  gen ratio = hf_provs/hf_provs_vig
  bys country: egen sum = sum(ratio)
  bys country: gen weight = ratio/sum

    egen vig = rowmean(treat?)
    reg vig c.irt##i.country##i.hf_type
    drop vig
      predict vig
      replace vig = 0 if vig < 0
      replace vig = 100*vig

    replace hf_type = 0 if hf_type == 6
    replace cadre = 0 if cadre == 3
    egen fuid = group(country fid)

    areg cap  b0.hf_type [pweight=weight] , a(country) cl(fuid)
    est sto reg2
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg2
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg2

    areg cap  b0.hf_type b0.cadre  i.provider_mededuc1 [pweight=weight] , a(country) cl(fuid)
    est sto reg3
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg3
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg3

    areg cap vig hf_provs b0.hf_type b0.cadre  i.provider_mededuc1 [pweight=weight] , a(country) cl(fuid)
    est sto reg4
      test 1.hf_type - 4.hf_type = 0
        estadd scalar hos = `r(p)' : reg4
      test 2.hf_type - 5.hf_type = 0
        estadd scalar cli = `r(p)' : reg4

  outwrite reg4 reg11 reg21 reg1 reg2 reg3 reg4     ///
    using "${git}/outputs/main/t-regs-capacity.xlsx" ///
  , replace stats(N r2 hos cli) ///
    colnames("X" "1" "2" "3" "4" "5" "6")


// Tables of comparative statistics
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

    expand 2 , gen(fake)
    replace hf_rural = 2 if fake == 1

  collapse ///
    (p25) cap25 = cap vig25 = vig ///
    (p75) cap75 = cap vig75 = vig ///
    (mean) vig vig_hftype fake ///
    [aweight = cap*hf_provs/hf_provs_vig] , by(country hf_rural)

    gen gain = vig_hftype - vig
    gen g2 = gain / vig

    gen caprat = cap75/cap25
    gen vigdif = vig75-vig25

  sort fake hf_rural country

  export excel ///
    country hf_rural vig vig_hftype gain g2 cap25 cap75 caprat vig25 vig75 vigdif  ///
    using "${git}/outputs/main/t-optimize-quality.xlsx" ///
  , replace first(var)


//
