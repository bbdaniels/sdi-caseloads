**************************************************
// Part 1: Capacity optimization methods
**************************************************

// Calculate new capacity per day at each provider based on resorting
use "${git}/constructed/capacity.dta" if hf_provs <10, clear
  drop if cap == .

  egen vig = rowmean(treat?)
  reg vig c.irt##i.country##i.hf_type
  drop vig
    predict vig

  tempfile irt all
  keep country irt cap hf_type hf_level hf_rural public cadre ///
    hf_staff_op hf_outpatient hf_inpatient treat? vig
  save `all'

  // Restricted to type resort
  preserve
  gsort country hf_type -irt
    keep country hf_type irt vig
    ren vig vig_hftype
    ren irt irt_hftype
    gen ser_hftype = _n
    save `irt' , replace
  restore

  gsort country hf_type -cap
    gen cap_hftype = cap
    gen ser_hftype = _n
    merge 1:1 ser_hftype using `irt' , nogen

  // Restricted to cadre resort
  preserve
  gsort country cadre -irt
    keep country cadre irt vig
    ren vig vig_cadres
    ren irt irt_cadres
    gen ser_cadres = _n
    save `irt' , replace
  restore

  gsort country cadre -cap
    gen cap_cadres = cap
    gen ser_cadres = _n
    merge 1:1 ser_cadres using `irt' , nogen

  // Unrestricted resort
  preserve
  gsort country -irt
    keep country irt cap vig
    ren vig vig_unrest
    ren irt irt_unrest
    gen ser_unrest = _n
    save `irt' , replace
  restore

  gsort country -cap
    gen cap_unrest = cap
    gen ser_unrest = _n
    merge 1:1 ser_unrest using `irt' , nogen

  // Restricted to level resort
  preserve
  gsort country hf_level -irt
    keep country hf_level irt cap  vig
    ren vig vig_levels
    ren irt irt_levels
    gen ser_levels = _n
    save `irt' , replace
  restore

  gsort country hf_level -cap
    gen cap_levels = cap
    gen ser_levels = _n
    merge 1:1 ser_levels using `irt' , nogen

  // Restricted to zone resort
  preserve
  gsort country hf_rural -irt
    keep country hf_rural irt cap vig
    ren vig vig_rururb
    ren irt irt_rururb
    gen ser_rururb = _n
    save `irt' , replace
  restore

  gsort country hf_rural -cap
    gen cap_rururb = cap
    gen ser_rururb = _n
    merge 1:1 ser_rururb using `irt' , nogen

  // Restricted to sector resort
  preserve
  gsort country public -irt
    keep country public irt cap vig
    ren vig vig_public
    ren irt irt_public
    gen ser_public = _n
    save `irt' , replace
  restore

  gsort country public -cap
    gen cap_public = cap
    gen ser_public = _n
    merge 1:1 ser_public using `irt' , nogen

  ren (irt cap vig) (irt_old cap_old vig_old)
    egen c = rowmean(treat?)
    reg c c.irt_old##i.country

  save "${git}/constructed/capacity-optimized.dta" , replace

// Prepare dataset for comparative statistics
use "${git}/constructed/capacity-optimized.dta" , clear
  tempfile all

  preserve
    collapse irt_old vig_old [aweight=cap_old] , by(country)
      ren vig_old irt_xxx
      reshape long irt , i(country) j(x) string
    save `all' , replace
  restore

  qui foreach type in ///
    unrest hftype levels rururb public cadres {
    preserve
      collapse irt_`type' vig_`type' [aweight=cap_`type'] , by(country)
        ren vig_`type' irt_xxx
        reshape long irt , i(country) j(x) string
        ren irt irt_`type'
        replace x = "_old" if x != "_xxx"
      merge 1:1 country x using `all' , nogen
      save `all' , replace
    restore
  }

  use `all' , clear
    egen mean = rowmean(irt_*)
    egen smean = rowmean(irt_cadres irt_public irt_rururb irt_levels irt_hftype irt_unrest)
    gen sdif = smean - irt

  sort x country
    replace x = "Knowledge" if x == "_old"
    replace x = "Correct" if x == "_xxx"
    gen loss = (irt_hftype-irt) / irt_hftype if x == "Correct"
    gen gain = (irt_hftype-irt) / irt if x == "Correct"


  save "${git}/constructed/capacity-comparison.dta" , replace
  use "${git}/constructed/capacity-comparison.dta" , replace

-
**************************************************
// Part 4: Doctor resampling
**************************************************

clear
tempfile all
  save `all' , emptyok
use "${git}/constructed/capacity-optimized.dta" , clear
gen uid = _n

gen doctor = (cadre == 1)
keep doctor irt_old country cap_old uid

levelsof country, local(cs)

foreach c in `cs' {

  preserve
  keep if doctor == 1 & country == `c'
  clonevar irt_doc = irt_old
  tempfile doctors
  save `doctors'
  restore

  preserve
  keep if doctor == 0 & country == `c'
  cross using `doctors'

  append using `all'
  append using `doctors'
  save `all' , replace
  restore

}
use `all' , clear
save "${git}/constructed/optimize-doctors-basis.dta" , replace

cap prog drop upskill
prog def upskill

args frac

    use "${git}/constructed/optimize-doctors-basis.dta" , clear

    gen r = runiform()
      replace r = 0 if doctor == 1
    bys uid (r) : gen v = _n
     keep if v == 1

    bys country: egen rank = rank(r)
    gsort country -doctor rank
      bys country: gen N = _N
      replace rank = rank/N

    gen irt_new = irt_old
      replace irt_new = irt_doc if rank < `frac' & irt_doc > irt_new

    collapse irt_new [aweight=cap_old] , by(country)
    gen f = `frac'

end

clear
tempfile all
  save `all' , replace emptyok

  set seed 943750
  qui forv f = 0(0.05)1 {
    forv it = 1/50 {
      upskill `f'
      append using `all'
      save `all' , replace
    }
  }
  save "${git}/constructed/optimize-doctors-done.dta" , replace

//
