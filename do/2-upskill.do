**************************************************
// Doctor resampling
**************************************************

clear
tempfile all
  save `all' , emptyok
use "${git}/constructed/capacity-optimized.dta" , clear
gen uid = _n

gen doctor = (cadre == 1)
keep doctor vig_old country cap_old uid

levelsof country, local(cs)

foreach c in `cs' {

  preserve
  keep if doctor == 1 & country == `c'
  clonevar vig_doc = vig_old
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

    gen vig_new = vig_old
      replace vig_new = vig_doc if rank < `frac' & vig_doc > vig_new

    collapse vig_new [aweight=cap_old] , by(country)
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
