// Set global path locations

ssc install repkit, replace

  cap ssc install repkit
    reproot, project("sdi-caseloads") roots("box git") clear
    repado using "${git}/ado"

// Set up schemes and user-installed packages

  copy "https://github.com/graykimbrough/uncluttered-stata-graphs/raw/master/schemes/scheme-uncluttered.scheme" ///
    "${git}/ado/scheme-uncluttered.scheme" , replace

    cd "${git}/ado/"
    set scheme uncluttered , perm
    graph set eps fontface "Helvetica"

  cap ssc install iefieldkit , replace
  cap ssc install cdfplot , replace
  cap net install grc1leg , from("http://www.stata.com/users/vwiggins/") replace
  cap net install binsreg , from("https://raw.githubusercontent.com/nppackages/binsreg/master/stata")
  cap net install st0085_2 , from("http://www.stata-journal.com/software/sj14-2")
  cap net install winsor2 , from("http://fmwww.bc.edu/RePEc/bocode/w")

  net from "https://github.com/bbdaniels/stata/raw/main/"
    cap net install outwrite

// Globals

  // Options for -twoway- graphs
  global tw_opts ///
    title(, justification(left) color(black) span pos(11)) ///
    graphregion(color(white) lc(white) lw(med)) bgcolor(white) ///
    ylab(,angle(0) nogrid) xtit(,placement(left) justification(left)) ///
    yscale(noline) xscale(noline) legend(region(lc(none) fc(none)))

  // Options for -graph- graphs
  global graph_opts ///
    title(, justification(left) color(black) span pos(11)) ///
    graphregion(color(white) lc(white) lw(med)) bgcolor(white) ///
    ylab(,angle(0) nogrid) ytit(,placement(left) justification(left))  ///
    yscale(noline) legend(region(lc(none) fc(none)))

  // Options for histograms
  global hist_opts ///
    ylab(, angle(0) axis(2)) yscale(off alt axis(2)) ///
    ytit(, axis(2)) ytit(, axis(1))  yscale(alt)

  // Useful stuff
  global pct `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
  global numbering `""(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)""'
  global bar lc(white) lw(thin) la(center) fi(100)

// Copy in raw data -- comment out in final package

  iecodebook export "${box}/comparison.dta" ///
    using "${git}/data/comparison.xlsx" ///
    , replace save sign reset

  iecodebook export "${box}/SDI_Nigeria-2013_Module1_Raw.dta" ///
    using "${git}/data/nigeria.xlsx" ///
    , replace save sign reset

  iecodebook export "${box}/SDI_Tanzania-2016_Module1_Raw.dta" ///
    using "${git}/data/tanzania.xlsx" ///
    , replace save sign reset

  iecodebook export "${box}/SDI_Uganda-2013_Module1_Raw.dta" ///
    using "${git}/data/uganda.xlsx" ///
    , replace save sign reset

  iecodebook export "${box}/poverty_rates.dta" ///
    using "${git}/data/poverty_rates.xlsx" ///
    , replace save sign reset

  copy "${box}/All_countries_harm.dta" ///
     "${git}/data/vignettes.dta" ///
    , replace

  iecodebook export "${box}/All_countries_pl.dta" ///
    using "${git}/data/vignettes-provider.xlsx" ///
    , replace save sign reset

  iecodebook export "${box}/IRT_parameters.dta" ///
    using "${git}/data/irt-parameters.xlsx" ///
    , replace save sign reset

  copy "${box}/provider-codebook.xlsx" ///
    "${git}/data/provider-codebook.xlsx" , replace

// Queueing simulation program

cap prog drop q_up
prog def q_up , rclass

args periods patients duration

  local p_patient = `patients'/`periods'
  local p_resolve = 1/`duration'

  clear
  set obs 1
  gen period = 0
  gen service = . // to add more servers later
  gen q1 = .

  qui forv i = 1/`periods' {

    expand 2 in 1
    gsort -period
    replace period = `i' in 1
    local shift = 0

    local pos ""
    local q 1
    // Increment wait for all patients in queue
    foreach var of varlist q* {
      if "`=`var'[1]'" == "." & "`pos'" == "" {
        local pos = `q'
      }
      local ++q
      replace `var' = `var' + 1 in 1 if `var' != .
    }
    if "`pos'" == "" local pos 1

    // Spawn new patient at end of queue
    gen r = runiform()
    if `=r[1]' < `p_patient' {
      local total_pats = `total_pats' + 1

      if "`=q`pos'[1]'" == "." {
        replace q`pos' = 0 in 1
      }
      else gen q`q' = 0 in 1
    }
    drop r

    // Clear service
    gen r = runiform()
    if `=r[1]' < `p_resolve' {
      replace service = . in 1
    }
    drop r

    // Advance patients if service is open
    if "`=service[1]'" == "." & "`=q1[1]'" != "." {
      replace service = q1 in 1
      local total_wait = `total_wait' + `=service[1]'
      local shift = 1
    }

    local count = 1
    qui if `shift' == 1 qui foreach var of varlist q* {
      local ++count
      if "`=q`count'[1]'" != "" {
        replace `var' = `=q`count'[1]' in 1
      }
      else replace `var' = . in 1
    }

    }

    // Calculate statistics
    return scalar total_wait = `total_wait'
    return scalar total_pats = `total_pats'
    return scalar mean_wait  = `total_wait'/`total_pats'

    qui count if service != .
    return scalar total_work = `r(N)'
    return scalar work_time  = `r(N)'/`periods'
    return scalar idle_time  = 1 - `r(N)'/`periods'

end

// Run code past here


// End of runfile
