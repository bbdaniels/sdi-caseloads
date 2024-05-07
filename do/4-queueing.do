// Queueing simulation

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

       graph save "${git}/temp/queue-`x'.gph" , replace
       local ++x
}

grc1leg ///
"${git}/temp/queue-1.gph" ///
"${git}/temp/queue-2.gph" ///
"${git}/temp/queue-3.gph" ///
"${git}/temp/queue-4.gph" ///
 , altshrink

 graph draw, ysize(6)
 graph export "${git}/appendix/queue-1.png" , width(3000) replace

local x = 1
set seed 123396
foreach pats in 15 20 30 40 {
 qui q_up 360 `pats' 10
   return list
     local idle : di %3.2f `r(idle_time)'
     local wait : di %3.1f `r(mean_wait)'
     local pati = `r(total_pats)'

   egen check = rownonmiss(q*)
   replace period = period/60
   gen zero = 0
   tw (rarea check zero  period , lc(white%0) fc(gray) connect(stairstep))(line check period , lc(black) connect(stairstep))(scatter check period if service == . , mc(red) m(.)) ///
     , ytit("Patients in Queue") yscale(r(0)) ylab(#6) ///
       xtit("Patients/Day: `pats' | Idle Share: `idle' | Mean Wait: `wait' Min.") xlab(0 "Hours {&rarr}" 1 2 3 4 5 6 "Close") xoverhang ///
       legend(on order(3 "No Patients" 2 "Serving Patients" 1 "Patients Waiting") r(1)  pos(12) ring(1) symxsize(small))

      graph save "${git}/temp/queue-`x'.gph" , replace
      local ++x
}

grc1leg ///
"${git}/temp/queue-1.gph" ///
"${git}/temp/queue-2.gph" ///
"${git}/temp/queue-3.gph" ///
"${git}/temp/queue-4.gph" ///
, altshrink

graph draw, ysize(6)
graph export "${git}/appendix/queue-2.png" , width(3000) replace


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

  replace wait = 1 if wait < 1
  tw (scatter idle wait if pats == 15 , mc(black)) ///
     (scatter idle wait if pats == 20 , mc(red) m(t)) ///
     (scatter idle wait if pats == 30 , mc(blue) m(S)) ///
     (scatter idle wait if pats == 40 , mc(green) m(D)) ///
  , legend(on pos(2) c(1) ring(0) ///
    order(1 "15 Patients/Day" 2 "20 Patients/Day" 3 "30 Patients/Day" 4 "40 Patients/Day")) ///
    xtit("Mean Waiting Time for Serviced Patients (Minutes)") xscale(log) ///
    xlab(1 "No Wait" 2.5 5 10 20 40 80) ///
    ytit("Idle Time for Provider") ylab(1 "100%" .75 "75%" .5 "50%" .25 "25%" 0 "0%")

    graph export "${git}/appendix/queue-3.png" , width(3000) replace

// End
