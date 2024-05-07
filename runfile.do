// Set global path locations
  
ssc install repkit, replace

  cap ssc install repkit
    repado using "${git}/ado"
    reproot, project("projectname") roots("box git")

// Set up schemes and user-installed packages

  copy "https://github.com/graykimbrough/uncluttered-stata-graphs/raw/master/schemes/scheme-uncluttered.scheme" ///
    "${git}/ado/scheme-uncluttered.scheme" , replace

    cd "${git}/ado/"
    set scheme uncluttered , perm
    graph set eps fontface "Helvetica"

  cap ssc install iefieldkit

  net from "https://github.com/bbdaniels/stata/raw/main/"

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

// Run code past here


// End of runfile
