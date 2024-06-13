use "${git}/data/nigeria.dta" , clear
  keep fac_id m1saq7 m1sbq8
  gen country = "NIGERIA"
  ren (fac_id m1saq7 m1sbq8)(facility_id rural facility_level)
    recode rural (2=0)
  tempfile nigeria
    save `nigeria'

use "${git}/data/tanzania.dta" , clear
  keep fac_id m1saq7 m1sbq8
  gen country = "TANZANIA"
  ren (fac_id m1saq7 m1sbq8)(facility_id rural facility_level)
    recode rural (2=0)
  tempfile tanzania
    save `tanzania'

use "${git}/data/uganda.dta" , clear
  keep fac_id h1csqb9  h1sa9
  gen country = "UGANDA"
  ren (fac_id h1csqb9  h1sa9)(facility_id rural facility_level)
    recode rural (2=0)
  tempfile uganda
    save `uganda'


  *Open harmonized dataset
  use  "${git}/data/vignettes.dta", clear

    merge m:1 country facility_id using `nigeria' , update replace nogen keep(1 3 4 5)
    merge m:1 country facility_id using `tanzania' , update replace nogen keep(1 3 4 5)
    merge m:1 country facility_id using `uganda' , update replace nogen keep(1 3 4 5)

  drop if rural == 3

  drop fac_type
  egen fac_type = group(rural facility_level) , label

  *Isolate the variables in which the dataset is unique
  keep  cy country year facility_id provider_id unique_id  /// unique identifiers
      admin1_name admin2_name med_frac num_med     /// variales being added
      num_staff caseload skip_* *_history_* *_exam_*  ///
      diag* *_test_* diag* treat* num_skipped      ///
      public *_antibio fac_type ipw ///
      /// Caseload variables
      fac_is_out mean_abs num_med num_med_pres ///
      num_outpatient num_inpatient num_inpatient_beds

  order  country year facility_id
  sort   country year facility_id provider_id unique_id

  *Apply lables to isolated variables
  label var  country "Name of country"
  label var   year   "Year survey was conducted"

  // Drop small samples
  drop if cy == "GNB_2018" | cy == "TZN_2014"

  *Check if the dataset is unique at provider level
  isid country year facility_id provider_id // dataset is unique at provider id level

  *Drop provider id variable
  // drop facility_id             // this variable is no longer needed
  sort country year provider_id unique_id  // sort dataset

/*************************************
    Merge variables
**************************************/

  *Merge varibles needed to provider level dataset
  merge 1:1 country year unique_id using "${git}/data/vignettes-provider.dta"

  *Check that there are no unmatched observations
  assert  _merge!= 1
  drop   _merge     // variable is no longer needed

/*********************************************************
  Merge povery rates variables to provider level dataset
**********************************************************/

  gen    admin1_name_temp  = admin1_name
  replace  admin1_name_temp  = admin2_name    if country == "SIERRALEONE"
  replace  admin1_name_temp  = admin2_name    if country == "MALAWI"
  replace  admin1_name_temp  = admin2_name    if cy == "KEN_2012"
  replace admin1_name_temp  = "Nairobi City"  if cy == "KEN_2012"
  replace admin1_name_temp  = "Golfe/Lome"    if admin1 == 1 & country == "TOGO"
  replace admin1_name_temp  = "Western"     if admin1_name_temp == "Western Rural" | admin1_name_temp == "Western Urban"

  sort   country admin1_name_temp
  merge m:1 country admin1_name_temp using "${box}/poverty_rates.dta"
  drop if _merge ==2  // drop unmatched poverty rates from using dataset
  drop   _merge     // _merge no longer needed

  *Drop admin variables no longer needed
  drop admin1_name_temp

/*****************************************************
  Merge in IRT estimates to provider level dataset
*******************************************************/

  *Create a unique_id that includes country year needed for merge
  sort   cy unique_id
  gen   unique_id2 = cy + "_" + unique_id

  sort   unique_id2
  merge   1:1 unique_id2 using "${git}/data/irt-parameters.dta", keepusing(theta_mle)
  drop   if _merge != 3     // drop providers that did not make the final merged module dataset
  drop   _merge unique_id2   // these variables are not needed anymore

/***************************************************
 Construct variables Needed for Analysis
***************************************************/

  *Rename countries to removed capitalized letters
  replace country = "Kenya"      if country == "KENYA"
  replace country = "Madagascar"    if country == "MADAGASCAR"
  replace country = "Mozambique"     if country == "MOZAMBIQUE"
  replace country = "Niger"       if country == "NIGER"
  replace country = "Nigeria"     if country == "NIGERIA"
  replace country = "Sierra Leone"  if country == "SIERRALEONE"
  replace country = "Tanzania"     if country == "TANZANIA"
  replace country = "Togo"       if country == "TOGO"
  replace country = "Uganda"       if country == "UGANDA"
  replace country = "Guinea Bissau"  if country == "GUINEABISSAU"
  replace country = "Malawi"       if country == "MALAWI"
  replace country = "Cameroon" if country == "CAMEROON"


  *Create rural/urban indicator
  gen      rural = 0
  replace   rural = 1 if fac_type == 4 | fac_type == 5 | fac_type == 6
  lab define  rur_lab 1 "Rural" 0 "Urban"
  label val   rural rur_lab
  label var   rural "Facility region"

  *Create a facility variable and recode it
  gen     facility_level_rec = 1 if facility_level == 3
  replace   facility_level_rec = 2 if facility_level == 1
  replace   facility_level_rec = 3 if facility_level == 2
  lab define  facility_level_lab 1 "Health Post" 2 "Hospital" 3 "Health Center"
  label val   facility_level_rec facility_level_lab

  *Create age group
  gen      age_gr = 1 if provider_age1>= 10 & provider_age1<20
  replace    age_gr = 2 if provider_age1>= 20 & provider_age1<30
  replace    age_gr = 3 if provider_age1>= 30 & provider_age1<40
  replace    age_gr = 4 if provider_age1>= 40 & provider_age1<50
  replace    age_gr = 5 if provider_age1>= 50 & provider_age1<60
  replace    age_gr = 6 if provider_age1>= 60 & provider_age1<70
  replace    age_gr = 7 if provider_age1>= 70 & provider_age1<80
  replace    age_gr = 8 if provider_age1>= 80 & provider_age1<90
  label var   age_gr "Age Grouping"
  lab define  age_lab 1 "Age 10-20" 2 "Age 20-30" 3 "Age 30-40" 4 "Age 40-50"  ///
            5 "Age 50-60" 6 "Age 60-70" 7 "Age 70-80" 8 "Age 80-90"
  label val   age_gr age_lab

  *Create a variable for male and female provides
  gen    gen_male      = 1  if provider_male1 == 1
  gen    gen_female      = 1 if provider_male1 == 0
  gen    gen_male_per     = gen_male
  gen    gen_female_per     = gen_female
  gen   provider_cadre1_per = provider_cadre1

  *Create an all doctors variable
  gen   doctors = 0
  replace doctors = 1 if provider_cadre1 == 1
  by     country year facility_id, sort: egen tot_doctors = total(doctors)
  lab var tot_doctors "Total number of dcotors"

  *Create a variable for all female doctors
  gen   doctors_fem = 0
  replace doctors_fem = 1 if provider_cadre1 == 1 & provider_male1 == 0
  by     country year facility_id, sort: egen tot_doctors_fem = total(doctors_fem)
  lab var tot_doctors_fem "Total number of female doctors"

  *Create share of doctors per all clinical staff
  gen   med_fem_frac = tot_doctors_fem/tot_doctors
  lab var med_fem_frac "Share of female doctors"

  *Create a variable for facility has a doctor
  gen   fac_hasdoctors = 1 if tot_doctors != 0

  *Create a variable for proportion of doctors under the age of 35
  gen   doctor_35 = 0
  replace doctor_35 = 1 if provider_cadre1 == 1 & (provider_age1 < 35 & !missing(provider_age1))
  by     country year facility_id, sort: egen tot_doctors_35 = total(doctor_35)

  gen   doctors_age = 0
  replace doctors_age = 1 if provider_cadre1 == 1 & !missing(provider_age1)
  by     country year facility_id, sort: egen tot_doctors_age = total(doctors_age)

  *Create a variable for all nurses or "other"
  gen    nurse_other = 0
  replace nurse_other = 1 if provider_cadre1 == 3 | provider_cadre1 == 4
  by     cy facility_id, sort: egen tot_nurseother = total(nurse_other)
  gen   fac_hasnurseother = 1 if tot_nurseother != 0

  *Clean up total staff and total medical staff variables
  recode  num_med num_staff (0 .a = .)
  gen   all_med = 0
  replace  all_med = 1 if provider_cadre1 == 1 | provider_cadre1 == 3 | provider_cadre1 == 4
  by     cy facility_id, sort: egen tot_all_med = total(all_med)
  replace num_med   = tot_all_med  if num_med   == .
  replace num_staff   = tot_all_med  if num_staff == .
  drop   all_med tot_all_med // these variables are no longer needed

  *Create share of medical officers per all clinical staff
  gen   med_cli_frac = tot_doctors/num_med

  *Needed to match the name of the admin area mentioned in the GDP data receieved from the World Bank's poverty team
  replace  admin1_name  = admin2_name    if country == "Sierra Leone"
  replace  admin1_name  = admin2_name    if country == "Malawi"

  *Recode variables
  recode public (6/7=0)

  *Encode country year variable
  encode  country, gen(country_coded)

  *Order the variables
  sort  country year admin1_name unique_id
  order  country year admin1_name provider_id unique_id

  *Drop Kenya 2012 from the sample
  drop if country == "Kenya" & year == 2012

  *Drop Cameroon from the sample
  drop if country == "Cameroon"

  // Remove inaccurate private obs
  drop if public == 0 & (country == "Guinea Bissau" | country == "Mozambique")

  *Save final dataset with new variables added
  save "${box}/Final_pl.dta", replace

  /*****************************************************
    Create a vignettes only dataset
  *******************************************************/

  *Drop observations that skipped all vignettes
  drop if num_skipped == 8 | num_skipped == .

  *Drop eclampsia since only Niger did this moddule
  drop   eclampsia_* skip_eclampsia

  *Drop unwanted variable
  drop  d_* sh_* total_d_* diag1_alt    ///
      diag1_simp diag5_simp treat1_alt   ///
      treat1_alt2 treat2pne treat2fev    ///
      gpslat_all gpslong_all

  // Magic
  iecodebook apply using "${git}/data/provider-codebook.xlsx" , drop

  // Recode occupation by education
  replace cadre = 1 if cadre == 4 & inlist(provider_mededuc1,3,4)

  // Staff count
  egen ftag = tag(country year hf_id)
    lab var ftag "Facility Tag"
  bys country year hf_id : gen hf_provs = _N
    lab var hf_provs "Observed Providers"

    gen check = hf_provs/hf_staff_op if hf_staff_op < 11 & ftag == 1
     su check if hf_staff_op > 1
     gen temp = hf_staff_op*`r(mean)'
     ren hf_provs hf_provs_vig
     clonevar hf_provs = hf_provs_vig
       replace hf_provs = temp if hf_staff_op > 10
       drop temp check

  *Order the variables
  isid country year hf_id prov_id, sort
  egen uid = group(country year hf_id prov_id)
    lab var uid "Provider ID"
  egen fid = group(country year hf_id)
    lab var fid "Facility ID"
  order country year fid uid hf_id prov_id , first

  drop if hf_outpatient == 0 | missing(hf_outpatient)

  *  Create facility level dataset
  preserve
  labelcollapse ///
    (firstnm) hf_provs country year hf_staff_op hf_outpatient hf_type ///
    , by(fid) vallab(country hf_type)

    gen cap = hf_outpatient/(60)
      lab var cap "Outpatients per Facility Day"

      clonevar cap_raw = cap
      winsor2 cap , cuts(0 97.5) replace by(country)

    gen cap_prov = hf_outpatient/(60*hf_provs)
      lab var cap_prov "Outpatients per Provider Day"

      clonevar cap_prov_raw = cap_prov
      winsor2 cap_prov , cuts(0 97.5) replace by(country)

    save "${git}/constructed/capacity-fac.dta", replace
    use "${git}/constructed/capacity-fac.dta", clear

      drop cap cap_raw
      ren (cap_prov cap_prov_raw)(cap cap_raw)

      lab var cap "Outpatients per Provider Day"
      lab var cap_raw "Outpatients per Provider Day (Unwinsorized)"

      tempfile fac
        save `fac'
  restore

  *  Back to providers
  merge m:1 fid using `fac' , nogen

  save "${git}/constructed/capacity.dta", replace
  use "${git}/constructed/capacity.dta", clear

************************ End of do-file *****************************************
