********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global outputs		"$root\2.Analysis\3.Outputs\Tables"
********************************************************************************************************************

use "$inputs\rounds_all.dta" , clear
	
	global outcomes ppvt_stdscre_r2 cda_r2 ppvt_stdscre_r3 egra_r3 math_r3 agency_r3 ppvt_stdscre_r4 lang_r4 math_r4  agency_r4  sef_r4  ses_pride_r4 ppvt_stdscre_r5 reading_r5 ///
					math_r5  agency_r5 sef_r5  ses_pride_r5
								
	global cov_hh  	urban bad_event 
	
	global cov_cg   i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
					moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
					beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"

	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)
************************************************************************************************
**II. IMPACT: OUTCOMES**************************************************************************
************************************************************************************************
*Standardising outcomes: 
foreach var in $outcomes{
	gen `var'_copy=`var'
	sum `var' if treatment==0 & filter_final==0
	replace `var'=(`var'_copy-r(mean))/r(sd)
	}

*Calculating weights to identify obs outside common support:
kmatch eb treatment $covariates if filter_final==0,  comsup att wgen(w_eb) targets(2)
		
*Calculating ATT:
foreach var in $outcomes{
	eststo ols_`var': reg `var' treatment if filter_final==0 & w_eb!=0, vce(cluster clustid_r1)
	eststo eb_`var':  kmatch eb treatment $covariates (`var') if filter_final==0, comsup targets(2) att vce(cluster clustid_r1)
	eststo ipw_`var': kmatch ps treatment $covariates  (`var') if filter_final==0, kernel(epan) bwidth(cv) comsup att vce(cluster clustid_r1)
	}
	
	*************************
	**TABLE 5 ******
	*************************		
	global r2_cog ols_ppvt_stdscre_r2 eb_ppvt_stdscre_r2 ipw_ppvt_stdscre_r2 ols_cda_r2  	eb_cda_r2     ipw_cda_r2   
	global r3_cog ols_ppvt_stdscre_r3 eb_ppvt_stdscre_r3 ipw_ppvt_stdscre_r3 ols_egra_r3 	eb_egra_r3 	  ipw_egra_r3 	 ols_math_r3 eb_math_r3  ipw_math_r3 
	global r4_cog ols_ppvt_stdscre_r4 eb_ppvt_stdscre_r4 ipw_ppvt_stdscre_r4 ols_lang_r4 	eb_lang_r4 	  ipw_lang_r4 	 ols_math_r4 eb_math_r4  ipw_math_r4  
	global r5_cog ols_ppvt_stdscre_r5 eb_ppvt_stdscre_r5 ipw_ppvt_stdscre_r5 ols_reading_r5 eb_reading_r5 ipw_reading_r5 ols_math_r5 eb_math_r5  ipw_math_r5 
	
	*Round 2:
	esttab $r2_cog using "$outputs\table5_r2.rtf", ///
			replace alignment(cr) mtitle("PPVT-OLS" "PPVT-EB" "PPVT-IPW" "CDA-OLS" "CDA-EB" "CDA-IPW") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)
	*Round 3:
	esttab $r3_cog using "$outputs\table5_r3.rtf", ///
			replace alignment(cr) mtitle("PPVT-OLS" "PPVT-EB" "PPVT-IPW" "EGRA-OLS" "EGRA-EB" "EGRA-IPW" "Math-OLS" "Math-EB" "Math-IPW") label se(%9.4f) ///
			star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)

	*Round 4:
	esttab $r4_cog using "$outputs\table5_r4.rtf", ///
			replace alignment(cr) mtitle("PPVT-OLS" "PPVT-EB" "PPVT-IPW" "Reading-OLS" "Reading-EB" "Reading-IPW" "Math-OLS" "Math-EB" "Math-IPW") label se(%9.4f) ///
			star(* 0.1 ** .05 *** 0.01) obs nocons	b(a2)
	
	*Round 5:
	esttab $r5_cog using "$outputs\table5_r5.rtf", ///
			replace alignment(cr) mtitle("PPVT-OLS" "PPVT-EB" "PPVT-IPW" "Reading-OLS" "Reading-EB" "Reading-IPW" "Math-OLS" "Math-EB" "Math-IPW") label se(%9.4f) ///
			star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)
	
	*************************
	**TABLE 6 ******
	*************************		
	global r3_non_cog ols_agency_r3 eb_agency_r3 ipw_agency_r3 ols_pride_r3 eb_pride_r3 ipw_pride_r3   
	global r4_non_cog ols_agency_r4 eb_agency_r4 ipw_agency_r4 ols_sef_r4 	eb_sef_r4 	ipw_sef_r4 		ols_ses_pride_r4 eb_ses_pride_r4 ipw_ses_pride_r4 
	global r5_non_cog ols_agency_r5 eb_agency_r5 ipw_agency_r5 ols_sef_r5 	eb_sef_r5 	ipw_sef_r5 		ols_ses_pride_r5 eb_ses_pride_r5 ipw_ses_pride_r5
	
	*Round 3:
	esttab $r3_non_cog using "$outputs\table6_r3.rtf", ///
			replace alignment(cr) mtitle("Agency-OLS" "Agency-EB" "Agency-IPW" "Self-esteem-OLS" "Self-esteem-EB" "Self-esteem-IPW") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)

	*Round 4:
	esttab $r4_non_cog using "$outputs\table6_r4.rtf", ///
			replace mtitle("Agency-OLS" "Agency-EB" "Agency-IPW" "Self-efficacy-OLS" "Self-efficacy-EB" " Self-efficacy-IPW" "Self-esteem-OLS" "Self-esteem-EB" "Self-esteem-IPW") ///
			label se(%9.4f) alignment(cr) star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)
	
	*Round 5:
	esttab $r5_non_cog using "$outputs\table6_r5.rtf", ///
			replace mtitle("Agency-OLS" "Agency-EB" "Agency-IPW" "Self-efficacy-OLS" "Self-efficacy-EB" " Self-efficacy-IPW" "Self-esteem-OLS" "Self-esteem-EB" "Self-esteem-IPW")  ///
			label se(%9.4f) alignment(cr) star(* 0.1 ** .05 *** 0.01) obs nocons b(a2)
	


