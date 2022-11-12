********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\1.Build\2.Aux"
global outputs_1	"$root\2.Analysis\3.Outputs\2.balance"
global outputs_2	"$root\2.Analysis\3.Outputs\3.impact"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************

use "$inputs\rounds_all.dta" , clear
	
	global cov_hh    					urban bad_event 
	
	global cov_cg    					mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
										moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
										beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
								
	*global cov_com						land_invasion_r1 toilets_r1 water_r1  movwork_r1 prog_health_r1 drink_water_r1 bank_r1 internet_cab_r1 
	
	global outcomes 					ppvt_stdscre_r2 cda_r2 ppvt_stdscre_r3 egra_r3 math_r3 agency_r3 ppvt_stdscre_r4 lang_r4 math_r4  agency_r4  sef_r4  ses_pride_r4 ///
										ppvt_stdscre_r5 reading_r5 math_r5  agency_r5 sef_r5  ses_pride_r5
	
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	tab pecodpt1_r1, gen(departamento)

	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)

	* No information of father absence (round 1):
	tab treatment,m

	* Attrition
	tab treatment attrition,m
	gen no_attrition=cond(treatment!=. & attrition==0,1,0)

	* Incomplete information for propensity score estimation:
	egen missing2=rowmiss($cov_hh $cov_cg)
	gen missing_ps=cond(missing2==0 & attrition==0 & treatment!=.,0,1)

	* Incomplete information of outcomes
	egen missing3=rowmiss($outcomes )
	gen missing_outcomes=cond(missing3==0 & missing2==0 & attrition==0 & treatment!=.,0,1)

	* Lack of common support
	global cov_cg2    		i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
							moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
							beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
	
	global covariates2 "$cov_hh $cov_cg2 i.pecodpt1_r1"
	
	*kmatch ipw treatment $covariates2 if filter_final==0,  comsup att wgen(w_ipw) 
	*gen comsup=cond(w_ipw==0,1,0)

	* Putting all together:
	gen sample=cond(treatment==.,1,cond(no_attrition==0,2,cond(missing_ps==1,3,cond(missing_outcomes==1,4,5))))

	label define lbsample 1 "Out of sample of interest" 2 "Attrition" 3 "Incomp. info. ps estimation" 4 "Incomp. info outcomes" 5 "Effective sample",replace
	label values sample lbsample

	*************************
	**TABLE 2 ***************
	************************* 
tab sample
tab sample if sample!=1 
tab treatment if filter_final==0