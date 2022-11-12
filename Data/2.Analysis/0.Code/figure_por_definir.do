********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\2.Analysis\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\1.alternatives_eval"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear
	global cov_hh    		urban bad_event 
	
	global cov_cg  			i.mother_edu age_pregnancy teen_pregnancy not_spanish dead_children abortion no_boy child_not_first child_wanted  ///
							moves_cohab_preg_dum moves_cohab_bef_dum moves_separ_bef_dum  moves_disp_bef_dum  no_moves_before_dum  antenatal_visit poor_pregnancy ///
							beaten_mom own_house

	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	tab pecodpt1_r1, gen(departamento)

	keep if filter_final==0

*************************************************************************
*II.I SEX:	
*************************************************************************

	*Calculating PS per sex. Females:
		logit treatment  $covariates  if filter_final==0 & female_r1==1, or
		predict ps_logit_fem if filter_final==0
		
		gen ps_logit_treat_fem=cond(treatment==1 & female_r1==1 & filter_final==0,ps_logit_fem,.)
		gen ps_logit_control_fem=cond(treatment==0 & female_r1==1 & filter_final==0,ps_logit_fem,.)
		
		twoway (kdensity ps_logit_treat_fem, lwidth(thick)) (kdensity ps_logit_control_fem, lwidth(thick)), xtitle("PS")  xlabel(0(0.1)0.8) ylabel(0(1)6) ///
																											legend (label(1 "Treatment") label(2 "Control")) ///
																											 name(ps_common_support_fem,replace) title(Female)
		graph export "$outputs/ps_common_support_fem.png", as(png) replace

	*Males:
		logit treatment  $covariates  if filter_final==0 & female_r1==0, or
		predict ps_logit_male if filter_final==0

		gen ps_logit_treat_male=cond(treatment==1 & female_r1==0,ps_logit_male,.)
		gen ps_logit_control_male=cond(treatment==0 & female_r1==0,ps_logit_male,.)
		twoway (kdensity ps_logit_treat_male, lwidth(thick)) (kdensity ps_logit_control_male, lwidth(thick)), xtitle("PS") xlabel(0(0.1)0.8) ylabel(0(1)6) ///
																												legend (label(1 "Treatment") label(2 "Control")) ///
																												name(ps_common_support_male,replace) title(Male)
		graph export "$outputs/ps_common_support_male.png", as(png) replace																										
	
	*Both:
		graph combine ps_common_support_fem ps_common_support_male, col(1) name(ps_common_support_sex,replace) xsize(3)
		
		graph export "$outputs/ps_common_support_sex.png", as(png) replace
		
	**********************************************
	*II.I.I IPW:	
	**********************************************
		kmatch ipw treatment  $covariates  if filter_final==0,  comsup att over(female_r1)
		mat define ps_ipw_treat_exclusion=e(_N)

		kmatch summarize  $cov_hh $cov_cg 
		mat define ps_ipw_mean_dif=r(M)
		mat define ps_ipw_var=r(V)
		mat ps_ipw_mean_dif_female=ps_ipw_mean_dif[22..42,1..6]
		mat ps_ipw_mean_dif_male=ps_ipw_mean_dif[1..21,1..6]		

		coefplot matrix(ps_ipw_mean_dif_female[,3]) matrix(ps_ipw_mean_dif_female[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) xlabel(-0.7(0.1)1.1) ///
																						 ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) name(ipw_std_dif_fem,replace)
		graph export "$outputs/ps_ipw_sex-female.png", as(png) replace
		
		coefplot matrix(ps_ipw_mean_dif_male[,3]) matrix(ps_ipw_mean_dif_male[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(-0.7(0.1)1.1) ///
																					 ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) name(ipw_std_dif_male,replace)
		graph export "$outputs/ps_ipw_sex-male.png", as(png) replace
	
	**********************************************
	*II.I.II Entropy:	
	**********************************************
		kmatch eb treatment  $covariates  if filter_final==0, targets(2) comsup att over(female_r1)
		mat define ps_ipw_treat_exclusion=e(_N)
				
		kmatch summarize own_house  $cov_hh $cov_cg 
		mat define eb_mean_dif=r(M)
		mat define eb_var=r(V)
		mat eb_mean_dif_female=eb_mean_dif[22..42,1..6]
		mat eb_mean_dif_male=eb_mean_dif[1..21,1..6]		

		coefplot matrix(eb_mean_dif_female[,3]) matrix(eb_mean_dif_female[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) xlabel(-0.7(0.1)1.1) ///
																				 ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) name(eb_std_dif_fem,replace)
		graph export "$outputs/eb_sex-female.png", as(png) replace
		
		coefplot matrix(eb_mean_dif_male[,3]) matrix(eb_mean_dif_male[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(-0.7(0.1)1.1) ///
																			 ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) name(eb_std_dif_male,replace)		
		graph export "$outputs/eb_sex-male.png", as(png) replace