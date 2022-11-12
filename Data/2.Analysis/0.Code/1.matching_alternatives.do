********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\2.Analysis\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\1.alternatives_eval"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear
	/*global cov_child agemon_r1 female_r1
	global cov_hh    urban_r1 bad_event_r1
	global cov_cg    mother_edu_r1 age_pregnancy_r1 teen_pregnancy_r1 not_spanish_r1 dead_children_r1 abortion_r1 catholic_r1 no_boy_r1 child_not_first_r1 own_house_r1
	global cov_com	 pop_r1 env_problems_r1 community_crime_r1 barrio_r1 land_invasion_r1 quechua_r1 toilets_r1 water_r1 rubbish_r1 roads_paved_r1 pub_transp_r1 nofood_r1 movwork_r1 ///
					 litwork_r1 sufood_r1 prog_health_r1 prog_microcred_r1 police_r1 electricity_r1 drink_water_r1 park_r1 cinema_r1 bank_r1 internet_cab_r1 police_dist_r1  ///
					 electricity_dist_r1 drink_water_dist_r1 park_dist_r1 cinema_dist_r1 internet_cab_dist_r1 bank_dist_r1 pharmacy_dist_r1 pub_health_dist_r1*/
		
	global cov_hh    	urban bad_event 
	
	global cov_cg    	i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
						moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
						beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
								
	*global cov_com	   land_invasion_r1 toilets_r1 water_r1  movwork_r1 prog_health_r1 drink_water_r1 bank_r1 internet_cab_r1 
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	tab pecodpt1_r1, gen(departamento)

	keep if filter_final==0

	*Calculating the PS:
	logit treatment $covariates, or
	predict ps_logit

*************************************************************************
*I.I LOGIT PS-NEAREST NEIGHBOURS (NN):	
************************************************************************* 
kmatch ps treatment, pscore(ps_logit) nn comsup att
kmatch summarize $cov_hh $cov_cg
mat define logit_nn_mean_dif=r(M)
mat define logit_nn_var=r(V)
		
	*Std diff:
		coefplot matrix(logit_nn_mean_dif[,3]) matrix(logit_nn_mean_dif[,6]) , title ("Std. mean differences") noci nolabels xlabel(-0.6(0.1)0.6) legend(order(1 "Raw" 2 "Matched")) ///
																			   name(ps_logit_nn,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		graph export "$outputs/ps_logit_nn.png", as(png) replace
		putexcel set "$aux/logit_nn_mean_dif.xlsx", replace
		putexcel a1=matrix(logit_nn_mean_dif), names

		preserve
			import excel "$aux/logit_nn_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="PS"
			gen regression="logit"
			gen matching= "NN"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d 
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	*Variance ratio:
		coefplot matrix(logit_nn_var[,3]) matrix(logit_nn_var[,6]) , noci nolabels title(Variance ratio) legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(ps_logit_nn_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash)) 
		graph export "$outputs/ps_logit_nn_var.png", as(png) replace
		putexcel set "$aux/logit_nn_var.xlsx", replace
		putexcel a1=matrix(logit_nn_var), names

		preserve
			import excel "$aux\logit_nn_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="PS"
			gen regression="logit"
			gen matching= "NN"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d /*Var ratio reduced in 40%*/
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore

*************************************************************************
*I.II LOGIT PS-KERNEL (EPANECHIKOV) & CROSS-VALIDATED BANDWIDTH:	
*************************************************************************
*Matching:
kmatch ps treatment, pscore(ps_logit) kernel(epan) bwidth(cv) comsup att
kmatch cvplot, ms(o o) index mlabposition(1 1) sort name(ps_kernel_bw,replace) title(Kernel (PS)) /*plot to observe the cross-validation procedure*/
mat define ps_kernel_treat_exclusion=e(_N)
	
kmatch summarize $cov_hh $cov_cg
mat define ps_kernel_mean_dif=r(M)
mat define ps_kernel_var=r(V)
		
	*Std diff:
		coefplot matrix(ps_kernel_mean_dif[,3]) matrix(ps_kernel_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) ///
																					xlabel(-0.6(0.1)0.6) name(ps_kernel,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		graph export "$outputs/ps_kernel.png", as(png) replace
		putexcel set "$aux/ps_kernel_mean_dif.xlsx", replace
		putexcel a1=matrix(ps_kernel_mean_dif), names

		preserve
			import excel "$aux\ps_kernel_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="PS"
			gen regression="logit"
			gen matching= "Kernel"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d
			gen treated_excluded=ps_kernel_treat_exclusion[1,2]
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	* Variance ratio:
		coefplot matrix(ps_kernel_var[,3]) matrix(ps_kernel_var[,6]) , title("Variance ratio") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(ps_kernel_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		graph export "$outputs/ps_kernel_var.png", as(png) replace
		putexcel set "$aux/ps_kernel_var.xlsx", replace
		putexcel a1=matrix(ps_kernel_var), names

		preserve
			import excel "$aux\ps_kernel_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="PS"
			gen regression="logit"
			gen matching= "Kernel"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d
			*Var ratio reduced in 40%
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore			

*************************************************************************
*I.V LOGIT PS-INVERSE PROBABILITY WEIGHTING (IPW):	
*************************************************************************
*Matching imposing common support:
kmatch ipw treatment, pscore(ps_logit) comsup att
mat define ps_ipw_treat_exclusion=e(_N)
	
kmatch summarize $cov_hh $cov_cg
mat define ps_ipw_mean_dif=r(M)
mat define ps_ipw_var=r(V)
	
	*PS distribution:
	kmatch density ps_logit, lwidth(medthick medthick) lcolor(maroon navy) name(ipw_ps_dist,replace) ylabel(0(1)5)
	graph export "$outputs/ipw_ps_dist.png", as(png) replace	
		
	*Std diff:
		coefplot matrix(ps_ipw_mean_dif[,3]) matrix(ps_ipw_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) ///
																					xlabel(-0.6(0.1)0.6) name(ps_ipw,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		graph export "$outputs/ps_ipw.png", as(png) replace
		putexcel set "$aux/ps_ipw_mean_dif.xlsx", replace
		putexcel a1=matrix(ps_ipw_mean_dif), names

		preserve
			import excel "$aux\ps_ipw_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="PS"
			gen regression="logit"
			gen matching= "IPW"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d
			gen treated_excluded=ps_ipw_treat_exclusion[1,2]
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	*Variance ratio:
		coefplot matrix(ps_ipw_var[,3]) matrix(ps_ipw_var[,6]) , title("Variance ratio") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(ps_ipw_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		graph export "$outputs/ps_ipw_var.png", as(png) replace
		putexcel set "$aux/ps_ipw_var.xlsx", replace
		putexcel a1=matrix(ps_ipw_var), names

		preserve
			import excel "$aux\ps_ipw_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="PS"
			gen regression="logit"
			gen matching= "IPW"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d
			*Var ratio reduced in 40%
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore		

*************************************************************************
*I.VI MAHALANOBIS DISTANCE (MD)-NN:	
*************************************************************************		
*Matching:
kmatch md treatment $cov_hh $cov_cg, metric(maha) nn comsup att
kmatch summarize $cov_hh $cov_cg
mat define maha_nn_mean_dif=r(M)
mat define maha_nn_var=r(V)
		
	*Std diff:
		coefplot matrix(maha_nn_mean_dif[,3]) matrix(maha_nn_mean_dif[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) xlabel(-0.6(0.1)0.6) ///
																			name(maha_nn,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		graph export "$outputs/maha_nn.png", as(png) replace
		putexcel set "$aux/maha_nn_mean_dif.xlsx", replace
		putexcel a1=matrix(maha_nn_mean_dif), names

		preserve
			import excel "$aux\maha_nn_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="Mahalanobis"
			gen regression="N/A"
			gen matching= "NN"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta",update
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	*Variance ratio:
		coefplot matrix(maha_nn_var[,3]) matrix(maha_nn_var[,6]) , title(Variance ratio) noci nolabels legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(maha_nn_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash)) 
		graph export "$outputs/maha_nn_var.png", as(png) replace
		putexcel set "$aux/maha_nn_var.xlsx", replace
		putexcel a1=matrix(maha_nn_var), names

		preserve
			import excel "$aux\maha_nn_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="Mahalanobis"
			gen regression="N/A"
			gen matching= "NN"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d
			*Var ratio reduced in 40%
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta", update
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
		
*************************************************************************
*I.VII MD-KERNEL (EPANECHIKOV) & CROSS-VALIDATED BANDWIDTH:	
*************************************************************************	
*Matching:
kmatch md treatment $cov_hh $cov_cg, metric(maha) kernel(epa) bwidth(cv) comsup att
kmatch cvplot, ms(o o) index mlabposition(1 1) sort title(kernel (MD)) name(maha_kernel_bw,replace)
mat define maha_kernel_treat_exclusion=e(_N)
	
kmatch summarize $cov_hh $cov_cg
mat define maha_kernel_mean_dif=r(M)
mat define maha_kernel_var=r(V)
		
	*Std diff:
		coefplot matrix(maha_kernel_mean_dif[,3]) matrix(maha_kernel_mean_dif[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) xlabel(-0.6(0.1)0.6) ///
																						name(maha_kernel,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash))
		graph export "$outputs/maha_kernel.png", as(png) replace
		putexcel set "$aux/maha_kernel_mean_dif.xlsx", replace
		putexcel a1=matrix(maha_kernel_mean_dif), names

		preserve
			import excel "$aux\maha_kernel_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="Mahalanobis"
			gen regression="N/A"
			gen matching= "Kernel"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d
			gen treated_excluded=maha_kernel_treat_exclusion[1,2]
			*Std dif. reduced on almost 50%"
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	* Variance ratio:
		coefplot matrix(maha_kernel_var[,3]) matrix(maha_kernel_var[,6]) , title("Variance ratio") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(maha_kernel_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		graph export "$outputs/maha_kernel_var.png", as(png) replace
		putexcel set "$aux/maha_kernel_var.xlsx", replace
		putexcel a1=matrix(maha_kernel_var), names

		preserve
			import excel "$aux\maha_kernel_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="Mahalanobis"
			gen regression="N/A"
			gen matching= "Kernel"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d
			*Var ratio reduced in 40%
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
	
*************************************************************************
*I.VIII ENTROPY:	
*************************************************************************
*Matching: 
kmatch eb treatment $covariates, targets(3) comsup att
mat define eb_treat_exclusion=e(_N)

kmatch summarize $cov_hh $cov_cg
mat define eb_mean_dif=r(M)
mat define eb_var=r(V)

	*PS distribution: 
	kmatch density ps_logit, lwidth(medthick medthick) lcolor(maroon navy) name(eb_ps_dist,replace) ylabel(0(1)5)
	graph export "$outputs/eb_ps_dist.png", as(png) replace
		
	*Std diff:
		coefplot matrix(eb_mean_dif[,3]) matrix(eb_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) ///
																					xlabel(-0.6(0.1)0.6) name(eb,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		graph export "$outputs/eb.png", as(png) replace
		putexcel set "$aux/eb_mean_dif.xlsx", replace
		putexcel a1=matrix(eb_mean_dif), names

		preserve
			import excel "$aux\eb_mean_dif.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated StdDif E F G) (variable treat_raw control_raw std_dif_raw treat_matched control_matched std_dif_matched)
			gen std_dif_matched_abs=abs(std_dif_matched)
			gen std_dif_raw_abs=abs(std_dif_raw)
			gen metric="N/A"
			gen regression="N/A"
			gen matching= "Entropy"
			gen evaluating="Standardised dif."
			sum std_dif_raw_abs,d
			sum std_dif_matched_abs,d
			gen treated_excluded=eb_treat_exclusion[1,2]
			*Std dif. reduced on almost 50%"
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore
			
	* Variance ratio:
		coefplot matrix(eb_var[,3]) matrix(eb_var[,6]) , title("Variance ratio") noci nolabels  legend(order(1 "Raw" 2 "Matched")) xlabel(0.2(0.1)1.8) ///
																	  name(eb_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		graph export "$outputs/eb_var.png", as(png) replace
		putexcel set "$aux/eb_var.xlsx", replace
		putexcel a1=matrix(eb_var), names

		preserve
			import excel "$aux\eb_var.xlsx", sheet("Sheet1") cellrange(A2:G26) firstrow clear
			rename ( A Treated Untreated Ratio E F G) (variable treat_raw control_raw var_ratio_raw treat_matched control_matched var_ratio_matched)
			gen metric="N/A"
			gen regression="N/A"
			gen matching= "Entropy"
			gen evaluating="Variance ratio"
			gen var_ratio_raw_adj=cond(var_ratio_raw<1,1/var_ratio_raw,var_ratio_raw)
			gen var_ratio_matched_adj=cond(var_ratio_matched<1,1/var_ratio_matched,var_ratio_matched)
			sum var_ratio_raw_adj,d
			sum var_ratio_matched_adj,d
			*Var ratio reduced in 40%
			merge 1:1 variable metric regression matching evaluating using "$outputs/matching_alternatives.dta"
			drop _merge
			save "$outputs/matching_alternatives.dta",replace
		restore

****************************************************************************************************************************************************************************
**************************************************************************II. HETEROGENOUS EFFECTS *************************************************************************	
****************************************************************************************************************************************************************************
		
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

		kmatch summarize ps_logit  $cov_hh $cov_cg 
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
				
		kmatch summarize ps_logit  $cov_hh $cov_cg 
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
	
*************************************************************************
*II.II URBAN:	
*************************************************************************

	*Calculating PS per type of location. Urban:
		logit treatment  $covariates  if filter_final==0 & urban_r1==1, or
		predict ps_logit_urban if filter_final==0 & urban_r1==1
		
		gen ps_logit_treat_urban=cond(treatment==1 & urban_r1==1 & filter_final==0,ps_logit_urban,.)
		gen ps_logit_control_urban=cond(treatment==0 & urban_r1==1 & filter_final==0,ps_logit_urban,.)
		
		twoway (kdensity ps_logit_treat_urban, lwidth(thick)) (kdensity ps_logit_control_urban, lwidth(thick)), xtitle("PS")  xlabel(0(0.1)1) ylabel(0(1)6) ///
																								name(ps_common_support_urban,replace) ///
																								legend (label(1 "Treatment") label(2 "Control")) title(Urban location)
		graph export "$outputs/ps_common_support_urban.png", as(png) replace
		
	*Rural areas:
		logit treatment  $covariates  if filter_final==0 & urban_r1==0, or
		predict ps_logit_rural if filter_final==0 & urban_r1==0

		gen ps_logit_treat_rural=cond(treatment==1 & urban_r1==0,ps_logit_rural,.)
		gen ps_logit_control_rural=cond(treatment==0 & urban_r1==0,ps_logit_rural,.)
		twoway (kdensity ps_logit_treat_rural, lwidth(thick)) (kdensity ps_logit_control_rural, lwidth(thick)), xtitle("PS") name(ps_common_support_rural,replace) ylabel(0(1)6) ///
																								legend (label(1 "Treatment") label(2 "Control")) xlabel(0(0.1)1) title(Rural location)
		graph export "$outputs/ps_common_support_rural.png", as(png) replace
		
	*Both:
		graph combine ps_common_support_urban ps_common_support_rural, col(1) name(ps_common_support_location,replace) xsize(3)
		
		graph export "$outputs/ps_common_support_location.png", as(png) replace
		
	**********************************************
	*II.I.I IPW:	
	**********************************************
		kmatch ipw treatment  $covariates  if filter_final==0,  comsup att over(urban_r1)
		mat define ps_ipw_treat_exclusion=e(_N)

		kmatch summarize ps_logit  $cov_hh $cov_cg 
		mat define ps_ipw_mean_dif=r(M)
		mat define ps_ipw_var=r(V)
		mat ps_ipw_mean_dif_urban=ps_ipw_mean_dif[36..70,1..6]
		mat ps_ipw_mean_dif_rural=ps_ipw_mean_dif[1..35,1..6]		

		coefplot matrix(ps_ipw_mean_dif_urban[,3]) matrix(ps_ipw_mean_dif_urban[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																						xlabel(-0.8(0.1)1.3, labsize(small)) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																						name(ipw_std_dif_urban,replace)
		graph export "$outputs/ps_ipw_location-urban.png", as(png) replace
																						 
		coefplot matrix(ps_ipw_mean_dif_rural[,3]) matrix(ps_ipw_mean_dif_rural[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) ///
																						xlabel(-0.8(0.1)1.3, labsize(small)) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																						name(ipw_std_dif_rural,replace)
		graph export "$outputs/ps_ipw_location-rural.png", as(png) replace
		
	**********************************************
	*II.I.II Entropy:	
	**********************************************
		kmatch eb treatment  $covariates   if filter_final==0, comsup att over(urban_r1)
		mat define ps_ipw_treat_exclusion=e(_N)
				
		kmatch summarize ps_logit  $cov_hh $cov_cg 
		mat define eb_mean_dif=r(M)
		mat define eb_var=r(V)
		mat eb_mean_dif_urban=eb_mean_dif[36..70,1..6]
		mat eb_mean_dif_rural=eb_mean_dif[1..35,1..6]		

		coefplot matrix(eb_mean_dif_urban[,3]) matrix(eb_mean_dif_urban[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																				xlabel(-0.8(0.1)1.3, labsize(small)) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																				name(eb_std_dif_urban,replace)
		graph export "$outputs/eb_location-urban.png", as(png) replace
		
		coefplot matrix(eb_mean_dif_rural[,3]) matrix(eb_mean_dif_rural[,6]) , title("Std. mean difference") noci nolabels  legend(order(1 "Raw" 2 "Matched")) ///
																				xlabel(-0.8(0.1)1.3, labsize(small)) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																				name(eb_std_dif_rural,replace)		
		graph export "$outputs/eb_location-rural.png", as(png) replace

*************************************************************************
*II.III SIBLINGS: OLDER	
*************************************************************************
	*Generating variable to identify the cases: children who had older siblings at home during most of the rounds
		egen dropme=rowmean(all_sibling_old_r1 all_sibling_old_r2 all_sibling_old_r3 all_sibling_old_hh_r4 all_sibling_old_hh_r5)
		gen sibling_old=cond(dropme>0.5,1,0)
		drop dropme
	
	*Calculating PS for children who had an older sibling:
		logit treatment  $covariates  if filter_final==0 & sibling_old==1, or
		predict ps_logit_sibling_old if filter_final==0 & sibling_old==1
		
		gen ps_logit_treat_sibling_old =cond(treatment==1 & sibling_old==1 & filter_final==0,ps_logit_sibling_old,.)
		gen ps_logit_control_sibling_old =cond(treatment==0 & sibling_old==1 & filter_final==0,ps_logit_sibling_old,.)
		
		twoway (kdensity ps_logit_treat_sibling_old, lwidth(thick)) (kdensity ps_logit_control_sibling_old, lwidth(thick)), xtitle("PS") xlabel(0(0.1)0.9) ylabel(0(1)8) ///
																														name(ps_common_support_sibling_old,replace) ///
																														legend (label(1 "Treatment") label(2 "Control")) title(Older sibling)
		graph export "$outputs/ps_common_support_has-older-sibling.png", as(png) replace
		
	*No older sibling:
		logit treatment  $covariates  if filter_final==0 & sibling_old==0, or
		predict ps_logit_sibling_old_no if filter_final==0 & sibling_old==0

		gen ps_logit_treat_sibling_old_no=cond(treatment==1 & sibling_old==0,ps_logit_sibling_old_no,.)
		gen ps_logit_control_sibling_old_no=cond(treatment==0 & sibling_old==0,ps_logit_sibling_old_no,.)
		twoway (kdensity ps_logit_treat_sibling_old_no, lwidth(thick)) (kdensity ps_logit_control_sibling_old_no, lwidth(thick)), xtitle("PS") xlabel(0(0.1)0.9) ylabel(0(1)8) ///
																																  name(ps_common_support_sibling_old_no,replace) ///
																																  legend (label(1 "Treatment") label(2 "Control")) ///
																																  title(No older sibling) 
		graph export "$outputs/ps_common_support_no-older-sibling.png", as(png) replace
		
		graph combine ps_common_support_sibling_old ps_common_support_sibling_old_no, col(1) name(ps_common_support_sibling_old_he,replace) xsize(3)
		
		graph export "$outputs/ps_common_support_older-siblings_both.png", as(png) replace
		
	**********************************************
	*II.I.I IPW:	
	**********************************************
		kmatch ipw treatment  $covariates  if filter_final==0,  comsup att over(sibling_old)
		mat define ps_ipw_treat_exclusion=e(_N)

		kmatch summarize ps_logit  $cov_hh $cov_cg 
		mat define ps_ipw_mean_dif=r(M)
		mat define ps_ipw_var=r(V)
		mat ps_ipw_mean_dif_sibling_old=ps_ipw_mean_dif[36..70,1..6]
		mat ps_ipw_mean_dif_sibling_old_no=ps_ipw_mean_dif[1..35,1..6]		

		coefplot matrix(ps_ipw_mean_dif_sibling_old[,3]) matrix(ps_ipw_mean_dif_sibling_old[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																									xlabel(-0.5(0.1)1.1) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																									name(ipw_std_dif_sibling_old,replace)
		graph export "$outputs/ps_ipw_location-has-older-sibling.png", as(png) replace
																						 
		coefplot matrix(ps_ipw_mean_dif_sibling_old_no[,3]) matrix(ps_ipw_mean_dif_sibling_old_no[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																											 xlabel(-0.5(0.1)1.1) ylabel(,labsize(tiny)) xline(0) ///
																											 xline(-0.1 0.1, lpattern(dash)) name(ps_ipw_mean_dif_sibling_old_no,replace)
		graph export "$outputs/ps_ipw_location-no-older-sibling.png", as(png) replace
		
	**********************************************
	*II.I.II Entropy:	
	**********************************************
		kmatch eb treatment  $covariates  if filter_final==0, comsup att over(sibling_old)
		mat define ps_ipw_treat_exclusion=e(_N)
				
		kmatch summarize ps_logit  $cov_hh $cov_cg 
		mat define eb_mean_dif=r(M)
		mat define eb_var=r(V)
		mat eb_mean_dif_sibling_old=eb_mean_dif[36..70,1..6]
		mat eb_mean_dif_sibling_old_no=eb_mean_dif[1..35,1..6]		

		coefplot matrix(eb_mean_dif_sibling_old[,3]) matrix(eb_mean_dif_sibling_old[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																									xlabel(-0.5(0.1)1.1) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																									name(eb_std_dif_sibling_old,replace)
		graph export "$outputs/eb_location-has-older-sibling.png", as(png) replace
																						 
		coefplot matrix(eb_mean_dif_sibling_old_no[,3]) matrix(eb_mean_dif_sibling_old_no[,6]) , title("Std. mean difference") noci nolabels legend(order(1 "Raw" 2 "Matched")) ///
																											 xlabel(-0.5(0.1)1.1) ylabel(,labsize(tiny)) xline(0) ///
																											 xline(-0.1 0.1, lpattern(dash)) name(eb_mean_dif_sibling_old_no,replace)
		graph export "$outputs/eb_location-no-older-sibling.png", as(png) replace
		
	
