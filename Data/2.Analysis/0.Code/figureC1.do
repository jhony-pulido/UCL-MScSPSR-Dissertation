********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global outputs		"$root\2.Analysis\3.Outputs\Figures"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear		
	
	global cov_hh  		urban bad_event 
	
	global cov_cg    		i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
							moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
							beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	tab pecodpt1_r1, gen(departamento)
	
	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)
	keep if filter_final==0
********************************************************************************************************************

	*Calculating the PS:
	logit treatment $covariates, or
	predict ps_logit

	*************************************************************************
	*I.I LOGIT PS-NEAREST NEIGHBOURS (NN):	
	************************************************************************* 
	kmatch ps treatment $covariates, nn comsup att
	kmatch summarize $cov_hh $cov_cg
	mat define logit_nn_mean_dif=r(M)
	mat define logit_nn_var=r(V)
			
	*Std diff:
		coefplot matrix(logit_nn_mean_dif[,3]) matrix(logit_nn_mean_dif[,6]) , title ("Std. mean differences") noci nolabels xlabel(-0.6(0.1)0.6,labsize(vsmall)) ///
																				legend(off) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) ///
																				name(ps_logit_nn,replace) 
		*graph export "$outputs/ps_logit_nn.png", as(png) replace
				
	*Variance ratio:
		coefplot matrix(logit_nn_var[,3]) matrix(logit_nn_var[,6]) , noci nolabels title(Variance ratio) legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																	  name(ps_logit_nn_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash)) 
		*graph export "$outputs/ps_logit_nn_var.png", as(png) replace

		graph combine  ps_logit_nn ps_logit_nn_var, col(2) title(Propensity score-NN) name(ps_nn,replace) xsize(8)

	*************************************************************************
	*I.II LOGIT PS-KERNEL (EPANECHIKOV) & CROSS-VALIDATED BANDWIDTH:	
	*************************************************************************
	*Matching:
	kmatch ps treatment $covariates, kernel(epan) bwidth(cv) comsup att	
	kmatch summarize $cov_hh $cov_cg
	mat define ps_kernel_mean_dif=r(M)
	mat define ps_kernel_var=r(V)
			
	*Std diff:
		coefplot matrix(ps_kernel_mean_dif[,3]) matrix(ps_kernel_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(off) ///
																					xlabel(-0.6(0.1)0.6,labsize(vsmall)) name(ps_kernel,replace) ylabel(,labsize(tiny)) xline(0) ///
																					xline(-0.1 0.1, lpattern(dash)) 
		*graph export "$outputs/ps_kernel.png", as(png) replace
				
	*Variance ratio:
		coefplot matrix(ps_kernel_var[,3]) matrix(ps_kernel_var[,6]) , title("Variance ratio") noci nolabels  legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																	  name(ps_kernel_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		*graph export "$outputs/ps_kernel_var.png", as(png) replace
		graph combine  ps_kernel ps_kernel_var, col(2) title(Propensity score-Kernel) name(ps_kernel,replace) xsize(8)
		
		*graph combine  ps_nn ps_kernel, row(2)
		
	*************************************************************************
	*I.V LOGIT PS-INVERSE PROBABILITY WEIGHTING (IPW):	
	*************************************************************************
	*Matching imposing common support:
	kmatch ipw treatment $covariates, comsup att
		
	kmatch summarize $cov_hh $cov_cg
	mat define ps_ipw_mean_dif=r(M)
	mat define ps_ipw_var=r(V)
		
	*Std diff:
		coefplot matrix(ps_ipw_mean_dif[,3]) matrix(ps_ipw_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(off) ///
																					xlabel(-0.6(0.1)0.6,labsize(vsmall)) name(ps_ipw,replace) ylabel(,labsize(tiny)) xline(0) ///
																					xline(-0.1 0.1, lpattern(dash)) 
		*graph export "$outputs/ps_ipw.png", as(png) replace
				
	*Variance ratio:
		coefplot matrix(ps_ipw_var[,3]) matrix(ps_ipw_var[,6]) , title("Variance ratio") noci nolabels  legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																	  name(ps_ipw_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
		*graph export "$outputs/ps_ipw_var.png", as(png) replace

		graph combine  ps_ipw ps_ipw_var, col(2) title(Propensity score-IPW) name(ps_ipw,replace) xsize(8)
		
		*graph combine  ps_nn ps_kernel ps_ipw, row(2) col(2) xsize(8)
	*************************************************************************
	*I.VI MAHALANOBIS DISTANCE (MD)-NN:	
	*************************************************************************		
	*Matching:
	kmatch md treatment $cov_hh $cov_cg, metric(maha) nn comsup att
	kmatch summarize $cov_hh $cov_cg
	mat define maha_nn_mean_dif=r(M)
	mat define maha_nn_var=r(V)
			
	*Std diff:
		coefplot matrix(maha_nn_mean_dif[,3]) matrix(maha_nn_mean_dif[,6]) , title("Std. mean difference") noci nolabels legend(off) xlabel(-0.6(0.1)0.6,labsize(vsmall)) ///
																			name(maha_nn,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash)) 
		*graph export "$outputs/maha_nn.png", as(png) replace

	*Variance ratio:
		coefplot matrix(maha_nn_var[,3]) matrix(maha_nn_var[,6]) , title(Variance ratio) noci nolabels legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																	  name(maha_nn_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash)) 
		*graph export "$outputs/maha_nn_var.png", as(png) replace
		
		graph combine  maha_nn maha_nn_var, col(2) title(Mahalanobis Distance-NN) name(maha_nn,replace) xsize(8)
		
		*graph combine   ps_nn ps_kernel ps_ipw maha_nn, row(2) col(2)  xsize(12)
	*************************************************************************
	*I.VII MD-KERNEL (EPANECHIKOV) & CROSS-VALIDATED BANDWIDTH:	
	*************************************************************************	
	*Matching:
	kmatch md treatment $cov_hh $cov_cg, metric(maha) kernel(epa) bwidth(cv) comsup att
		
	kmatch summarize $cov_hh $cov_cg
	mat define maha_kernel_mean_dif=r(M)
	mat define maha_kernel_var=r(V)
			
	*Std diff:
		coefplot matrix(maha_kernel_mean_dif[,3]) matrix(maha_kernel_mean_dif[,6]) , title("Std. mean difference") noci nolabels legend(off) xlabel(-0.6(0.1)0.6,labsize(vsmall)) ///
																						name(maha_kernel,replace) ylabel(,labsize(tiny)) xline(0) xline(-0.1 0.1, lpattern(dash))
		*	graph export "$outputs/maha_kernel.png", as(png) replace
				
	*Variance ratio:
		coefplot matrix(maha_kernel_var[,3]) matrix(maha_kernel_var[,6]) , title("Variance ratio") noci nolabels  legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																		name(maha_kernel_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
	*		graph export "$outputs/maha_kernel_var.png", as(png) replace
		
		graph combine  maha_kernel maha_kernel_var, col(2) title(Mahalanobis Distance-Kernel) name(maha_kernel,replace)
		
		*graph combine   ps_nn ps_kernel ps_ipw maha_nn maha_kernel, row(2) col(3) 	
	*************************************************************************
	*I.VIII ENTROPY:	
	*************************************************************************
	*Matching: 
	kmatch eb treatment $covariates, targets(3) comsup att
	kmatch summarize $cov_hh $cov_cg
	mat define eb_mean_dif=r(M)
	mat define eb_var=r(V)

	*Std diff:
		coefplot matrix(eb_mean_dif[,3]) matrix(eb_mean_dif[,6]) , title("Std. mean difference") noci nolabels  legend(off) ///
																					xlabel(-0.6(0.1)0.6,labsize(vsmall)) name(eb,replace) ylabel(,labsize(tiny)) xline(0) ///
																					xline(-0.1 0.1, lpattern(dash)) 
	*	graph export "$outputs/eb.png", as(png) replace
				
	*Variance ratio:
		coefplot matrix(eb_var[,3]) matrix(eb_var[,6]) , title("Variance ratio") noci nolabels  legend(off) xlabel(0.2(0.1)1.8,labsize(vsmall)) ///
																		  name(eb_var,replace) ylabel(,labsize(tiny)) xline(1) xline(0.9 1.1, lpattern(dash))
	*	graph export "$outputs/eb_var.png", as(png) replace

		graph combine  eb eb_var, col(2) title(Entropy balancing) name(eb,replace) 
		
		graph combine   ps_nn maha_nn ps_kernel maha_kernel ps_ipw eb, row(3) col(2)  xsize(17) ysize(8)
graph export "$outputs/figureC1.png", as(png) replace

	
