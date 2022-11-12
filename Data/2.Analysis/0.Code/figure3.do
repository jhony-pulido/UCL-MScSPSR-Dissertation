********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global outputs		"$root\2.Analysis\3.Outputs\Figures"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear		
	
	global cov_hh  		urban bad_event 
	
	global cov_cg  		i.mother_edu age_pregnancy teen_pregnancy not_spanish dead_children abortion no_boy child_not_first child_wanted  ///
						moves_cohab_preg_dum moves_cohab_bef_dum moves_separ_bef_dum  moves_disp_bef_dum  no_moves_before_dum  antenatal_visit poor_pregnancy ///
						beaten_mom
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"

	keep if filter_final==0
********************************************************************************************************************
	*Generating the weights:
	*kmatch ipw treatment $covariates,  comsup att wgen(w_ipw)
	kmatch eb treatment $covariates,   att wgen(w_eb)
	
	drop if w_eb==0

	*Observing the distribution of weights:
	graph hbox w_eb if treatment==0, name(w_eb,replace) ylabel(0 (0.1)2.9,labsize(small) angle(270))  ytitle("EB weights (control group)", height(5)) 
	*graph hbox w_ipw if treatment==0, name(w_ipw,replace) title(B. Inverse Probability Weighting, size(huge)) ylabel(0 (0.1)3,labsize(medium) angle(270)) ytitle("")
	*graph combine w_eb w_ipw, col(2) name(weights_dist,replace)

	*Comparing weights:
	*gen w_comparison=w_eb-w_ipw
	
	*graph hbox w_comparison if treatment==0, name(w_comp,replace) title(C. Weights comparison, size(vlarge)) ylabel(-0.7(0.05)0.45,labsize(medium)) ytitle(Differences in weights)
	*graph combine weights_dist w_comp, row(2) name(weights,replace)
graph export "$outputs/figure3.png", as(png) replace