********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\2.Analysis\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\Figures"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear		

	global cov_hh    		urban bad_event 
	
	global cov_cg    		i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
							moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
							beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	tab pecodpt1_r1, gen(departamento)

	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)
	
	keep if filter_final==0
	
	*Calculating the PS:
	logit treatment $covariates, or
	predict ps_logit

	*Looking at the common support:
	histogram ps_logit if treatment==0, name(logit_control,replace) width(0.02) start(0) xlabel(0(0.1)0.8) ylabel(0(1)7) title("PS logit regression-control")
	histogram ps_logit if treatment==1, name(logit_treatment,replace) width(0.02) start(0) xlabel(0(0.1)0.8) ylabel(0(1)7) title("PS logit regression-treatment")
	graph combine logit_control logit_treatment, col(1) name(logit,replace)

	graph box ps_logit, over(treatment, relabel (1 "Control" 2 "Treatment")) name(logit_boxplot, replace) ylabel(0(0.1)0.8) ytitle(PS (logit regression))

	gen ps_logit_treat=cond(treatment==1,ps_logit,.)
	gen ps_logit_control=cond(treatment==0,ps_logit,.)

	*************************
	**FIGURE 1 **************
	************************* 
	twoway (kdensity ps_logit_treat, lwidth(thick)) (kdensity ps_logit_control, lwidth(thick)), xtitle("Propensity score") legend (label(1 "Treatment") label(2 "Control")) xlabel(0(0.1)0.8) ///
																				ylabel(1(1)7) name(ps_common_support,replace) /*still with problems of common support*/
graph export "$outputs/figure1.png", as(png) replace