********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\2.Analysis\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\Figures"
********************************************************************************************************************
use "$inputs\rounds_all.dta" , clear
	
	global cov_hh  		urban bad_event 
	
	global cov_cg    		i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
							moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
							beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
				 
	global covariates "$cov_hh $cov_cg i.pecodpt1_r1"
	
	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)
	keep if filter_final==0

	*Calculating the PS:
	eststo pscore: logit treatment $covariates, or 

	************************************
	*********TABLE C1*******************
	************************************
esttab pscore using "$outputs\tablec1.rtf", replace nobaselevels alignment(cr) title("") mtitle("Early father absence") eform label se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
												refcat(1.mother_edu "Mother education (ref.: less than secondary)", nolabel) pr2 drop(*.pecodpt1_r1)
