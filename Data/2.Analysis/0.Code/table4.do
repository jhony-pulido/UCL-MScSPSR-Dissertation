********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\1.Build\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\Tables"
********************************************************************************************************************

use "$inputs\rounds_all.dta" , clear
	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)
**************************************************************************************
**I. BALANCE TESTS********************************************************************
**************************************************************************************
global balance urban bad_event mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
										moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
										beaten_mom birth_home_r1 birth_no_prof_r1 cg_mother_not_spanish_r1 read_spanish_dif_r1 own_house_r1

foreach var in $balance{
	gen `var'_copy=`var'
	sum `var' if treatment==0 & filter_final==0
	replace `var'=(`var'_copy-r(mean))/r(sd)
	}
				   
quietly estpost sum $balance if filter_final==0 & treatment==1
est store treated

quietly estpost sum $balance if filter_final==0 & treatment==0
est store control

gen treatment_inverted=cond(treatment==.,.,cond(treatment==1,0,1))

eststo difference: quietly estpost ttest $balance if filter_final==0, by(treatment_inverted)

*************************
**TABLE 4 ******
*************************								
esttab treated control difference using "$outputs/table4.rtf", replace mtitles("Treatment (T)" "Control (C)" "Difference (T-C)") obs ////
																   cells("mean(pattern(1 1 0)fmt(2)) b(star pattern(0 0 1) fmt(2))") ////
																			collabels(" " " " " ") star(* 0.1 ** .05 *** 0.01) label nonum