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
	
	drop if treatment==.
************************************************************************************************
**II. IMPACT: OUTCOMES**************************************************************************
************************************************************************************************
global balance urban bad_event mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
										moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
										beaten_mom birth_home_r1 birth_no_prof_r1 cg_mother_not_spanish_r1 read_spanish_dif_r1 own_house_r1

foreach var in $balance{
	gen `var'_copy=`var'
	sum `var' if filter_final==0
	replace `var'=(`var'_copy-r(mean))/r(sd)
	}
				   
quietly estpost sum $balance if filter_final==0
est store included

quietly estpost sum $balance if filter_final==1 
est store excluded

eststo difference: quietly estpost ttest $balance, by(filter_final)

*************************
**TABLE XXXXXX ******
*************************								
esttab included excluded difference using "$outputs/tablee1.rtf", replace mtitles("Included (I)" "Excluded (E)" "Difference (I-C)") obs ////
																   cells("mean(pattern(1 1 0)fmt(2)) b(star pattern(0 0 1) fmt(2))") ////
																			collabels(" " " " " ") star(* 0.1 ** .05 *** 0.01) label nonum