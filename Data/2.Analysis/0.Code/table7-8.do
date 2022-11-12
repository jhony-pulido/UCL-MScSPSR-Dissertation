********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\1.Build\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\Figures"
global outputs_2	"$root\2.Analysis\3.Outputs\3.impact"
global code			"$root\1.Build\0.Code"
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
**IV. IMPACT: HETEROGENOUS EFFECTS**************************************************************
************************************************************************************************
foreach var in $outcomes{
	gen `var'_copy=`var'
	sum `var' if treatment==0 & filter_final==0
	replace `var'=(`var'_copy-r(mean))/r(sd)
	}

kmatch ipw treatment $covariates if filter_final==0, att wgen(w_ipw) 
kmatch eb treatment $covariates if filter_final==0,   att  wgen(w_eb) targets(2)

label var treatment "Father absence"

	************************************
	**IV.I. SEX ************************
	************************************
		*Generating the matching weights:
		label var treatment "Father absence"
		label var female_r1 "Female"

		foreach var in $outcomes{
			display "Girls:"
			eststo `var'_g: reg `var' i.treatment [aw=w_eb] if filter_final==0 & female_r1==1,  vce(cluster clustid_r1)
			display "Boys:"
			eststo `var'_b: reg `var' i.treatment [aw=w_eb] if filter_final==0 & female_r1==0,  vce(cluster clustid_r1)
			display "Dif"
			eststo `var'_dif: reg `var' i.female_r1##i.treatment [aw=w_eb] if filter_final==0,  vce(cluster clustid_r1)
			}

		*************************
		**TABLE 7 ******
		*************************		
			global r2_cog ppvt_stdscre_r2_b ppvt_stdscre_r2_g ppvt_stdscre_r2_dif  cda_r2_b cda_r2_g cda_r2_dif 
			global r3_cog ppvt_stdscre_r3_b ppvt_stdscre_r3_g ppvt_stdscre_r3_dif  egra_r3_b egra_r3_g egra_r3_dif 			math_r3_b math_r3_g math_r3_dif  
			global r4_cog ppvt_stdscre_r4_b ppvt_stdscre_r4_g ppvt_stdscre_r4_dif  lang_r4_b lang_r4_g lang_r4_dif 			math_r4_b math_r4_g math_r4_dif
			global r5_cog ppvt_stdscre_r5_b ppvt_stdscre_r5_g ppvt_stdscre_r5_dif  reading_r5_b reading_r5_g reading_r5_dif math_r5_b math_r5_g math_r5_dif
			
			*Round 2:
			esttab $r2_cog using "$outputs\table7_r2.rtf", replace alignment(cr) mtitle("PPVT-B" "PPVT-G" "PPVT-Dif" "CDA-B" "CDA-G" "CDA-Dif") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
															obs nobaselevels drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference )
						
			*Round 3:
			esttab $r3_cog using "$outputs\table7_r3.rtf", replace alignment(cr) mtitle("PPVT-B" "PPVT-G" "PPVT-Dif" "EGRA-B" "EGRA-G" "EGRA-Dif" "Math-B" "Math-G" "Math-Dif") label ///
															se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nobaselevels drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference)
				
			*Round 4:
			esttab $r4_cog using "$outputs\table7_r4.rtf", replace alignment(cr) mtitle("PPVT-B" "PPVT-G" "PPVT-Dif" "Reading-B" "Reading-G" "Reading-Dif" "Math-B" "Math-G" "Math-Dif") label ///
															se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nobaselevels drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference)
		
			*Round 5:
			esttab $r5_cog using "$outputs\table7_r5.rtf", replace alignment(cr) mtitle("PPVT-B" "PPVT-G" "PPVT-Dif" "Reading-B" "Reading-G" "Reading-Dif" "Math-B" "Math-G" "Math-Dif") label ///
															se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nobaselevels drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference)

		*************************
		**TABLE 8 ******
		************************* 
			global r3_non_cog agency_r3_b agency_r3_g agency_r3_dif 
			global r4_non_cog agency_r4_b agency_r4_g agency_r4_dif sef_r4_b sef_r4_g sef_r4_dif ses_pride_r4_b ses_pride_r4_g ses_pride_r4_dif 
			global r5_non_cog agency_r5_b agency_r5_g agency_r5_dif sef_r5_b sef_r5_g sef_r5_dif ses_pride_r5_b ses_pride_r5_g ses_pride_r5_dif
			
			*Round 3:
			esttab $r3_non_cog using "$outputs\table8_r3.rtf", replace alignment(cr) mtitle("Agency-B" "Agency-G" "Agency-Dif") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs ///
																			  drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference) nobaselevels
									
			*Round 4:
			esttab $r4_non_cog using "$outputs\table8_r4.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
										mtitle("Agency-B" "Agency-G" "Agency-Dif" "Self-efficacy-B" "Self-efficacy-G" "Self-efficacy-Dif" "Self-esteem-B" "Self-esteem-G" "Self-esteem-Dif")  ///
										drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference)
				
			*Round 5:			
			esttab $r5_non_cog using "$outputs\table8_r5.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
									 mtitle("Agency-B" "Agency-G" "Agency-Dif" "Self-efficacy-B" "Self-efficacy-G" "Self-efficacy-Dif" "Self-esteem-B" "Self-esteem-G" "Self-esteem-Dif")  ///
																		 drop(1.female_r1 _cons) rename(1.female_r1#1.treatment Difference)
																		  
	************************************
	**IV.II. URBAN *********************
	************************************
		estimates clear
		*Generating the matching weights:
		kmatch eb  treatment $covariates if filter_final==0,  comsup att over(urban_r1) wgen(w_eb_urban)  

		label var urban_r1 "Urban location"

		foreach var in $outcomes{
				eststo ols_`var': reg `var' i.urban_r1##i.treatment if filter_final==0 & w_eb_urban!=0,  vce(cluster clustid_r1)
				*eststo eb_`var':  reg `var' i.urban_r1##i.treatment [aw=w_eb_urban] if filter_final==0,  vce(cluster clustid_r1)
				eststo eb_`var': reg `var' i.urban_r1##i.treatment [aw=w_eb_urban] if filter_final==0,  vce(cluster clustid_r1)
				eststo km_eb_`var': kmatch eb  treatment $covariates  (`var') if filter_final==0,  comsup att over(urban_r1)
				}

		*************************
		**TABLE XXXXXX ******
		*************************		
			*Round 2:
			esttab $r2_cog using "$outputs_2\urban_round2_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "CDA" "CDA") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nobaselevels ///
																	drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" )
						
			*Round 3:
			esttab $r3_cog using "$outputs_2\urban_round3_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "EGRA" "EGRA" "Math" "Math") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs ///
																  nobaselevels drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" )
				
			*Round 4:
			esttab $r4_cog using "$outputs_2\urban_round4_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "Lang." "Lang." "Math" "Math") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs ///
																  nobaselevels drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" )
		
			*Round 5:
			esttab $r5_cog using "$outputs_2\urban_round5_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "Reading" "Reading" "Math" "Math") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
																 obs nobaselevels drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" )

		*************************
		**TABLE XXXXXX ******
		************************* 
			*Round 3:
			esttab $r3_non_cog using "$outputs_2\urban_round3_non-cog_alt.rtf", replace alignment(cr) mtitle("Agency" "Agency" "Pride" "Pride") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
																			  drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" ) ///
																			  nobaselevels obs
									
			*Round 4:
			esttab $r4_non_cog using "$outputs_2\urban_round4_non-cog_alt.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
																			    mtitle("Agency" "Agency" "Self-efficacy" "Self-efficacy" "Self-esteem" "Self-esteem")  ///
																				drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" ) 
				
			*Round 5:			
			esttab $r5_non_cog using "$outputs_2\urban_round5_non-cog.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
																			mtitle("Agency" "Agency" "Self-efficacy" "Self-efficacy" "Self-esteem" "Self-esteem")  ///
																			drop(1.urban_r1 1.treatment _cons) rename(1.urban_r1#1.treatment Difference main:ATT "Rural" 1:ATT "Urban" )
 
	************************************
	**IV.III. OLDER SIBLING ************
	************************************
		*Generating variable to identify the cases: children who had older siblings at home during most of the rounds
		rename (all_sibling_old_r1 all_sibling_old_r2 all_sibling_old_r3) (all_sibling_old_hh_r1 all_sibling_old_hh_r2 all_sibling_old_hh_r3)
		egen dropme=rowmean(all_sibling_old_hh_r1 all_sibling_old_hh_r2 all_sibling_old_hh_r3 all_sibling_old_hh_r4 all_sibling_old_hh_r5)
		gen sibling_old=cond(dropme>0.5,1,0)
		drop dropme
		estimates clear
		
		/*forvalues i=2(1)5{
			kmatch eb  treatment $covariates if filter_final==0,  comsup att over(sibling_old) wgen(w_eb_sibling_old) 
			label var all_sibling_old_hh_r`i' "Older sibling"
			}*/
		
		*Generating the matching weights:
		kmatch eb  treatment $covariates if filter_final==0,  comsup att over(sibling_old) wgen(w_eb_sibling_old)  
		label var sibling_old "Older sibling"

		foreach var in $outcomes{
			eststo ols_`var': reg `var' i.sibling_old##i.treatment if filter_final==0 & w_eb_sibling_old!=0,  vce(cluster clustid_r1)
			*eststo eb_`var':  reg `var' i.sibling_old##i.treatment [aw=w_eb_sibling_old] if filter_final==0,  vce(cluster clustid_r1)
			eststo eb_`var': reg `var' i.sibling_old##i.treatment [aw=w_eb_sibling_old] if filter_final==0,  vce(cluster clustid_r1)
			eststo km_eb_`var': kmatch eb  treatment $covariates  (`var') if filter_final==0,  comsup att over(sibling_old)
			}

		*************************
		**TABLE XXXXXX ******
		*************************		
			*Round 2:
			esttab $r2_cog using "$outputs_2\sibling_old_round2_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "CDA" "CDA") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs nobaselevels ///
																		  rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling" ) ///
																		  drop(1.sibling_old 1.treatment _cons) 
						
			*Round 3:
			esttab $r3_cog using "$outputs_2\sibling_old_round3_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "EGRA" "EGRA" "Math" "Math") label se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
																		  obs nobaselevels rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling" ) ///
																		  drop(1.sibling_old 1.treatment _cons) 
				
			*Round 4:
			esttab $r4_cog using "$outputs_2\sibling_old_round4_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "Lang." "Lang." "Math" "Math") se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs ///
																		  label nobaselevels rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling" ) ///
																		  drop(1.sibling_old 1.treatment _cons) 
		
			*Round 5:
			esttab $r5_cog using "$outputs_2\sibling_old_round5_alt.rtf", replace alignment(cr) mtitle("PPVT" "PPVT" "Reading" "Reading" "Math" "Math") se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
																		  label nobaselevels rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling") ///
																		  obs drop(1.sibling_old 1.treatment _cons) 
																		  
		*************************
		**TABLE XXXXXX ******
		************************* 
			*Round 3:
			esttab $r3_non_cog using "$outputs_2\sibling_old_round3_non-cog_alt.rtf", replace alignment(cr) mtitle("Agency" "Agency" "Pride" "Pride") se(%9.4f) star(* 0.1 ** .05 *** 0.01) ///
																					  rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling" ) ///
																					  nobaselevels obs label drop(1.sibling_old 1.treatment _cons) label obs 
																			  
									
			*Round 4:
			esttab $r4_non_cog using "$outputs_2\sibling_old_round4_non-cog_alt.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
																					  mtitle("Agency" "Agency" "Self-efficacy" "Self-efficacy" "Self-esteem" "Self-esteem")  ///
																					  rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling" ) ///
																					  drop(1.sibling_old 1.treatment _cons) label obs
				
			*Round 5:			
			esttab $r5_non_cog using "$outputs_2\sibling_old_round5_non-cog.rtf", replace alignment(cr) obs nobaselevels se(%9.4f) star(* 0.1 ** .05 *** 0.01) label  ///
																				  mtitle("Agency" "Agency" "Self-efficacy" "Self-efficacy" "Self-esteem" "Self-esteem")  ///
																				  rename(1.sibling_old#1.treatment Difference main:ATT "No-older-sibling" 1:ATT "Older-sibling") ///
