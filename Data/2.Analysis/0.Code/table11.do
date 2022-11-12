********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\1.Build\2.Aux"
global outputs		"$root\2.Analysis\3.Outputs\Tables"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************

use "$inputs\rounds_all.dta" , clear

	global cov_hh  		urban bad_event
	
	global cov_cg    	i.mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
						moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
						beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
								
	global m_income 	r_t_num_r1 wi_r1 r_t_num_r2 e_s_cur_r2 exp_pc_r2_log wi_r2  r_t_num_r3 e_s_cur_r3 exp_pc_r3_log wi_r3 r_t_num_r4 e_s_cur_r4 exp_pc_r4_log wi_r4 e_s_cur_r5 ///
						exp_pc_r5_log wi_r5 	
	
	global m_time 		m_works_r1 m_jobs_r1 a_hh_r1 a_hh2_r1 m_works_r2 m_jobs_r2 a_hh_r2 a_hh2_r2 m_works_r3 m_jobs_r3 a_hh_r3 a_hh2_r3 nhlp_study_r3 m_works_r4 m_jobs_r4 a_hh_r4 a_hh2_r4 ///
						nhlp_study_r4 a_hh_r5 a_hh2_r5 
	
	global covariates	"$cov_hh $cov_cg i.pecodpt1_r1"
	
	global m_cg 		sw_cg_curr_r2 ag_cg_r2 pride_cg_r2 sw_cg_curr_r3 ag_cg_r3  pride_cg_r3 sw_cg_curr_r4 ag_cg_r4 pride_cg_r4 pa_r_r4 sw_cg_curr_r5 ag_cg_r5 pride_cg_r5 pa_r_r5 ///
						exp_a_earn_r2 exp_a_edu_r2 exp_a_leave_r2  exp_a_earn_r4 exp_a_edu_r4 exp_a_leave_r4 exp_a_earn_r5 exp_a_edu_r5 exp_a_leave_r5  exp_a_indp_r2 exp_a_indp_r4 exp_a_indp_r5
	
	
	global m_all  		"m_income m_time m_cg"
	
	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)

***************************************************************************************************
**III. IMPACT: MECHANISMS**************************************************************************
***************************************************************************************************														

	foreach mechanism in $m_all{
		foreach var in $`mechanism'{
			gen `var'_copy=`var'
			sum `var' if treatment==0 & filter_final==0
			replace `var'=(`var'_copy-r(mean))/r(sd)
			eststo `var': kmatch eb treatment  $covariates (`var') if filter_final==0, targets(2) att vce(cluster clustid_r1)
			}
		}
		
	*************************
	**TABLE XXXXXX ******
	*************************					
		global m_inc_r1 r_t_num_r1 wi_r1 
		global m_inc_r2 r_t_num_r2 e_s_cur_r2 exp_pc_r2_log wi_r2  
		global m_inc_r3 r_t_num_r3 e_s_cur_r3 exp_pc_r3_log wi_r3 
		global m_inc_r4 r_t_num_r4 e_s_cur_r4 exp_pc_r4_log wi_r4
		global m_inc_r5 e_s_cur_r5 exp_pc_r5_log wi_r5 
		
		global m_t_r1 m_works_r1 m_jobs_r1 a_hh2_r1 
		global m_t_r2 m_works_r2 m_jobs_r2 a_hh2_r2
		global m_t_r3 m_works_r3 m_jobs_r3 a_hh2_r3  
		global m_t_r4 m_works_r4 m_jobs_r4 a_hh2_r4 
		global m_t_r5 a_hh2_r5 
		
		global m_cg_r2 sw_cg_curr_r2 ag_cg_r2 pride_cg_r2 
		global m_cg_r3 sw_cg_curr_r3 ag_cg_r3 pride_cg_r3 
		global m_cg_r4 sw_cg_curr_r4 ag_cg_r4 pride_cg_r4 exp_a_earn_r4 exp_a_edu_r4 exp_a_indp_r4 pa_r_r4  
		global m_cg_r5 sw_cg_curr_r5 ag_cg_r5 pride_cg_r5 exp_a_earn_r5 exp_a_edu_r5 exp_a_indp_r5 exp_a_leave_r5 pa_r_r5 
		
		*Income:
			esttab $m_inc_r1 using "$outputs\table11-m_inc_round1.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("RT" "WI")  b(a3)
			
			esttab $m_inc_r2 using "$outputs\table11-m_inc_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("RT" "ES" "Exp" "WI") b(a3)	
				
			esttab $m_inc_r3 using "$outputs\table11-m_inc_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("RT" "ES" "Exp" "WI") b(a3)

			esttab $m_inc_r4 using "$outputs\table11-m_inc_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("RT" "ES" "Exp" "WI" ) b(a3)
				
			esttab $m_inc_r5 using "$outputs\table11-m_inc_round5.rtf", replace alignment(cr) label se(%9.5f) star(* 0.1 ** .05 *** 0.01) obs mtitle("ES" "Exp" "WI") b(a3)
							
		*Time:					
			esttab $m_t_r1 using "$outputs\table11-m_t_round1.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("Mom works" "Mom # jobs" "Mother alone") b(a3)
				
			esttab $m_t_r2 using "$outputs\table11-m_t_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("Mom works" "Mom # jobs" "Mother alone") b(a3)

			esttab $m_t_r3 using "$outputs\table11-m_t_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) ///
																mtitle("Mom works" "Mom # jobs" "Mother alone" " No help to study") 
			
			esttab $m_t_r4 using "$outputs\table11-m_t_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) ///
																mtitle("Mom works" "Mom # jobs" "Mother alone" "No help to study") 
			
			esttab $m_t_r5  using "$outputs\table11-m_t_round5.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("Mother alone") b(a3)
					
		*Caregiver:		
			esttab $m_cg_r2 using "$outputs\table11-m_cg-psy_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) ///
																	mtitle("SW" "Agency" "Pride") 
												
			esttab $m_cg_r3 using "$outputs\table11-m_cg-psy_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("SW" "Agency" "Pride") b(a3)

			esttab $m_cg_r4 using "$outputs\table11-m_cg-psy_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) ///
																mtitle("SW" "Agency" "Pride" "Expec. Age (earn)" "Expec. Age (leave edu.)" "Expec. Age (fin. indp.)" "Relationship with child") 
						
			esttab $m_cg_r5 using "$outputs\table11-m_cg-psy_round5.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) ///
										mtitle("SW" "Agency" "Pride" "Expec. Age (earn)" "Expec. Age (leave edu.)" "Expec. Age (fin. indp.)" "Expec. Age (leave HH)" "Relationship with child") 
