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
	
	global m_use_time	child_hrs_hh_r2 child_hrs_work_r2 child_hrs_study_r2 child_hrs_play_r2 child_hrs_hh_r3 child_hrs_work_r3 ///
						child_hrs_study_r3 child_hrs_play_r3 child_hrs_hh_r4 child_hrs_work_r4 child_hrs_study_r4 child_hrs_play_r4 ///
						child_hrs_hh_r5 child_hrs_work_r5 child_hrs_study_r5 child_hrs_play_r5
	
	
	global m_all  		"m_income m_time m_cg m_use_time"
	
	gen dropme=cond(treatment==0 & seedad_r2==1,0,cond(treatment==1 & seedad_r2!=1 & seedad_r2!=.,1,.))
	
	rename (treatment dropme) (treatment2 treatment)

***************************************************************************************************
**III. IMPACT: MECHANISMS**************************************************************************
***************************************************************************************************														
	
	label var treatment "Father absence"
	label var female_r1 "Female"

	foreach mechanism in $m_all{
		foreach var in $`mechanism'{
			gen `var'_copy=`var'
			sum `var' if treatment==0 & filter_final==0
			replace `var'=(`var'_copy-r(mean))/r(sd)
			eststo km_eb_`var': kmatch eb  treatment $covariates  (`var') if filter_final==0,  att over(female_r1) targets(2) wgen(w_eb_sex)
			eststo eb_`var': reg `var' i.female_r1##i.treatment [aw=w_eb_sex] if filter_final==0,  vce(cluster clustid_r1)
			drop w_eb_sex
			}
		}
		

	*************************
	**TABLE XXXXXX ******
	*************************					
		global m_inc_r1 	eb_r_t_num_r1  km_eb_r_t_num_r1  eb_wi_r1  km_eb_wi_r1 
		global m_inc_r2 	eb_r_t_num_r2  km_eb_r_t_num_r2  eb_wi_r2  km_eb_wi_r2  eb_exp_pc_r2_log  km_eb_exp_pc_r2_log  eb_e_s_cur_r2  km_eb_e_s_cur_r2      
		global m_inc_r3 	eb_r_t_num_r3  km_eb_r_t_num_r3  eb_wi_r3  km_eb_wi_r3  eb_exp_pc_r3_log  km_eb_exp_pc_r3_log  eb_e_s_cur_r3  km_eb_e_s_cur_r3     
		global m_inc_r4 	eb_r_t_num_r4  km_eb_r_t_num_r4  eb_wi_r4  km_eb_wi_r4  eb_exp_pc_r4_log  km_eb_exp_pc_r4_log  eb_e_s_cur_r4  km_eb_e_s_cur_r4    
		global m_inc_r5 							  		 eb_wi_r5  km_eb_wi_r5  eb_exp_pc_r5_log  km_eb_exp_pc_r5_log  eb_e_s_cur_r5  km_eb_e_s_cur_r5    
		
		global m_t_r1  eb_m_works_r1  km_eb_m_works_r1  eb_m_jobs_r1  km_eb_m_jobs_r1  eb_a_hh2_r1  km_eb_a_hh2_r1 
		global m_t_r2  eb_m_works_r2  km_eb_m_works_r2  eb_m_jobs_r2  km_eb_m_jobs_r2  eb_a_hh2_r2  km_eb_a_hh2_r2
		global m_t_r3  eb_m_works_r3  km_eb_m_works_r3  eb_m_jobs_r3  km_eb_m_jobs_r3  eb_a_hh2_r3  km_eb_a_hh2_r3 
		global m_t_r4  eb_m_works_r4  km_eb_m_works_r4  eb_m_jobs_r4  km_eb_m_jobs_r4  eb_a_hh2_r4  km_eb_a_hh2_r4
		global m_t_r5 																   eb_a_hh2_r5  km_eb_a_hh2_r5
		
		global m_cg_r2  eb_sw_cg_curr_r2  km_eb_sw_cg_curr_r2  eb_ag_cg_r2  km_eb_ag_cg_r2  eb_pride_cg_r2  km_eb_pride_cg_r2 
		global m_cg_r3  eb_sw_cg_curr_r3  km_eb_sw_cg_curr_r3  eb_ag_cg_r3  km_eb_ag_cg_r3  eb_pride_cg_r3  km_eb_pride_cg_r3 
		global m_cg_r4  eb_sw_cg_curr_r4  km_eb_sw_cg_curr_r4  eb_ag_cg_r4  km_eb_ag_cg_r4  eb_pride_cg_r4  km_eb_pride_cg_r4  eb_exp_a_earn_r4  km_eb_exp_a_earn_r4  eb_exp_a_edu_r4 ///
					    km_eb_exp_a_edu_r4  eb_exp_a_indp_r4  km_eb_exp_a_indp_r4  eb_pa_r_r4 km_eb_pa_r_r4 
		
		global m_cg_r5  eb_sw_cg_curr_r5  km_eb_sw_cg_curr_r5  eb_ag_cg_r5  km_eb_ag_cg_r5  eb_pride_cg_r5  km_eb_pride_cg_r5  eb_exp_a_earn_r5  km_eb_exp_a_earn_r5  eb_exp_a_edu_r5 ///
					    km_eb_exp_a_edu_r5  eb_exp_a_indp_r5  km_eb_exp_a_indp_r5  eb_exp_a_leave_r5 km_eb_exp_a_leave_r5  eb_pa_r_r5 km_eb_pa_r_r5 
		
		global m_u_t_r2  eb_child_hrs_hh_r2 km_eb_child_hrs_hh_r2 eb_child_hrs_work_r2 km_eb_child_hrs_work_r2 eb_child_hrs_study_r2 km_eb_child_hrs_study_r2 eb_child_hrs_play_r2 ///
						 km_eb_child_hrs_play_r2 
		
		global m_u_t_r3  eb_child_hrs_hh_r3 km_eb_child_hrs_hh_r3 eb_child_hrs_work_r3 km_eb_child_hrs_work_r3 eb_child_hrs_study_r3 km_eb_child_hrs_study_r3 eb_child_hrs_play_r3 ///
						 km_eb_child_hrs_play_r3 
						
		global m_u_t_r4  eb_child_hrs_hh_r3 km_eb_child_hrs_hh_r4 eb_child_hrs_work_r4 km_eb_child_hrs_work_r4 eb_child_hrs_study_r4 km_eb_child_hrs_study_r4 eb_child_hrs_play_r4 ///
						 km_eb_child_hrs_play_r4 
						
		global m_u_t_r5  eb_child_hrs_hh_r5 km_eb_child_hrs_hh_r5 eb_child_hrs_work_r5 km_eb_child_hrs_work_r5 eb_child_hrs_study_r5 km_eb_child_hrs_study_r5 eb_child_hrs_play_r5 ///
						 km_eb_child_hrs_play_r5 
		
		*Income:
			esttab $m_inc_r1 using "$outputs\tabled1_round1.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("RT" "RT" "WI" "WI")  b(a3) nobaselevels ///
																		drop(1.female_r1 1.treatment _cons) rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female" )
		
			esttab $m_inc_r2 using "$outputs\tabled1_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) mtitle("RT" "RT" "WI" "WI" "Exp" "Exp" "ES" "ES"  ) ///
																		rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female")  drop(1.female_r1 1.treatment _cons) ///
																		obs b(a3) nobaselevels
				
			esttab $m_inc_r3 using "$outputs\tabled1_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) mtitle("RT" "RT" "WI" "WI" "Exp" "Exp" "ES" "ES" ) ///
																		rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female")  drop(1.female_r1 1.treatment _cons) ///
																		obs b(a3) nobaselevels
																		
			esttab $m_inc_r4 using "$outputs\tabled1_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) mtitle("RT" "RT" "WI" "WI" "Exp" "Exp" "ES" "ES" ) ///
																		rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female")  drop(1.female_r1 1.treatment _cons) ///
																		obs b(a3) nobaselevels
																		
			esttab $m_inc_r5 using "$outputs\tabled1_round5.rtf", replace alignment(cr) label se(%9.5f) star(* 0.1 ** .05 *** 0.01) mtitle("WI" "WI" "Exp" "Exp" "ES" "ES" ) ///
																		rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") drop(1.female_r1 1.treatment _cons) ///
																		obs b(a3) nobaselevels
							
		*Time:					
			esttab $m_t_r1 using "$outputs\tabled2_round1.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) nobaselevels  ///
																	mtitle("Mom works" "Mom works" "Mom # jobs" "Mom # jobs" "Mother alone"  "Mother alone") ///
																	rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") ///
																	 drop(1.female_r1 1.treatment _cons)
				
			esttab $m_t_r2 using "$outputs\tabled2_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) nobaselevels ///
																	mtitle("Mom works" "Mom works" "Mom # jobs" "Mom # jobs" "Mother alone"  "Mother alone") ///
																	rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") ///
																	 drop(1.female_r1 1.treatment _cons)

			esttab $m_t_r3 using "$outputs\tabled2_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) nobaselevels   ///
																	mtitle("Mom works" "Mom works" "Mom # jobs" "Mom # jobs" "Mother alone"  "Mother alone") ///
																	rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") ///
																	 drop(1.female_r1 1.treatment _cons)
			
			esttab $m_t_r4 using "$outputs\tabled2_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) nobaselevels   ///
																	mtitle("Mom works" "Mom works" "Mom # jobs" "Mom # jobs" "Mother alone"  "Mother alone") ///
																	rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") ///
																	 drop(1.female_r1 1.treatment _cons)
			
			esttab $m_t_r5  using "$outputs\tabled2_round5.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs mtitle("Mother alone" "Mother alone") b(a3) ///
																	 drop(1.female_r1 1.treatment _cons) rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female")	///
																	nobaselevels 
					
		*Caregiver:		
			esttab $m_cg_r2 using "$outputs\tabled3_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) drop(1.female_r1 1.treatment _cons) ///
																nobaselevels rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female" ) ///
																mtitle("SW" "SW" "Agency" "Agency" "Pride"  "Pride")
																		
												
			esttab $m_cg_r3 using "$outputs\tabled3_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) drop(1.female_r1 1.treatment _cons) ///
																 nobaselevels rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female" ) ///
																 mtitle("SW" "SW" "Agency" "Agency" "Pride"  "Pride")

			esttab $m_cg_r4 using "$outputs\tabled3_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) drop(1.female_r1 1.treatment _cons) ///
			mtitle("SW" "SW" "Agency" "Agency" "Pride"  "Pride" "Expec. Age (earn)" "Expec. Age (earn)" "Expec. Age (leave edu.)" "Expec. Age (leave edu.)"  "Expec. Age (fin. indp.)" "Expec. Age (fin. indp.)" "Relationship with child" "Relationship with child") ///
			nobaselevels rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female" )  
															 
			esttab $m_cg_r5 using "$outputs\tabled3_round5.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) obs b(a3) drop(1.female_r1 1.treatment _cons) ///
									mtitle("SW" "SW" "Agency" "Agency" "Pride"  "Pride" "Expec. Age (earn)" "Expec. Age (earn)" "Expec. Age (leave edu.)" "Expec. Age (leave edu.)"  "Expec. Age (fin. indp.)" "Expec. Age (fin. indp.)" "Expec. Age (leave HH)"  "Expec. Age (leave HH)"  "Relationship with child" "Relationship with child") ///
									nobaselevels rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female" ) 
									
		*Use of time:
			esttab $m_u_t_r2 using "$outputs\tabled4_round2.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) drop(1.female_r1 1.treatment _cons) nobaselevels obs b(a3) ///
														mtitle("HH activities" "HH activities" "Work" "Work" "Homework/study" "Homework/study" "Leisure" "Leisure") ///
														rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") 
														
			esttab $m_u_t_r3 using "$outputs\tabled4_round3.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) drop(1.female_r1 1.treatment _cons) nobaselevels obs b(a3) ///
														mtitle("HH activities" "HH activities" "Work" "Work" "Homework/study" "Homework/study" "Leisure" "Leisure") ///
														rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") 
														
			esttab $m_u_t_r4 using "$outputs\tabled4_round4.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) drop(1.female_r1 1.treatment _cons) nobaselevels obs b(a3) ///
														mtitle("HH activities" "HH activities" "Work" "Work" "Homework/study" "Homework/study" "Leisure" "Leisure") ///
														rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") 
														
			esttab $m_u_t_r5 using "$outputs\tabled4_round5.rtf", replace alignment(cr) label se(%9.4f) star(* 0.1 ** .05 *** 0.01) drop(1.female_r1 1.treatment _cons) nobaselevels obs b(a3) ///
														mtitle("HH activities" "HH activities" "Work" "Work" "Homework/study" "Homework/study" "Leisure" "Leisure") ///
														rename(1.female_r1#1.treatment Difference main:ATT "Male" 1:ATT "Female") 