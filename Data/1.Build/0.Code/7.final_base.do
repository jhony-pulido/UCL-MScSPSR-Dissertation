
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\3.Outputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

use "$inputs\round1.dta",clear

gen round1=1
order treatment round1, after(childid) 
*Merge all rounds
forvalues i=2(1)5{
	merge 1:1 childid using "$outputs\round`i'.dta"
	gen round`i'=cond(_merge==3,1,0)
	order round`i', after(_merge)
	drop _merge
	}

merge 1:1 childid using "$aux/father_information.dta"
drop _merge 

egen survey_participation=rowtotal(round*)
gen attrition=cond(treatment==.,.,cond(survey_participation==5,0,1))

order childid childcode treatment survey_participation attrition, first

global round1  placeid_r1 comnro_r1 clustid_r1 female_r1 agemon_r1 momlive_r1 daddead_r1 seedad_r1 pecodpt1_r1 own_house_r1 cov_hh_charac_r1 urban_r1 lima_r1 antenatal_visit_r1 ///
			   birth_home birth_premature birth_no_prof birth_no_rel birth_alone cg_mother_not_spanish read_spanish_dif timelive ///
			   child_wanted_r1 poor_pregnancy_r1 sex_hoped_r1 mother_age_r1 mother_edu_r1 age_pregnancy_r1 teen_pregnancy_r1 not_spanish_r1 dead_children_r1 abortion_r1 catholic_r1 no_boy_r1 ///
			   child_not_first_r1 mother_hh_r1 bio_father_hh_r1 step_father_hh_r1 grandparents_hh_r1 alone_hh_r1 blood_sibling_r1 half_sibling_r1 all_sibling_r1 blood_sibling_old_r1 ///
			   blood_sibling_young_r1 half_sibling_old_r1 half_sibling_young_r1 all_sibling_old_r1 all_sibling_young_r1 violence_gp_mom_r1 violence_gp_dad_r1 beaten_mom_r1 beaten_dad_r1 ///
			   alone_hh2_r1 father_age_r1 grade_original_r1 still_r1 grade_r1 father_edu_r1 father_lives_in_hh_r1 bad_event_r1 mom_jobs_r1 payment_r1 mom_work_days1_r1 ///
			   mom_work_days2_r1 mom_work_days3_r1 mom_work_days4_r1 months_r1 mom_works_r1 cd_r1 hq_r1 sv_r1 wi_r1 remit_transf_r1 remit_transf_num_r1 remit_rest_r1 remit_rest_num_r1 ///
			   covariates_com_r1 pop_r1 env_problems_r1 community_crime_r1 barrio_r1 land_invasion_r1 quechua_r1 toilets_r1 water_r1 rubbish_r1 roads_paved_r1 pub_transp_r1 social_capital_r1 ///
			   movwork_r1 sufood_r1 litwork_r1 nofood_r1 prog_food_r1 prog_health_r1 prog_infrast_r1 prog_produc_r1 prog_proper_r1 prog_microcred_r1 prog_work_r1 police_r1 electricity_r1 ///
			   drink_water_r1 park_r1 pharmacy_dist_r1 pub_health_dist_r1
order $round1, after(round1)

global round2  situac_r2 attrition_r2 agemon house_moved_r2 house_months_r2 urban_r2 cov_hh_charac_r2 father_alive_r2 mother_alive_r2 seedad_r2 seemum_r2 carelive_r2 moves_pregnancy_r2 ///
			   moves_before_r2 moves_before_minor_r2 moves_cohab_preg_r2 moves_cohab_bef_r2 moves_cohab_minor_r2 moves_separ_preg_r2 moves_separ_bef_r2 moves_separ_minor_r2 ///
			   moves_viole_preg_r2 moves_viole_bef_r2 moves_viole_minor_r2 moves_disp_preg_r2 moves_disp_bef_r2 moves_disp_minor_r2 moves_pregnancy2_r2 moves_before2_r2 moves_minor2_r2 ///
			   moves_missing_r2 no_moves_before_r2 moves_pregnancy_dum_r2 moves_before_dum_r2 moves_before_minor_dum_r2 moves_cohab_preg_dum_r2 moves_cohab_bef_dum_r2 ///
			   moves_cohab_minor_dum_r2 moves_separ_preg_dum_r2 moves_separ_bef_dum_r2 moves_separ_minor_dum_r2 moves_viole_preg_dum_r2 moves_viole_bef_dum_r2 moves_viole_minor_dum_r2 ///
			   moves_disp_preg_dum_r2 moves_disp_bef_dum_r2 moves_disp_minor_dum_r2 moves_pregnancy2_dum_r2 moves_before2_dum_r2 moves_minor2_dum_r2 moves_missing_dum_r2 ///
			   no_moves_before_dum_r2 bornbef_r2 bornaft_r2 enter_hh_r2 want_hh_out_r2 want_hh_out_reason_r2 mother_hh_r2 bio_father_hh_r2 step_father_hh_r2 grandparents_hh_r2 alone_hh_r2 ///
			   alone_hh2_r3 blood_sibling_r2 half_sibling_r2 all_sibling_r2 blood_sibling_old_r2 blood_sibling_young_r2 half_sibling_old_r2 half_sibling_young_r2 all_sibling_old_r2 ///
			   all_sibling_young_r2 mom_some_college_r2 father_some_college_r2  father_age_r2 grade_original_r2 still_r2 grade_r2 father_edu_r2 father_lives_in_hh_r2 hhsize_r2 numroom_r2 ///
			   mom_works_r2 mom_jobs_r2 mom_works_total_r2 mom_works_inc_month_r2 mom_works_inc_month_log_r2 ///
			   mom_decisions_resources_r2 mom_decisions_main_prod_r2 mom_decisions_prod_r2 mom_earnings_prod_r2 mom_earnings_main_prod_r2 carer3y_father_r2 carer3y_grandparents_r2 ///
			   carer3y_num_r2 cov_hh_econ_r2 external_resources_r2 remit_transf_num remit_rest_num remit_transf_last remit_rest_last_r2 remit_transf_r2 remit_alimony_r2 remit_alimony_num_r2 ///
			   remit_alimony_amount_r2 econ_status_curr_r2 econ_status_past_r2 econ_status_struggle_r2 econ_status_poor_r2 econ_status_worse_r2 econ_enter_poverty_r2 econ_exit_poverty_r2 ///
			   econ_stay_poverty_r2 food_hh_problems_r2 food_security_child_r2 food_security_caregiver_r2 donfood_r2 food_shortage_r2 foodexp_rpc_r2 nfoodexp_rpc_r2 exp_pc_r2_log hq_r2 ///
			   cd_r2 sv_r2 wi_r2 debt_r2 raise_r2 pemxbank_r2 pemxngo_r2 pemxfam_r2 peloan_r2 invest_r2 ownland_r2 cov_education_r2 exp_private_classes_r2 exp_books_r2 edu_child_center_r2 ///
			   edu_presch_r2  edu_presch_private_r2 edu_presch_duration_r2 edu_private_expect_r2 edu_good_qual_expect_r2 edu_trav_no_adult_r2 cov_health_r2 exp_med_cons_r2 exp_medicines_r2  ///
			   health_inj_ill_r2 health_longterm_r2 health_insurance_r2 health_vaccines_r2 zwfa_r2 zhfa_r2 fwfa_r2 fhfa_r2 cov_time_use_r2 child_hrs_sleep_r2 child_hrs_care_r2 ///
			   child_hrs_chores_r2 child_hrs_work_r2 child_hrs_school_r2 child_hrs_study_r2 child_hrs_play_r2 cov_hh_risk_r2 exp_tobacco_r2 exp_tv_r2 exp_party_r2 hh_divorce_r2 hh_drunk_r2  ///
			   hh_agressive_r2 hh_smokers_r2 mother_pregnancies_r2 cov_caregiver_r2 percp_child_edu_cg_r2 subjective_wellbeing_cg_curr_r2 subjective_wellbeing_cg_fut_r2 agency_cg_r2 ///
			   pride_cg_r2 inclusion_cg_r2 percp_locus_child_prog_cg_r2 home_importance_cg_r2 instrumental_assistance_cg_r2 psychological_appreciation_cg_r2 rewarding_interaction_cg_r2 ///
			   expec_support_child_r2 expect_age_child_r2 expect_age_earn_r2 expect_age_education_r2 expect_age_indp_r2 expect_age_leave_r2 expect_age_marry_r2 expect_age_parent_r2 ///
			   cov_area_r2 crime_victim_r2 area_qual_r2 access_selfhelp_r2 access_welfare_r2 access_female_gr_r2 cov_others_r2 support_index_r2 ///
			   info_child_sought_r2 info_child_acess_r2 info_income_r2 info_income_sought_r2 support_material_r2 support_less_r2

order $round2, after(round2)

global round3 attrition_r3 house_moved_r3 urban_r3 cov_hh_charac_r3 mumal_r3 seemum_r3 father_alive_r3  father_freq_r3 whyndl_r3 lvmron_r3 mother_hh_r3 bio_father_hh_r3 step_father_hh_r3 ///
			  grandparents_hh_r3 alone_hh_r3 mom_works_r3 mom_jobs_r3 mom_works_months_r3 mom_works_inc_month_r3 mom_works_inc_total_r3 mom_works_inc_month_log_r3 ///
			  mom_works_inc_total_log_r3 blood_sibling_r3 half_sibling_r3 all_sibling_r3 blood_sibling_old_r3 blood_sibling_young_r3 half_sibling_old_r3 half_sibling_young_r3 ///
			  all_sibling_old_r3 all_sibling_young_r3 alone_hh2_r3 rellivr3_r3 move_separ_dum_r3 move_cohab_dum_r3 move_disp_dum_r3 move_work_dum_r3 move_separ_mom_dum_r3 ///
			  move_cohab_mom_dum_r3 move_disp_mom_dum_r3 father_age_r3 grade_original_r3 still_r3 grade_r3 father_edu_r3 father_lives_in_hh_r3 move_work_mom_dum_r3 cov_hh_econ_r3 ///
			  remit_transf_num_r3 remit_rest_num_r3 remit_transf_last_r3  ///
			  remit_rest_last_r3 remit_transf_r3 remit_alimony_r3 remit_alimony_num_r3 remit_alimony_amount_r3 rernin_r3 econ_enter_poverty_r3 econ_exit_poverty_r3 econ_status_curr_r3 ///
			  econ_status_past_r3 econ_status_poor_r3 econ_status_struggle_r3 econ_status_worse_r3 econ_stay_poverty_r3 foodexp_rpc_r3 nfoodexp_rpc_r3 exp_pc_r3_log hq_r3 cd_r3 sv_r3 wi_r3 ///
			  cov_time_use_r3 child_hrs_care_r3 child_hrs_chores_r3 child_hrs_play_r3 child_hrs_school_r3 child_hrs_selfcare_r3 child_hrs_sleep_r3 child_hrs_study_r3 child_hrs_work_r3  ///
			  cov_hh_risk_r3 hh_agressive_r3 hh_drunk_r3 hh_divorce_r3 chlwrkr3_r3 help_child_abuse_r3 mother_pregnancies_r3 cov_caregiver_r3 time_school_r3 know_school_r3 ///
			  subjective_wellbeing_cg_curr_r3 subjective_wellbeing_cg_fut_r3 agency_cg_r3 inclusion_cg_r3 pride_cg_r3 hlp_study_old_sibling_r3 hlp_study_mom_r3 hlp_study_father_r3 ///
			  hlp_study_nobody_r3 hlp_study_nobody2_r3 cov_health_r3 zbfa_r3_r3 zhfa_r3_r3 zwfa_r3_r3 fwfa_r3_r3 fhfa_r3_r3 fbfa_r3_r3 incgmer3_recode_r3 hrdtlkr3_recode_r3	  
order $round3, after(round3)

global round4 attrition_r4 house_moved_r4 cov_hh_charac_r4 seemum_r4 seedad_r4 moves_r4 moves_separ_r4 moves_separ_dum_r4 hhsize_r4 blood_sibling_r4 half_sibling_r4 all_sibling_r4 ///
			  blood_sibling_old_r4 blood_sibling_young_r4 half_sibling_old_r4 half_sibling_young_r4 all_sibling_old_r4 all_sibling_young_r4 mother_hh_r4 bio_father_hh_r4 step_father_hh_r4 ///
			  grandparents_hh_r4 alone_hh_r4 blood_sibling_old_hh_r4 blood_sibling_young_hh_r4 half_sibling_old_hh_r4 half_sibling_young_hh_r4 all_sibling_old_hh_r4 all_sibling_young_hh_r4 ///
			  father_age_r4 grade_original_r4 still_r4 grade_r4 father_edu_r4 father_lives_in_hh_r4 college_aspiration_r4 rptgrdr4_r4 mom_works_r4 mom_jobs_r4 cov_hh_econ_r4 debt_r4 ///
			  remit_transf_num_r4 remit_rest_num_r4 remit_transf_last_r4 remit_rest_last_r4 remit_transf_r4 ///
			  econ_enter_poverty_r4 econ_exit_poverty_r4 econ_status_curr_r4 econ_status_past_r4 econ_status_poor_r4 econ_status_struggle_r4 econ_status_worse_r4 econ_stay_poverty_r4 ///
			  econ_status_curr_child_r4 econ_status_past_child_r4 econ_status_struggle_child_r4 econ_status_poor_child_r4 econ_status_worse_child_r4 econ_enter_poverty_child_r4 ///
			  econ_exit_poverty_child_r4 econ_stay_poverty_child_r4 exp_pc_r4_log hq_r4 cd_r4 sv_r4 wi_r4 cov_time_use_r4 child_hrs_care_r4 child_hrs_chores_r4 child_hrs_play_r4 ///
			  child_hrs_school_r4 child_hrs_sleep_r4 child_hrs_study_r4 child_hrs_work_r4 cov_hh_risk_r4 hh_divorce_r4 hh_divorce_num_r4 cov_caregiver_r4 know_school_r4 ask_school_r4 ///
			  subjective_wellbeing_cg_curr_r4 subjective_wellbeing_cg_fut_r4  agency_cg_r4 pride_cg_r4 hlp_study_old_sibling_r4 hlp_study_mom_r4 hlp_study_father_r4 hlp_study_nobody_r4 ///
			  hlp_study_nobody2_r4 hlp_study_days_r4 support_ppl_cg_r4 expec_support_child_r4 expect_age_child_r4 expect_age_earn_r4 expect_age_education_r4 expect_age_indp_r4 ///
			  expect_age_leave_r4 expect_age_marry_r4 expect_age_parent_r4 cry_cg_r4 cg_college_expectations_r4 cov_health_r4 zbfa_r4 zhfa_r4 fhfa_r4 fbfa_r4 bmi_r4 test_lang_r4
order $round4, after(round4)

global round5  house_moved_r5 cov_hh_charac_r5 seemum_r5 seedad_r5 moves_r5 moves_separ_r5 moves_separ_dum_r5 hhsize_r5 blood_sibling_r5 half_sibling_r5 all_sibling_r5 blood_sibling_old_r5 ///
			   blood_sibling_young_r5 half_sibling_old_r5 half_sibling_young_r5 all_sibling_old_r5 all_sibling_young_r5 mother_hh_r5 bio_father_hh_r5 step_father_hh_r5 grandparents_hh_r5 ///
			   alone_hh_r5 alone_hh2_r5 blood_sibling_old_hh_r5 blood_sibling_young_hh_r5 half_sibling_old_hh_r5 half_sibling_young_hh_r5 all_sibling_old_hh_r5 all_sibling_young_hh_r5 ///
			   father_age_r5  grade_original_r5 still_r5 grade_r5 father_edu_r5 father_lives_in_hh_r5 cov_hh_econ_r5 ///
			   remit_transf_num_r5 remit_rest_num_r5 remit_transf_last_r5 remit_rest_last_r5 remit_transf_r5 econ_enter_poverty_r5 econ_exit_poverty_r5 ///
			   econ_status_curr_r5 econ_status_past_r5 econ_status_poor_r5 econ_status_struggle_r5 econ_status_worse_r5 econ_stay_poverty_r5 econ_status_curr_child_r5 ///
			   econ_status_past_child_r5 econ_status_struggle_child_r5 econ_status_poor_child_r5 econ_status_worse_child_r5 econ_enter_poverty_child_r5 econ_exit_poverty_child_r5 ///
			   econ_stay_poverty_child_r5 exp_pc_r5_log hq_r5 cd_r5 sv_r5 wi_r5 cov_time_use_r5 child_hrs_care_r5 child_hrs_chores_r5 child_hrs_play_r5 child_hrs_school_r5 ///
			   child_hrs_sleep_r5 child_hrs_study_r5 child_hrs_work_r5 cov_hh_risk_r5 hh_divorce_r5 hh_divorce_num_r5 cov_caregiver_r5 ask_school_r5 subjective_wellbeing_cg_curr_r5 ///
			   subjective_wellbeing_cg_fut_r5 agency_cg_r5 pride_cg_r5 hlp_study_old_sibling_r5 hlp_study_mom_r5 hlp_study_father_r5 hlp_study_nobody_r5 hlp_study_nobody2_r5 ///
			   hlp_study_days_r5 support_ppl_cg_r5 expec_support_child_r5 expect_age_child_r5 expect_age_earn_r5 expect_age_education_r5 expect_age_indp_r5 expect_age_leave_r5 ///
			   expect_age_marry_r5 expect_age_parent_r5 cov_health_r5 zbfa_r5 zhfa_r5 fhfa_r5 fbfa_r5 bmi_r5 childcode
order $round5, after(round5)

gen outcomes=.
order outcomes outcomes_r2 ppvt_stdscre_r2 ppvt_r2 cda_r2 outcomes_r3 ppvt_stdscre_r3 ppvtlang2_r3 egralang2_r3 mathlang2_r3 egra_r3 math_r3 egra_co_r3 math_co_r3 rppvt_co_r3 regra_co_r3 ///
	  rmath_co_r3 agency_r3 pride_r3 subjective_wellbeing_r3 peer_relationship_r3 bullied_r3 outcomes_r4 lang_r4 math_r4 ppvt_r4 ppvt_stdscre_r4 subjective_wellbeing_r4 cse_r4 cse2_r4 ///
	  peer_relationship_r4 self_efficacy_r4 self_esteem_r4 pride_r4 self_estem_pride_r4 parent_relationship_r4 agency_r4 bullied_r4 support_ppl_r4  outcomes_r5 math_r5 ppvt_r5 test_lang_r5 ///
	  ppvt_stdscre_r5 reading_r5 subjective_wellbeing_r5 cse_r5 cse2_r5 agency_r5 self_efficacy_r5 self_esteem_r5 pride_r5 self_estem_pride_r5 peer_relationship_r5 parent_relationship_r5 ///
	  gender_equality_r5 support_ppl_r5 , after(attrition)
	  
rename (urban_r1 bad_event_r1 mother_edu_r1 age_pregnancy_r1 teen_pregnancy_r1 not_spanish_r1 dead_children_r1 abortion_r1 no_boy_r1 child_not_first_r1 moves_cohab_preg_dum_r2 ///
			moves_cohab_bef_dum_r2 moves_separ_bef_dum_r2 moves_disp_bef_dum_r2 no_moves_before_dum_r2 antenatal_visit_r1 child_wanted_r1 poor_pregnancy_r1 beaten_mom_r1 )  ///
			(urban bad_event mother_edu age_pregnancy teen_pregnancy not_spanish dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
			moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy beaten_mom)

rename (self_efficacy_r4 self_esteem_r4 self_estem_pride_r4 self_efficacy_r5 self_esteem_r5 self_estem_pride_r5) (sef_r4 ses_r4 ses_pride_r4 sef_r5 ses_r5 ses_pride_r5) 

rename (subjective_wellbeing_cg_curr_r2 agency_cg_r2 pride_cg_r2 expect_age_earn_r2 expect_age_education_r2 expect_age_leave_r2 expect_age_indp_r2 ///
		subjective_wellbeing_cg_curr_r3 agency_cg_r3 pride_cg_r3 ///
		subjective_wellbeing_cg_curr_r4 agency_cg_r4 pride_cg_r4 expect_age_earn_r4 expect_age_education_r4 expect_age_leave_r4 expect_age_indp_r4 parent_relationship_r4 ///
		subjective_wellbeing_cg_curr_r5 agency_cg_r5 pride_cg_r5 expect_age_earn_r5 expect_age_education_r5 expect_age_leave_r5 expect_age_indp_r5 parent_relationship_r5 ) ///
	   (sw_cg_curr_r2 ag_cg_r2 pride_cg_r2 exp_a_earn_r2 exp_a_edu_r2 exp_a_leave_r2 exp_a_indp_r2 ///
		sw_cg_curr_r3 ag_cg_r3 pride_cg_r3 ///
		sw_cg_curr_r4 ag_cg_r4 pride_cg_r4 exp_a_earn_r4 exp_a_edu_r4 exp_a_leave_r4 exp_a_indp_r4 pa_r_r4 ///
		sw_cg_curr_r5 ag_cg_r5 pride_cg_r5 exp_a_earn_r5 exp_a_edu_r5 exp_a_leave_r5 exp_a_indp_r5 pa_r_r5 )
			
rename (subjective_wellbeing_r3 subjective_wellbeing_r4 subjective_wellbeing_r5) (sw_r3 sw_r4 sw_r5 )
			
rename (remit_transf_num_r1 ///
		remit_transf_num    econ_status_curr_r2  ///
		remit_transf_num_r3 econ_status_curr_r3 ///
		remit_transf_num_r4 econ_status_curr_r4 ///
							econ_status_curr_r5 ) ///
		(r_t_num_r1 ///
		 r_t_num_r2 e_s_cur_r2 ///
		 r_t_num_r3 e_s_cur_r3 ///
		 r_t_num_r4 e_s_cur_r4 ///
					e_s_cur_r5 )

rename (mom_works_r1 mom_jobs_r1 alone_hh_r1 alone_hh2_r1 ///
		mom_works_r2 mom_jobs_r2 alone_hh_r2 alone_hh2_r2 ///
		mom_works_r3 mom_jobs_r3 alone_hh_r3 alone_hh2_r3 hlp_study_nobody2_r3 ///
		mom_works_r4 mom_jobs_r4 alone_hh_r4 alone_hh2_r4 hlp_study_nobody2_r4 ///
								 alone_hh_r5 alone_hh2_r5) ///
	   (m_works_r1 m_jobs_r1 a_hh_r1 a_hh2_r1 ///
		m_works_r2 m_jobs_r2 a_hh_r2 a_hh2_r2 ///
		m_works_r3 m_jobs_r3 a_hh_r3 a_hh2_r3 nhlp_study_r3 ///
		m_works_r4 m_jobs_r4 a_hh_r4 a_hh2_r4 nhlp_study_r4 ///
							 a_hh_r5 a_hh2_r5 )
			   				
	global cov_hh    					urban bad_event 
	
	global cov_cg    					mother_edu age_pregnancy teen_pregnancy dead_children abortion no_boy child_not_first moves_cohab_preg_dum ///
										moves_cohab_bef_dum moves_separ_bef_dum moves_disp_bef_dum no_moves_before_dum antenatal_visit child_wanted poor_pregnancy ///
										beaten_mom birth_home birth_no_prof cg_mother_not_spanish read_spanish_dif own_house
								
	*global cov_com						land_invasion_r1 toilets_r1 water_r1  movwork_r1 prog_health_r1 drink_water_r1 bank_r1 internet_cab_r1 
	
	global outcomes 					ppvt_stdscre_r2 cda_r2 ppvt_stdscre_r3 egra_r3 math_r3 agency_r3 ppvt_stdscre_r4 lang_r4 math_r4  agency_r4  sef_r4  ses_pride_r4 ///
										ppvt_stdscre_r5 reading_r5 math_r5  agency_r5 sef_r5  ses_pride_r5
								
	global mechanisms_income			r_t_num_r1 wi_r1 r_t_num_r2 e_s_cur_r2 exp_pc_r2 wi_r2  r_t_num_r3 e_s_cur_r3 exp_pc_r3 wi_r3 r_t_num_r4 e_s_cur_r4 exp_pc_r4 wi_r4 ///
										e_s_cur_r5 exp_pc_r5 wi_r5 
																					
	global mechanisms_time				a_hh_r1  a_hh_r2 a_hh_r3 nhlp_study_r3 a_hh_r4 nhlp_study_r4 a_hh_r5
											
	global mechanisms_psychology_child 	sw_r3 sw_r4 pa_r_r4 sw_r5 pa_r_r5   
														
	global mechanisms_psychology_cg 	sw_cg_curr_r2 ag_cg_r2 pride_cg_r2 sw_cg_curr_r3 ag_cg_r3 pride_cg_r3 sw_cg_curr_r4 ag_cg_r4 pride_cg_r4 sw_cg_curr_r5 ag_cg_r5 pride_cg_r5 
	

	egen missings=rowmiss($cov_hh $cov_cg $outcomes)
	gen filter_final=cond(treatment==.|attrition==1|missings!=0,1,0) 
	order filter_final, after(treatment)  
	
	drop missings
	egen missings=rowmiss($mechanisms_income $mechanisms_time $mechanisms_psychology_child $mechanisms_psychology_cg)
	
	gen filter_mechanisms=cond(filter_final==1,1,cond(filter_final==0 & missings==0,0,1))
	
	replace m_jobs_r1=. if m_works_r1!=1
	replace m_jobs_r2=. if m_works_r2!=1
	replace m_jobs_r3=. if m_works_r3!=1
	replace m_jobs_r4=. if m_works_r4!=1
	
	gen child_hrs_hh_r2=child_hrs_care_r2+child_hrs_chores_r2
	gen child_hrs_hh_r3=child_hrs_care_r3+child_hrs_chores_r3
	gen child_hrs_hh_r4=child_hrs_care_r4+child_hrs_chores_r4
	gen child_hrs_hh_r5=child_hrs_care_r5+child_hrs_chores_r5
	run "$code/labels_covariates.do"
save "$outputs\rounds_all.dta",replace

