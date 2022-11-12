
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\1.Inputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

*Child database:
use "$inputs\pe_r3_childlevel8yrold.dta" , clear
	*Non-cogntive outcomes: agency, self-esteem, and subjective wellbeing:
		*Recoding:
			global recode ctryhdr3 cftrwrr3 cbrjobr3 cashshr3 ccltrgr3 cashclr3 cwrunir3 cembbkr3 cashwkr3
			foreach var in $recode{
				recode `var' (78 79 88 =.), gen(`var'_recode)
				}
			 
			global recode cpldecr3 cnochcr3
			foreach var in $recode{
				recode `var' (1=5 "Strongly disagree") (2=4 "Disagree") (4=2 "Agree") (5=1 "Strongly agree") (78 79 88 =.), gen(`var'_recode)
				}
				
			global recode hlpchlr3 leaderr3 incgmer3
			foreach var in $recode{
				recode `var' (2=1 "Sometimes") (1=2 "Yes/Always") (3=0 "No/Never") (78 79 88 =.), gen(`var'_recode)
				}
			
			recode hrdtlkr3 (1=0 "Always") (2=1 "Sometimes") (3=2 "Never") (78 79 88 =.), gen(hrdtlkr3_recode)
		
		*Relationship with peers:
			global peer_relationship hlpchlr3_recode leaderr3_recode incgmer3_recode hrdtlkr3_recode
			foreach var in $peer_relationship{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z,item /*very low internal consistency*/
			egen peer_relationship=rowmean(*_z)
			drop *_z
		
		*Pride index: 
			global pride cashshr3_recode cashclr3_recode cashwkr3_recode cembbkr3_recode cwrunir3_recode ccltrgr3_recode
			foreach var in $pride{
				sum `var'
				gen `var'_pr_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z,item /*regular internal consistency*/
			egen pride=rowmean(*_pr_z)
		
		*Agency: building on the concepts of locus of control proposed by Rotter, 1966 and self-efficacy by Bandura, 1993
			global agency ctryhdr3_recode cftrwrr3_recode cpldecr3_recode cbrjobr3_recode cnochcr3_recode
			foreach var in $agency{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z,item /*low internal consistency*/
			egen agency=rowmean(*_z)
			drop *_z
		
		*Subjective wellbeing
			recode stnprsr3  (79=.), gen(subjective_wellbeing)
	
	*Cognitive outcomes: vocabulary (PPVT), math, reading (early grade reading assessment-GRA), literacy (can read without problems-readci and can write without problems-writeci). 
	*All measurements will be standardised. In the case of vocabulary and reading, this will be performed taking into account the language (ppvtlang2 egralang2). 
		gen reads_well=cond(readci==.,.,cond(readci==4,1,0))
		gen writes_well=cond(writeci==.,.,cond(writeci==3,1,0))
	
	rename stdscre ppvt_stdscre
	*Renaming variables and saving database:
	global variables ppvt_stdscre egra math rppvt_co egra_co math_co rmath_co regra_co reads_well writes_well zwfa zhfa zbfa fwfa fhfa fbfa subjective_wellbeing agency pride ppvtlang2 ///
					 egralang2 mathlang2 hrdtlkr3_recode incgmer3_recode peer_relationship
	keep childid $variables 
	
	foreach var in $variables{
	    rename `var' `var'_r3
		}	
save "$aux\round3.dta",replace	

*Household questionaire:
use "$inputs\pe_r3_yc_householdlevel.dta" , clear
	*Identify attrition:
		gen attrition_r3=cond(situac_r3!=1,1,0)
		
		gen house_moved=cond(mvdtypr3==.,.,cond(mvdtypr3==1,0,1))				/*child has moved to another house between rounds 2 and 3*/
	
	*Father information:
		gen father_alive=cond(dadalr3==.|dadalr3==79,.,dadal)  		/*father is alive*/
		gen father_freq=cond(seedadr3==.|seedadr3==79,.,seedadr3)	/*frequency of seing the father*/
	
	*Reason for mother/caregiver migration:
		gen move_separ_dum=cond(whmvawr3==.,.,cond(whmvawr3==7,1,0))   /*divorce or separation*/
		gen move_cohab_dum=cond(whmvawr3==.,.,cond(whmvawr3==13,1,0))  /*cohabitation*/
		gen move_disp_dum=cond(whmvawr3==.,.,cond(whmvawr3==4,1,0))    /*family disputes*/
		gen move_work_dum=cond(whmvawr3==.,.,cond(whmvawr3==1,1,0))   /*work*/
	
	*Only mothers:
		gen move_separ_mom_dum=cond(whos1br3==2,0,move_separ_dum)   /*divorce or separation*/
		gen move_cohab_mom_dum=cond(whos1br3==2,0,move_cohab_dum)  /*cohabitation*/
		gen move_disp_mom_dum=cond(whos1br3==2,0,move_disp_dum)    /*family disputes*/
		gen move_work_mom_dum=cond(whos1br3==2,0,move_work_dum)	   /*work*/
		
	*Help to study:
		gen hlp_study_old_sibling=cond(hlphmwr3==5,1,0) /*older sibling*/
		gen hlp_study_mom=cond(hlphmwr3==3,1,0)			/*mother*/
		gen hlp_study_father=cond(hlphmwr3==2,1,0)		/*father*/
		gen hlp_study_nobody=cond(hlphmwr3==1,1,0)		/*nobody helps the child to study*/
		gen hlp_study_nobody2=cond(hlphmwr3==.,.,cond(hlphmwr3==1,1,0))		/*nobody helps the child to study*/
	
	*Remittances:
		egen remit_transf_num=rowtotal(tmealmr3 tmeoutr3 tmeinsr3) 	/*# of remittances in transfers received during the last 12 months*/
		egen remit_rest_num=rowtotal( tmertrr3 tmesclr3 tmerlgr3 tmechrr3 tmebnkr3 tmerntr3 tmepayr3 tmemedr3 tmedvdr3 tmeinhr3 tmejunr3 tmepubr3 tmeprvr3 tmeothr3) 
																	/*# of other types of remittances received during the last 12 months*/
		
		egen remit_transf_last=rowtotal(amtalmr3 amtinsr3 amtoutr3 ) 	/*total amount of last remittances in transfers received during the last 12 months*/
		egen remit_rest_last=rowtotal(amtrtrr3 amtsclr3 amtrlgr3 amtchrr3 amtbnkr3 amtrntr3 amtpayr3 amtmedr3 amtdvdr3 amtinhr3 amtjunr3 amtpubr3 amtprvr3 amtothr3)
																		/*total amount of last other types of remittances received during the last 12 months*/
		
		gen remit_transf=cond(remit_transf_num>0,1,0)
		
		recode almnyr3 (79 =.), gen(remit_alimony)
		rename (tmealmr3 amtalmr3) (remit_alimony_num remit_alimony_amount)
		
		recode helpr301 (79 =.), gen(help_child_abuse)
			 
	*Economic status & changes:
		recode dsccurr3 (2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute"), gen(econ_status_curr) /*Current economic status*/
		recode dscagor3 (2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute"), gen(econ_status_past) /*Past economic status (4 years ago)*/
		
		gen econ_status_struggle=cond(econ_status_curr==.,.,cond(econ_status_curr==3,1,0))											/*Current economic status: struggling*/
		gen econ_status_poor=cond(econ_status_curr==.,.,cond(econ_status_curr<3,1,0))												/*Current economic status: poor*/
		gen econ_status_worse=cond(econ_status_curr==.,.,cond(econ_status_curr<econ_status_past,1,0))								/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past>2,1,0))	/*4 yrs comparison: HH entered poverty*/
		gen econ_exit_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==0 & econ_status_past<3,1,0))	/*4 yrs comparison: HH exited to poverty*/
		gen econ_stay_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past<3,1,0))	/*4 yrs comparison: HH exited stayed in poverty*/
		
	*Time use:
		rename (ycslepr3 yccothr3 yccslvr3 ycdmtsr3 ycschlr3 ycplayr3) (child_hrs_sleep child_hrs_care child_hrs_selfcare child_hrs_chores child_hrs_school child_hrs_play)
		
		egen child_hrs_work=rowtotal (yctsfmr3 ycacmyr3) /*child: hours working in a typical day*/
		egen child_hrs_study=rowtotal (ycstdyr3 ycextur3) /*child: hours studying outside school in a typical day*/
	
	*Caregiver's perceptions and non-cognitive outcomes:
		*Subjective Wellbeing:
			rename (ladderr3 ownlfer3 ) (subjective_wellbeing_cg_curr satisfaction_life_cg)  
			recode farladr3 (77 78 79 = .), gen(subjective_wellbeing_cg_fut)											/*perceived future subjective wellbeing*/
			egen satisfaction_cg=rowmean( stslvgr3 stshthr3 stsachr3 stsrltr3 stssfer3 stscomr3 stssecr3 stsrlgr3)		/*satisfaction with elementos of life*/
			
		*Dedication to child:
			global time_school  wrkdayr3 mtprasr3 grpmtgr3 indmtgr3 schprtr3 fndrser3
			foreach var in $time_school{
				recode `var' (2 79 =.), gen(`var'_recode)
				}
			egen time_school=rowmean(wrkdayr3_recode mtprasr3_recode grpmtgr3_recode indmtgr3_recode schprtr3_recode fndrser3_recode) /*school activities*/
			
			egen know_school=rowmean(prntfrr3 tchrr3 nmfrndr3 aftrscr3) /*knowledge of child's school peers and parents*/
			
		*Agency:
			global recode cag4r3 cag5r3
			foreach var in $recode{
				recode `var' (1 = 5 "Strongly disagree") (2 = 4 "Disagree") (4 =2 "Agree") (5 = 1 "Strongly agree") (77 78 79 =.), gen(`var'_recode)
				}
			global agency cag1r3 cag2r3 cag3r3 cag4r3_recode cag5r3_recode
			foreach var in $agency{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen agency_cg=rowmean(*_z)
			drop *_z
		
		*Proud index:
			recode cps1r3 (77 78 79 =.), gen(cps1r3_recode)
			global pride cps1r3_recode cps2r3 cps3r3 cps4r3 cps5r3
			foreach var in $pride{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen pride_cg=rowmean(*_z)
			drop *_z
	
		*Self-respect and inclusion:
			global recode csd2r3 csd3r3
			foreach var in $recode{
				recode `var' (1 = 5 "Strongly disagree") (2 = 4 "Disagree") (4 =2 "Agree") (5 = 1 "Strongly agree") (77 78 79 =.), gen(`var'_recode)
				}
			global inclusion csd1r3 csd2r3_recode csd3r3_recode
			foreach var in $inclusion{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen inclusion_cg=rowmean(*_z)
			drop *_z
		 
	*HH risks:
		recode cnsdrkr3 (. = 0 "no") (1 = 2 "yes") (2 = 1 "sometimes"), gen(hh_drunk)		/*Any household member gets drunk*/
		recode aggdrkr3 (. = 0 "no") (1 = 2 "yes") (2 = 1 "sometimes"), gen(hh_agressive)	/*Any household member becomes aggressive when drunk*/
		rename (crtprgr3 bulldr3 evntr340) (mother_pregnancies bullied_r3 hh_divorce)
	
	destring typesite, gen(dropme)
	gen urban=cond(dropme==2,0,dropme)
	drop dropme
	
	gen exp_pc_r3_log=log(totalexp_pc)
	
	keep childid house_moved mumalr3 seemumr3 father_alive father_freq whyndlr3 lvmronr3 move_separ_dum move_cohab_dum move_disp_dum move_work_dum move_separ_mom_dum move_cohab_mom_dum ///
		 move_disp_mom_dum move_work_mom_dum time_school rerninr3 rellivr3 know_school bullied_r3 ///
		 help_child_abuse econ_status_curr econ_status_past econ_status_struggle econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty econ_stay_poverty hh_divorce chlwrkr3 ///
		 child_hrs_sleep child_hrs_care child_hrs_selfcare child_hrs_chores child_hrs_school child_hrs_play child_hrs_work child_hrs_study mother_pregnancies subjective_wellbeing_cg_curr ///
		 subjective_wellbeing_cg_fut agency_cg pride_cg inclusion_cg hh_drunk hh_agressive foodexp_rpc nfoodexp_rpc exp_pc_r3_log hq cd sv wi attrition_r3 hlp_study_old_sibling ///
		 hlp_study_mom hlp_study_father hlp_study_nobody hlp_study_nobody2 remit_transf_num remit_rest_num remit_transf_last remit_rest_last remit_transf remit_alimony remit_alimony_num ///
		 remit_alimony_amount urban
		 
	rename (mumalr3 seemumr3 father_alive father_freq whyndlr3 lvmronr3 rerninr3) (mumal_r3 seemum_r3 father_alive_r3 father_freq_r3 whyndl_r3 lvmron_r3 rernin_r3)
	
	merge 1:1 childid using "$aux\round3.dta"	
	drop _merge
save "$aux\round3.dta",replace

*Household composition database:
use "$inputs\pe_r3_yc_householdmemberlevel.dta" ,clear
	gen mother=cond((relate==1| relate==3| relate==24) & memsex==2,1,0)
	gen bio_father=cond((relate==1| relate==3| relate==24) & memsex==1,1,0)
	gen stepfather=cond(relate==2 & memsex==1,1,0)
	gen grandparent=cond(relate==5|relate==6,1,0)
	gen blood_sibling=cond(relate==7,1,0)
	gen half_sibling=cond(relate==8|relate==9|relate==10,1,0)
	gen all_sibling=cond(relate==7|relate==8|relate==9|relate==10,1,0)
	
	gen dropme=cond(id==0,age,.)
	bys childid: egen dropme2=max(dropme)
	
	gen blood_sibling_old=cond(blood_sibling==1 & age>dropme2,1,0)
	gen blood_sibling_young=cond(blood_sibling==1 & age<dropme2,1,0)
	
	gen half_sibling_old=cond(half_sibling==1 & age>dropme2,1,0)
	gen half_sibling_young=cond(half_sibling==1 &age<dropme2,1,0)
	
	gen all_sibling_old=cond(all_sibling==1 & age>dropme2,1,0)
	gen all_sibling_young=cond(all_sibling==1 & age<dropme2,1,0)
	
	gen uncle=cond(relate==13,1,0)
	
	gen mother_hh=cond(mother==1 & livhse==1,1,0)				/*mother still in hh*/
	gen bio_father_hh=cond(bio_father==1 & livhse==1,1,0)		/*father still in hh*/
	gen step_father_hh=cond(stepfather==1 & livhse==1,1,0)		/*stepfather still in hh*/
	gen grandparents_hh=cond(grandparent==1 & livhse==1,1,0)	/*grandparent still in hh*/
	gen uncle_hh=cond(uncle==1 & livhse==1,1,0)					/*uncle/aunt still in hh*/
	
	collapse (max)  blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh ///
					step_father_hh grandparents_hh uncle_hh, by(childid)
	
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh)
	gen alone_hh=cond(dropme==0,1,0) /*no presence of father, step father or grandparents*/
	drop dropme
	
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh uncle_hh)
	gen alone_hh2=cond(dropme==0,1,0) /*no presence of father, step father, grandparents, uncles or aunts*/
	drop dropme
	
	merge 1:1 childid using "$aux\round3.dta"
	drop _merge
save "$aux\round3.dta",replace

*Mom labour:
use "$inputs\pe_r3_yc_householdmemberlevel.dta" ,clear
	gen mother=cond((relate==1| relate==3| relate==24) & memsex==2,1,0)
	keep if mother==1
	keep childid id
save "$aux\round3_mother_id.dta",replace
	
use "$inputs\pe_r3_yc_stblhhsec3percwageincome.dta", clear
	gen work_payment1=1
	gen work_payment2=1
	gen work_months=cond(dpntmer3==1,.,cond(dpntmer3==2,dpnnumr3/30,cond(dpntmer3==3,(dpnnumr3/52)*12,cond(dpntmer3==4,(dpnnumr3/26)*12,cond(dpntmer3==5,dpnnumr3,dpnnumr3/12)))))
	gen work_wage_month=cond(dpntmer3==1,.,cond(dpntmer3==2,dpnamtr3*30,cond(dpntmer3==3,(dpnamtr3/7)*30,cond(dpntmer3==4,(dpnamtr3/14)*30,cond(dpntmer3==5,dpnamtr3,dpnamtr3/12)))))
	gen work_bonus_month=cond(cshtmer3==.,0,cond(cshtmer3==2,cshamtr3*30,cond(cshtmer3==3,(cshamtr3/7)*30,cond(cshtmer3==4,(cshamtr3/14)*30,cond(cshtmer3==5,cshamtr3,cond(cshtmer3==6,cshamtr3/3,cond(cshtmer3==7,cshamtr3/6,dpnamtr3/12)))))))
	egen work_inc_total_month=rowtotal(work_wage_month work_bonus_month)
	
	gen work_wage_total=dpnamtr3*dpnnumr3
	gen work_bonus_total=cshamtr3*cshnumr3
	egen work_inc_total=rowtotal(work_wage_total work_bonus_total)
	
	collapse (sum) work_inc_total_month work_inc_total work_payment2 (max) work_payment1 work_months , by(childid id)
	
	rename (work_payment1 work_payment2 work_months) (mom_works mom_jobs mom_works_months)
	
	merge 1:m childid id using "$inputs\pe_r3_yc_stblhhsec3percmonetaryincomes.dta" /*merge with data from non-wage monetary earnings*/
	replace mom_works=1 
	replace mom_jobs=cond(_merge==2,1,mom_jobs)
	
	gen dropme=cond(inctmer3==2,incnumr3/30,cond(inctmer3==3,(incnumr3/52)*12,cond(inctmer3==4,(incnumr3/26)*12,cond(inctmer3==5,incnumr3,cond(inctmer3==6,incnumr3/3,cond(inctmer3==7,incnumr3/6,incnumr3/12))))))
	replace dropme=cond(inctmer3==1,.,dropme)
	replace mom_works_months=cond(_merge==2,dropme,mom_works_months)
	drop dropme
	
	gen dropme=cond(inctmer3==2,incincr3*30,cond(inctmer3==3,(incincr3/7)*30,cond(inctmer3==4,(incincr3/14)*30,cond(inctmer3==5,incincr3,incincr3/12))))
	replace dropme=cond(inctmer3==1,.,dropme)
	replace work_inc_total_month=cond(_merge==2,dropme,work_inc_total_month)
	drop dropme
	
	gen dropme=incincr3*incnumr3
	replace work_inc_total=cond(_merge==2,dropme,work_inc_total)
	drop dropme
	
	collapse (sum) work_inc_total_month work_inc_total mom_jobs (max) mom_works mom_works_months , by(childid id)
	
	gen work_inc_total_month_log=log(work_inc_total_month) 
	gen work_inc_total_log=log(work_inc_total)
	
	rename (work_inc_total_month work_inc_total work_inc_total_month_log work_inc_total_log) (mom_works_inc_month mom_works_inc_total mom_works_inc_month_log mom_works_inc_total_log)

	merge 1:1 childid id using "$aux\round3_mother_id.dta"
	drop if _merge==1
	
	global works mom_jobs mom_works mom_works_months mom_works_inc_month mom_works_inc_total 
	foreach var in $works{
		replace `var'=cond(_merge==2,0,`var')
	}

	duplicates tag childid, gen(dropme)
	drop if dropme!=0 & _merge==2
	drop _merge dropme id
	merge 1:1 childid using "$aux\round3.dta"
	drop _merge
			
*Final touches: ordering outcomes and relevant covariates:
	replace mom_works=cond(mother_hh==0,.,mom_works)
	replace mom_jobs=cond(mother_hh==0,.,mom_jobs)
	
	order childid attrition_r3 house_moved urban, first
	
	gen outcomes=. 
	order outcomes  ppvt_stdscre_r3 ppvtlang2_r3 egralang2_r3 mathlang2_r3 egra_r3 math_r3 egra_co_r3 math_co_r3 rppvt_co_r3 regra_co_r3 rmath_co_r3 reads_well_r3 writes_well_r3 agency_r3 ///
					pride_r3 subjective_wellbeing_r3 peer_relationship_r3 bullied_r3 , after(house_moved)
	
	gen cov_hh_charac=.
	order cov_hh_charac mumal_r3 seemum_r3 father_alive_r3 father_freq_r3 whyndl_r3 lvmron_r3 mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh alone_hh2 mom_works_inc_month ///
		  mom_works_inc_total mom_jobs mom_works mom_works_months mom_works_inc_month mom_works_inc_month_log mom_works_inc_total_log blood_sibling half_sibling all_sibling ///
		  blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young rellivr3  move_separ_dum move_cohab_dum ///
		  move_disp_dum move_work_dum move_separ_mom_dum move_cohab_mom_dum move_disp_mom_dum move_work_mom_dum, after(incgmer3_recode_r3)
	
	gen cov_hh_econ=.
	order cov_hh_econ remit_transf_num remit_rest_num remit_transf_last remit_rest_last remit_transf remit_alimony remit_alimony_num remit_alimony_amount rernin_r3 econ_enter_poverty ///
		  econ_exit_poverty econ_status_curr econ_status_past econ_status_poor econ_status_struggle econ_status_worse econ_stay_poverty foodexp_rpc nfoodexp_rpc ///
		  exp_pc_r3_log hq cd sv wi, after(hlp_study_nobody)
	
	gen cov_time_use=.
	order cov_time_use child_hrs_care child_hrs_chores child_hrs_play child_hrs_school child_hrs_selfcare child_hrs_sleep child_hrs_study child_hrs_work, after(wi)
	
	gen cov_hh_risk=.
	order cov_hh_risk hh_agressive hh_drunk hh_divorce chlwrkr3 help_child_abuse mother_pregnancies, after(child_hrs_work)
	
	gen cov_caregiver=.
	order cov_caregiver  time_school know_school subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg inclusion_cg pride_cg hlp_study_old_sibling hlp_study_mom hlp_study_father ///
		  hlp_study_nobody hlp_study_nobody2, after(mother_pregnancies)

	gen cov_health=.
	order cov_health zbfa_r3 zhfa_r3 zwfa_r3 fwfa_r3 fhfa_r3 fbfa_r3, after(hlp_study_nobody)
	
	global variables cov_hh_charac mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh mom_works mom_jobs mom_works_months mom_works_inc_month mom_works_inc_total ///
					 mom_works_inc_month_log mom_works_inc_total_log blood_sibling half_sibling ///
					 all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young rellivr3 move_separ_dum move_cohab_dum ///
					 move_disp_dum move_work_dum move_separ_mom_dum move_cohab_mom_dum move_disp_mom_dum move_work_mom_dum cov_hh_econ econ_enter_poverty econ_exit_poverty econ_status_curr ///
					 econ_status_past econ_status_poor econ_status_struggle econ_status_worse econ_stay_poverty foodexp_rpc nfoodexp_rpc hq cd sv wi cov_time_use child_hrs_care ///
					 child_hrs_chores child_hrs_play child_hrs_school child_hrs_selfcare child_hrs_sleep child_hrs_study child_hrs_work cov_hh_risk hh_agressive hh_drunk hh_divorce chlwrkr3 ///
					 help_child_abuse mother_pregnancies cov_caregiver time_school know_school house_moved outcomes subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg ///
					 inclusion_cg pride_cg cov_health zbfa_r3 zhfa_r3 zwfa_r3 fwfa_r3 fhfa_r3 fbfa_r3 hlp_study_old_sibling hlp_study_mom hlp_study_father hlp_study_nobody hlp_study_nobody2 ///
					 remit_transf_num remit_rest_num remit_transf_last remit_rest_last remit_transf remit_alimony ///
					 remit_alimony_num remit_alimony_amount urban alone_hh2
	
	foreach var in $variables{
		rename `var' `var'_r3
		}
save "$outputs\round3.dta",replace	
	
	