
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\1.Inputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

*Child database: non-cognitive outcomes-agency, self-esteem, social skills, self-efficacy, relationship with parents, subjective wellbeing, gender equality 
use "$inputs\pe_r5_ycch_youngerchild.dta" , clear
	rename *, lower
	*Non-cognitive outcomes & mechanisms:
		*Recoding:
			global recode feay02r5 feay09r5 feay12r5 feay16r5 feay20r5 feay24r5  feay34r5 feay01r5 feay05r5 feay08r5 feay11r5 feay15r5 feay18r5 feay22r5 feay26r5 feay28r5 feay32r5 ///
						  feay04r5 feay06r5 feay14r5 feay17r5 feay23r5 feay27r5 feay30r5 feay31r5 feay33r5 cashclr5 cashwkr5 ccltrgr5 cembbkr5 cashshr5 cwrunir5 feay03r5 feay07r5 feay10r5 ///
						  feay13r5 feay19r5 feay21r5 feay25r5 feay29r5  cpldecr5 cnochcr5 ctryhdr5 cftrwrr5 cbrjobr5 gndrlr53 gndrlr55 gndrlr57 gndrlr59 gndrlr512

			foreach var in $recode{
				recode `var' (77 78 79 88 =.), gen(`var'_recode)
				}
				
			global gender gndrlr51 gndrlr52 gndrlr54 gndrlr56 gndrlr58 gndrlr510 gndrlr511
			foreach var in $gender{
				recode `var' (1 = 4 "Strongly disagree") (2 = 3 "Disagree") (3 = 2 "Agree") (4 = 1 "Strongly agree") (77 78 79 88 =.), gen(`var'_recode)
				}

		*Subjective Wellbeing
			recode stnprsr5 (77 78 79 88 =.), gen(subjective_wellbeing)
	
		*Relationship with peers: self-concept (Original source: Self description questionnaire-SDQ- Marsh, Relich & Smith, 1981)
			global peer_relationship feay02r5_recode feay09r5_recode feay12r5_recode feay16r5_recode feay20r5_recode feay24r5_recode feay31r5_recode feay34r5_recode
			foreach var in $peer_relationship{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item /*very good internal consistency*/
			egen peer_relationship=rowmean(*_z)
			drop *_z
			
		*Self-efficacy: (original source: Generalised Self-Efficacy Scale-GSE-Jerusalem & Schwarzer,1992)
			global self_efficacy feay01r5_recode feay05r5_recode feay08r5_recode feay11r5_recode feay15r5_recode feay18r5_recode feay22r5_recode feay26r5_recode feay28r5_recode feay32r5_recode
			foreach var in $self_efficacy{
				sum `var'
				gen `var'_sef_z=(`var'-r(mean))/r(sd)
				}
			alpha *_sef_z, item /*good internal consistency*/
			egen self_efficacy=rowmean(*_sef_z)
			
		*Self-esteem:(original source: Rosenberg Scale)
			global self_esteem feay04r5_recode feay06r5_recode feay14r5_recode feay17r5_recode feay23r5_recode feay27r5_recode feay30r5_recode feay33r5_recode  
			foreach var in $self_esteem{
				sum `var'
				gen `var'_ses_z=(`var'-r(mean))/r(sd)
				}
			alpha *_ses_z, item /*good internal consistency*/
			egen self_esteem=rowmean(*_ses_z)
	
		*Agency: building on the concepts of locus of control proposed by Rotter, 1966 and self-efficacy by Bandura, 1993
			global agency cpldecr5_recode cnochcr5_recode ctryhdr5_recode cftrwrr5_recode cbrjobr5_recode
			foreach var in $agency{
				sum `var'
				gen `var'_ag_z=(`var'-r(mean))/r(sd)
				}
			alpha *_ag_z, item /*low internal consistency*/
			egen agency=rowmean(*_ag_z)
					
		/*Core self-evaluation (Judge, Locke, and Durham (1997)): combination of agency, self-efficacy, and self-esteem. 
			Following Chang et al. (2021), it will be constructed using Principal Component Analysis (PCA): "A principal component analysis confirms that [in the Indian case] items from all 
			three scales load to the first factor which has an eigenvalue of 3.62 and explains 85% of the total variation... [W]e use the first factor emerging from the principal component 
			analysis as a measure of the latent CSE personality traits"*/
				alpha *_sef_z *_ses_z *_ag_z, item /*very good internal validity*/
				pca *_sef_z *_ses_z *_ag_z,components (4) /*first component explains only 24% of the total variance*/
				screeplot, mean /*however, the eigenvalues drop drastically after the first component*/
				
				predict cse, score /*PCA only works with complete data, thus 258 obs. are turned into missing*/
				
				egen cse2=rowmean(*_sef_z *_ses_z *_ag_z) /*calculating CSE as the mean of the items in order not to lose the 258 obs. This was done considering the high internal reliability.*/
				drop *_sef_z *_ag_z

		*Pride index: 
			global pride cashclr5_recode cashwkr5_recode cashshr5_recode ccltrgr5_recode cembbkr5_recode cwrunir5_recode
			foreach var in $pride{
				sum `var'
				gen `var'_pr_z=(`var'-r(mean))/r(sd)
				}
			alpha *_pr_z, item /*fair internal consistency*/
			egen pride=rowmean(*_pr_z)
			
			egen self_estem_pride=rowmean(*_ses_z *_pr_z)
			drop *_ses_z *_pr_z
		
		*Relationship with parents: self-concept (Original source: Self description questionnaire-SDQ- Marsh, Relich & Smith, 1981)
			global parent_relationship feay03r5_recode feay07r5_recode  feay10r5_recode  feay13r5_recode feay19r5_recode feay21r5_recode feay25r5_recode feay29r5_recode
			foreach var in $parent_relationship{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item /*very good internal consistency*/
			egen parent_relationship=rowmean(*_z)
			drop *_z

		*Gender equality: 
			global gender gndrlr51_recode gndrlr52_recode gndrlr54_recode gndrlr56_recode gndrlr58_recode gndrlr510_recode gndrlr511_recode gndrlr53_recode gndrlr55_recode ///
						  gndrlr57_recode gndrlr59_recode gndrlr512_recode
			foreach var in $gender{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item /*fair internal consistency*/
			egen gender_equality=rowmean(*_z)
			drop *_z
	
	*Time use:
		global time_use sleepr5 crothr5 dmtskr5 tsfarmr5 actpayr5 atschr5 studygr5 lsurer5
		foreach var in $time_use{
			recode `var' (77 78 79 88 = .), gen (`var'_recode)
			}
		
		egen child_hrs_work=rowtotal (tsfarmr5_recode actpayr5_recode) /*child: hours working in a typical day*/
		rename (sleepr5_recode crothr5_recode dmtskr5_recode atschr5_recode studygr5_recode lsurer5_recode) ///
			   (child_hrs_sleep child_hrs_care child_hrs_chores child_hrs_school child_hrs_study child_hrs_play)
	
	*Support
		recode pplrelr5 (77 78 79 88 =.), gen(support_ppl)

	*Child perception of economic status:	
		recode hhnowr5 (1 = 5 "Rich") (2 = 4 "Comfortable") (3 = 3 "Struggle") (4 = 2 "Poor") (5 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_curr_child) /*Current economic status*/
		recode hh4yrr5 (1 = 5 "Rich") (2 = 4 "Comfortable") (3 = 3 "Struggle") (4 = 2 "Poor") (5 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_past_child) /*Past economic status (4 yrs*/
		
		gen econ_status_struggle_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child==3,1,0))								/*Current economic status: struggling*/
		gen econ_status_poor_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child<3,1,0))										/*Current economic status: poor*/
		gen econ_status_worse_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child<econ_status_past_child,1,0))				/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==1 & econ_status_past_child>2,1,0))	/*vs 4 yrs-HH entered poverty*/
		gen econ_exit_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==0 & econ_status_past_child<3,1,0))	/*vs 4 yrs-HH exited to poverty*/
		gen econ_stay_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==1 & econ_status_past_child<3,1,0))	/*vs 4 yrs-HH stayed in poverty*/
	
 *Rename variables:
	 global variables  peer_relationship self_efficacy self_esteem pride self_estem_pride parent_relationship agency subjective_wellbeing  bmi zhfa zbfa fhfa fbfa  child_hrs_sleep ///
						child_hrs_care child_hrs_chores child_hrs_school child_hrs_study child_hrs_play child_hrs_work support_ppl  econ_status_curr_child econ_status_past_child ///
						econ_status_struggle_child econ_status_poor_child econ_status_worse_child econ_enter_poverty_child econ_exit_poverty_child econ_stay_poverty_child gender_equality ///
						cse cse2
	 keep childcode $variables
	 foreach var in $variables{
	     rename `var' `var'_r5
		}	
save "$aux\round5.dta",replace	

*Child database: cognitive outcomes
use "$inputs\pe_r5_yccogtest_youngerchild.dta" , clear
	rename *, lower
	
	recode stdscre (-888 -99 -88 -79 =.), gen(ppvt_stdscre_r5)
	
	rename (reading_raw maths_raw ppvt_raw testlang  ) (reading_r5 math_r5 ppvt_r5 test_lang_r5)
	keep childcode reading_r5 math_r5 ppvt_r5 test_lang_r5 ppvt_stdscre_r5
	merge 1:1 childcode using "$aux\round5.dta"
	drop _merge
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
save "$aux\round5.dta",replace	

*Child mobility database:
use "$inputs\pe_r5_ycch_childmobility.dta" , clear
	rename *, lower
	gen moves=1
	gen moves_separ=cond(rsnmvr51==4|rsnmvr52==4|rsnmvr51==5|rsnmvr52==5,1,0)

	collapse (sum)  moves moves_separ, by (childcode)
	gen house_moved=1
	gen moves_separ_dum=cond(moves_separ>0,1,0)
	gen childid="PE"+string(childcode, "%06.0f")
	merge 1:1 childid using  "$aux\round5.dta"
	global move moves moves_separ house_moved moves_separ_dum
	foreach var in $move{
	    replace `var'=cond(_merge==2,0,`var')
		}
	drop _merge
save "$aux\round5.dta",replace

*HH database:	
	use "$inputs\pe_r5_ychh_youngerhousehold.dta" , clear
		rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	recode finhlpr5 (76 77 78 79 88 99 =.), gen(finhlpr5_recode)
	rename (finhlpr5_recode ownhser5) (support_ppl_cg own_house_r5)
	
	*Economic status:	
		recode dsccurr5 (1 2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_curr) /*Current economic status*/
		recode dscagor5 (1 2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_past) /*Past economic status (4 yrs ago)*/
		
		gen econ_status_struggle=cond(econ_status_curr==.,.,cond(econ_status_curr==3,1,0))											/*Current economic status: struggling*/
		gen econ_status_poor=cond(econ_status_curr==.,.,cond(econ_status_curr<3,1,0))												/*Current economic status: poor*/
		gen econ_status_worse=cond(econ_status_curr==.,.,cond(econ_status_curr<econ_status_past,1,0))								/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past>2,1,0))	/*vs 4 yrs-HH entered poverty*/
		gen econ_exit_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==0 & econ_status_past<3,1,0))	/*vs 4 yrs-HH exited to poverty*/
		gen econ_stay_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past<3,1,0))	/*vs 4 yrs-HH exited stayed overty*/
		
	*Caregiver:
		*Wellbeing: 
			recode ladderr5 (77 78 79 88 = .), gen(subjective_wellbeing_cg_curr)										/*current future subjective wellbeing*/
			recode farladr5 (77 78 79 88 = .), gen(subjective_wellbeing_cg_fut)											/*perceived future subjective wellbeing*/
			*egen satisfaction_cg=rowmean( stslvgr3 stshthr3 stsachr3 stsrltr3 stssfer3 stscomr3 stssecr3 stsrlgr3)		/*satisfaction with elementos of life*/
			
		*Expectations from child:
			global expect  livclsr5 fnassbr5 hlphser5 fnasyur5 cresibr5 creyour5 emtsupr5 
			foreach var in $expect{
				recode `var' ( -88 -79 -77 76 77 78 79 88 99 277 288 303 =.), gen(`var'_recode)
				}
			
			global age  ernmnyr5 lveedcr5 finindr5 lvehser5 getmarr5 hvechdr5
			foreach var in $age{
				recode `var' ( -88/5 =.) (76/10000 =.), gen(`var'_recode)
				}
			
			egen expec_support_child=rowmean( livclsr5_recode fnassbr5_recode hlphser5_recode fnasyur5_recode cresibr5_recode creyour5_recode emtsupr5_recode)
			egen expect_age_child=rowmean(ernmnyr5_recode lveedcr5_recode finindr5_recode lvehser5_recode getmarr5_recode hvechdr5_recode)
			
			rename ( ernmnyr5_recode lveedcr5_recode finindr5_recode lvehser5_recode getmarr5_recode hvechdr5_recode) ///
					(expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent)
			
		*Dedication to child:
			recode askschr5 (77 78 79 88 =.), gen(ask_school) /*times p/week caregiver asked the child about school*/
	
		*Agency and pride:
			*Recoding:
				global recode cag1r5 cag2r5 cps2r5 cps3r5 cps4r5 cps5r5  /*recoding items that make the agency and pride indices*/
				foreach var in $recode{
					recode `var' (77 78 79 88 =.), gen(`var'_recode)
					}
				
				recode cag5r5 (1 = 5 "Strongly disagree") (2 = 4 "Disagree") (4 =2 "Agree") (5 = 1 "Strongly agree") (77 78 79 88 =.), gen(cag5r5_recode)
				
			*Agency:
				global agency cag1r5_recode cag2r5_recode cag5r5_recode
				alpha $agency, item /*very low internal consistency*/
				foreach var in $agency{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen agency_cg=rowmean(*_z)
				drop *_z
		
			*Pride index: 
				global pride cps2r5_recode cps3r5_recode cps4r5_recode cps5r5_recode  
				alpha $pride, item
				foreach var in $pride{
					sum `var'
					gen `var'_pr_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen pride_cg=rowmean(*_pr_z)
				drop *_z

		*Helping with homeworks:
			recode hlphmwr5 ( 77 78 79 88 = .), gen(hlphmwr5_recode)
			gen hlp_study_old_sibling=cond(hlphmwr5==5,1,0) 			/*older siblings*/
			gen hlp_study_mom=cond(hlphmwr5==3,1,0)						/*mother*/
			gen hlp_study_father=cond(hlphmwr5==2,1,0)					/*father*/
			gen hlp_study_nobody=cond(hlphmwr5==1,1,0)					/*nobody helps the child to study*/
			gen hlp_study_nobody2=cond(hlphmwr5_recode==.,.,cond(hlphmwr5==1,1,0))
			
			rename numhlpr5 hlp_study_days /*# of days caregiver helped the child with the homework during last week of classes*/
			replace hlp_study_days =cond(hlp_study_nobody==1,0,hlp_study_days)
	
	gen exp_pc_r5_log=log(totalexp_pc)
	
	keep childid childcode support_ppl_cg econ_status_curr econ_status_past econ_status_struggle econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty ///
		 econ_stay_poverty ask_school subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg ask_school hlp_study_old_sibling hlp_study_mom ///
		 hlp_study_father hlp_study_nobody hlp_study_nobody2 hlp_study_days exp_pc_r5_log hhsize hq cd sv wi exp_pc_r5_log  expec_support_child expect_age_child ///
		 expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent
	
	merge 1:1 childid using "$aux\round5.dta"
	drop _merge
save "$aux\round5.dta", replace

*Household composition database:
use "$inputs\pe_r5_ychh_householdrosterr5.dta" ,clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	gen mother=cond((relater5==1| relater5==3| relater5==24) & memsexr5==2,1,0)
	gen bio_father=cond((relater5==1| relater5==3| relater5==24) & memsexr5==1,1,0)
	gen stepfather=cond(relater5==2 & memsexr5==1,1,0)
	gen grandparent=cond(relater5==5|relater5==6,1,0)
	gen blood_sibling=cond(relater5==7,1,0)
	gen half_sibling=cond(relater5==8|relater5==9|relater5==10|relater5==11|relater5==12,1,0)
	gen all_sibling=cond(blood_sibling==1|half_sibling==1,1,0)
	
	gen dropme=cond(memidr5==0,memager5,.)
	bys childid: egen dropme2=max(dropme)
	
	gen blood_sibling_old=cond(blood_sibling==1 & memager5>dropme2,1,0)
	gen blood_sibling_young=cond(blood_sibling==1 & memager5<dropme2,1,0)
	
	gen half_sibling_old=cond(half_sibling==1 & memager5>dropme2,1,0)
	gen half_sibling_young=cond(half_sibling==1 &memager5<dropme2,1,0)
	
	gen all_sibling_old=cond(all_sibling==1 & memager5>dropme2,1,0)
	gen all_sibling_young=cond(all_sibling==1 & memager5<dropme2,1,0)
	
	gen uncle=cond(relater5==13,1,0)
	
	gen mother_hh=cond(mother==1 & livhser5==1,1,0)				/*mother still in hh*/
	gen bio_father_hh=cond(bio_father==1 & livhser5==1,1,0)		/*father still in hh*/
	gen step_father_hh=cond(stepfather==1 & livhser5==1,1,0)	/*stepfather still in hh*/
	gen grandparents_hh=cond(grandparent==1 & livhser5==1,1,0)	/*grandparent still in hh*/
	gen uncle_hh=cond(uncle==1 & livhser5==1,1,0)				/*uncle/aunt still in hh*/
	
	gen blood_sibling_old_hh=cond(blood_sibling_old==1 & livhser5==1,1,0)
	gen blood_sibling_young_hh=cond(blood_sibling_young==1 & livhser5==1,1,0)
	gen half_sibling_old_hh=cond(half_sibling_old==1 & livhser5==1,1,0)
	gen half_sibling_young_hh=cond(half_sibling_young==1 & livhser5==1,1,0)
	gen all_sibling_old_hh=cond(all_sibling_old==1 & livhser5==1,1,0)
	gen all_sibling_young_hh=cond(all_sibling_young==1 & livhser5==1,1,0)
	
	collapse (max)  blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh ///
					step_father_hh grandparents_hh  blood_sibling_old_hh blood_sibling_young_hh half_sibling_old_hh half_sibling_young_hh all_sibling_old_hh all_sibling_young_hh ///
					uncle_hh, by(childid)
					
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh)
	gen alone_hh=cond(dropme==0,1,0) /*no presence of father, step father or grandparents*/
	drop dropme		

	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh uncle_hh)
	gen alone_hh2=cond(dropme==0,1,0) /*no presence of father, step father, grandparents, uncles or aunts*/
	drop dropme	
	
	merge 1:1 childid using "$aux\round5.dta"
	drop _merge
save "$aux\round5.dta",replace

*Proxy for time spent with child in the cases when mothers do not live in HH:
use "$inputs\pe_r5_ychh_householdrosterr5.dta" ,clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	gen mother=cond((relater5==1| relater5==3| relater5==24) & memsexr5==2,1,0)
	gen bio_father=cond((relater5==1| relater5==3| relater5==24) & memsexr5==1,1,0)
	keep if livhser5!=1 & livhser5!=77 & livhser5!=88
	keep if mother==1|bio_father==1
	recode seenmer5 (1 = 4 "Daily") (2 = 3 "Weekly") (7 = 2 "Every two weeks") (3 = 1 "Monthly") (4 5 6 = 0 "More than monthly") (77 88 = .), gen(seenmer5_recode)
	gen seemum_r5=cond(mother==0,.,seenmer5_recode)
	gen seedad_r5=cond(mother==1,.,seenmer5_recode)
	gen missing_dad=cond(bio_father==1 & seedad_r5==.,1,0)
	gen missing_mom=cond(mother==1 & seemum_r5==.,1,0)
	collapse (sum) seemum_r5 seedad_r5 missing_mom missing_dad mother bio_father, by(childid)
	replace seemum_r5=cond(missing_mom!=0|mother==0,.,seemum_r5)
	replace seedad_r5=cond(missing_dad!=0|bio_father==0,.,seedad_r5)
	drop missing_mom missing_dad mother bio_father
	merge 1:1 childid using "$aux\round5.dta"
	drop _merge
save "$aux\round5.dta",replace

*Shocks:
use "$inputs\pe_r5_ychh_shocks.dta" , clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	keep if shckidr5==40
	rename evntr5 hh_divorce
	egen hh_divorce_num=rowtotal(ev20*)
	keep childid hh_divorce hh_divorce_num
	merge 1:1 childid using "$aux\round5.dta"
	drop _merge
save "$aux\round5.dta", replace	
	
*Remittances:
use "$inputs\pe_r5_ychh_transferdebtremittances.dta" , clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	global remit  rcvlstr5 oftrcvr5 rcvamtr5
	foreach var in $remit{
	    recode `var' (-99 -88 -79 -77  =.), gen(`var'_recode)
		}
	gen remit_total=1
	gen remit_transf=cond(srmnidr5==8|srmnidr5==9,1,0)
	collapse (sum) remit_total rcvlstr5_recode oftrcvr5_recode rcvamtr5_recode, by(childid remit_transf)

	gen remit_transf_num=cond(remit_transf==1,oftrcvr5_recode,0) 	/*# of remittances in transfers received during the last 12 months*/
	gen remit_rest_num=cond(remit_transf==0,oftrcvr5_recode,0)		/*# of other types of remittances received during the last 12 months*/
	
	gen remit_transf_last=cond(remit_transf==1,rcvamtr5_recode,0) 	/*total amount of remittances in transfers received during the last 12 months*/
	gen remit_rest_last=cond(remit_transf==0,rcvamtr5_recode,0)		/*total amount of other types of remittances received during the last 12 months*/
	
	collapse (sum)  remit_transf_num remit_rest_num remit_transf_last remit_rest_last, by (childid)
 	gen remit_transf=cond(remit_transf_num>0,1,0)
	merge 1:1 childid using "$aux\round5.dta"
	drop _merge

*Final touches: ordering outcomes and relevant covariates:
	order childid house_moved, first
	
	gen outcomes=. 
	order outcomes  math_r5 ppvt_r5 test_lang_r5 ppvt_stdscre_r5 reading_r5 subjective_wellbeing_r5 cse_r5 cse2_r5 agency_r5 self_efficacy_r5 self_esteem_r5 pride_r5 self_estem_pride_r5 ///
		  peer_relationship_r5 parent_relationship_r5 gender_equality_r5 support_ppl_r5, after(house_moved)
	
	gen cov_hh_charac=.
	order cov_hh_charac seemum_r5 seedad_r5 moves moves_separ moves_separ_dum hhsize blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old ///
		  half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh alone_hh2, after(support_ppl_r5)
	
	gen cov_hh_econ=.
	order cov_hh_econ remit_transf_num remit_rest_num remit_transf_last remit_rest_last remit_transf econ_enter_poverty econ_exit_poverty econ_status_curr econ_status_past ///
		  econ_status_poor econ_status_struggle econ_status_worse econ_stay_poverty  econ_status_curr_child_r5 econ_status_past_child_r5 econ_status_struggle_child_r5 ///
		  econ_status_poor_child_r5 econ_status_worse_child_r5 econ_enter_poverty_child_r5 econ_exit_poverty_child_r5 econ_stay_poverty_child_r5 exp_pc_r5_log hq cd sv wi, after(grandparents_hh)
	
	gen cov_time_use=.
	order cov_time_use child_hrs_care_r5 child_hrs_chores_r5 child_hrs_play_r5 child_hrs_school_r5 child_hrs_sleep_r5 child_hrs_study_r5 child_hrs_work_r5, after(wi)
	
	gen cov_hh_risk=.
	order cov_hh_risk hh_divorce hh_divorce_num, after(child_hrs_work_r5)
	
	gen cov_caregiver=.
	order cov_caregiver ask_school subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg hlp_study_old_sibling hlp_study_mom hlp_study_father ///
		  hlp_study_nobody hlp_study_nobody2 hlp_study_days support_ppl_cg expec_support_child expect_age_child expect_age_earn expect_age_education expect_age_indp expect_age_leave ///
		  expect_age_marry expect_age_parent, after(hh_divorce_num)

	gen cov_health=.
	order cov_health zbfa_r5 zhfa_r5 fhfa_r5 fbfa_r5 bmi_r5, after(expect_age_parent)
	
	global variables house_moved outcomes cov_hh_charac moves moves_separ moves_separ_dum hhsize blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old ///
					 half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh  blood_sibling_old_hh blood_sibling_young_hh ///
					 half_sibling_old_hh half_sibling_young_hh all_sibling_old_hh all_sibling_young_hh cov_hh_econ remit_transf_num remit_rest_num alone_hh2 ///
					 remit_transf_last remit_rest_last remit_transf econ_enter_poverty econ_exit_poverty econ_status_curr econ_status_past econ_status_poor econ_status_struggle ///
					 econ_status_worse econ_stay_poverty hq cd sv wi cov_time_use cov_hh_risk hh_divorce hh_divorce_num cov_caregiver ask_school ///
					 subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg hlp_study_old_sibling hlp_study_mom hlp_study_father hlp_study_nobody hlp_study_nobody2 ///
					 hlp_study_days support_ppl_cg cov_health expec_support_child expect_age_child expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry ///
					 expect_age_parent
	
	foreach var in $variables{
		rename `var' `var'_r5
		}
save "$outputs\round5.dta",replace	
