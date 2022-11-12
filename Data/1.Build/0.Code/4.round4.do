
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\1.Inputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

*Child database: 
use "$inputs\pe_r4_ycch_youngerchild.dta" , clear
	rename *, lower
	*Non-cognitive outcomes & mechanisms:
		*Recoding:
			global recode  feay01r4 feay02r4 feay03r4 feay04r4 feay05r4 feay06r4 feay07r4 feay08r4 feay09r4 feay10r4 feay11r4 feay12r4 feay13r4 feay14r4 feay15r4 feay16r4 feay17r4 feay18r4 ///
							feay19r4 feay20r4 feay21r4 feay22r4 feay23r4 feay24r4 feay25r4 feay26r4 feay27r4 feay28r4 feay29r4 feay30r4 feay31r4 feay32r4 feay33r4 feay34r4
			foreach var in $recode{
				recode `var' (77 78 79 88 =.), gen(`var'_recode)
				}
			
			global recode  cashclr4 cashwkr4 cashshr4 ccltrgr4 cembbkr4 cwrunir4    ctryhdr4 cftrwrr4 cbrjobr4
			foreach var in $recode{
				recode `var' (77 78 79 88 =.), gen(`var'_recode)
				}
			
			global recode cpldecr4 cnochcr4
			foreach var in $recode{
				recode `var' (1=5 "Strongly disagree") (2=4 "Disagree") (4=2 "Agree") (5=1 "Strongly agree") (77 78 79 88 =.), gen(`var'_recode)
				}
		
		*Subjective wellbeing:
			recode stnprsr4 (77 78 79 88 =.), gen(subjective_wellbeing)
		
		*Relationship with peers: self-concept (Original source: Self description questionnaire-SDQ- Marsh, Relich & Smith, 1981)
			global peer_relationship feay02r4_recode feay09r4_recode feay12r4_recode feay16r4_recode feay20r4_recode feay24r4_recode feay31r4_recode feay34r4_recode  
			alpha $peer_relationship, item /*good internal consistency*/
			foreach var in $peer_relationship{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			egen peer_relationship=rowmean(*_z)
			drop *_z
			
		*Self-efficacy: (original source: Generalised Self-Efficacy Scale-GSE-Jerusalem & Schwarzer,1992)
			global self_efficacy feay01r4_recode feay05r4_recode feay08r4_recode feay11r4_recode feay15r4_recode feay18r4_recode feay22r4_recode feay26r4_recode feay28r4_recode feay32r4_recode
			alpha $self_efficacy, item /*regular internal consistency*/
			foreach var in $self_efficacy{
				sum `var'
				gen `var'_sef_z=(`var'-r(mean))/r(sd)
				}
			egen self_efficacy=rowmean(*_sef_z)
			
		*Self-esteem:(original source: Rosenberg Scale)
			global self_esteem feay04r4_recode feay06r4_recode feay14r4_recode feay17r4_recode feay23r4_recode feay27r4_recode feay33r4_recode feay30r4_recode 
			alpha $self_esteem, item /*good internal consistency*/
			foreach var in $self_esteem{
				sum `var'
				gen `var'_ses_z=(`var'-r(mean))/r(sd)
				}
			egen self_esteem=rowmean(*_ses_z)
		
		*Agency: building on the concepts of locus of control proposed by Rotter, 1966 and self-efficacy by Bandura, 1993
			global agency cpldecr4_recode cnochcr4_recode ctryhdr4_recode cftrwrr4_recode cbrjobr4_recode
			alpha $agency, item /*low internal consistency*/
			foreach var in $agency{
				sum `var'
				gen `var'_ag_z=(`var'-r(mean))/r(sd)
				}
			egen agency=rowmean(*_ag_z)
			
			/*Core self-evaluation (Judge, Locke, and Durham (1997)): combination of agency, self-efficacy, and self-esteem. Following Chang et al. (2021), it will be constructed using
			Principal Component Analysis (PCA): "A principal component analysis confirms that [in the Indian case] items from all three scales load to the first factor which has an eigenvalue
			of 3.62 and explains 85% of the total variation... [W]e use the first factor emerging from the principal component analysis as a measure of the latent CSE personality traits"*/
				alpha *_sef_z *_ses_z *_ag_z, item /*good internal validity*/
				pca *_sef_z *_ses_z *_ag_z,components (4) /*first component explains only 21% of the total variance*/
				screeplot, mean /*however, the eigenvalues drop drastically after the first component*/
				
				predict cse, score /*PCA only works with complete data, thus 216 obs. are turned into missing*/
				
				egen cse2=rowmean(*_sef_z *_ses_z *_ag_z) /*calculating CSE as the mean of the items in order not to lose the 216 obs. This was done considering the high internal reliability.*/
				drop *_sef_z *_ag_z

		*Pride index: 
			global pride cashclr4_recode cashwkr4_recode cashshr4_recode ccltrgr4_recode cembbkr4_recode cwrunir4_recode
			alpha $pride, item
			foreach var in $pride{
				sum `var'
				gen `var'_pr_z=(`var'-r(mean))/r(sd)
				}
			egen pride=rowmean(*_pr_z)
			
			egen self_estem_pride=rowmean(*_ses_z *_pr_z)
			drop *_ses_z *_pr_z
			
		*Relationship with parents: self-concept (Original source: Self description questionnaire-SDQ- Marsh, Relich & Smith, 1981)
			global parent_relationship feay03r4_recode feay07r4_recode  feay10r4_recode  feay13r4_recode feay19r4_recode feay21r4_recode feay25r4_recode feay29r4_recode
			alpha $parent_relationship, item /*good internal consistency*/
			foreach var in $parent_relationship{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			egen parent_relationship=rowmean(*_z)
			drop *_z
	
	*Time use:
		global time_use sleepr4 crothr4 dmtskr4 tsfarmr4 actpayr4 atschr4 studygr4 lsurer4
		foreach var in $time_use{
			recode `var' (77 78 79 88 = .), gen (`var'_recode)
			}
		
		egen child_hrs_work=rowtotal (tsfarmr4_recode actpayr4_recode) /*child: hours working in a typical day*/
		rename (sleepr4_recode crothr4_recode dmtskr4_recode atschr4_recode studygr4_recode lsurer4_recode) ///
			   (child_hrs_sleep child_hrs_care child_hrs_chores child_hrs_school child_hrs_study child_hrs_play)
	
	*Support
		recode pplrelr4 (77 78 79 88 =.), gen(support_ppl)
	
	*Child perception of economic status:	
		recode hhnowr4 (1 = 5 "Rich") (2 = 4 "Comfortable") (3 = 3 "Struggle") (4 = 2 "Poor") (5 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_curr_child) /*Current economic status*/
		recode hh4yrr4 (1 = 5 "Rich") (2 = 4 "Comfortable") (3 = 3 "Struggle") (4 = 2 "Poor") (5 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_past_child) /*Past economic status (4 yrs*/
		
		gen econ_status_struggle_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child==3,1,0))								/*Current economic status: struggling*/
		gen econ_status_poor_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child<3,1,0))										/*Current economic status: poor*/
		gen econ_status_worse_child=cond(econ_status_curr_child==.,.,cond(econ_status_curr_child<econ_status_past_child,1,0))				/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==1 & econ_status_past_child>2,1,0))	/*vs 4 yrs-HH entered poverty*/
		gen econ_exit_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==0 & econ_status_past_child<3,1,0))	/*vs 4 yrs-HH exited to poverty*/
		gen econ_stay_poverty_child=cond(econ_status_curr_child==.|econ_status_past_child==.,.,cond(econ_status_poor_child==1 & econ_status_past_child<3,1,0))	/*vs 4 yrs-HH stayed in poverty*/
	
	 *Education aspirations
	 recode cldstdr4 (77 79 =.), gen(cldstdr4_recode)
	 gen college_aspiration=cond(cldstdr4_recode==16|cldstdr4_recode==19,1,0)
	 
	 *Rename variables:
	 global variables  peer_relationship self_efficacy self_esteem pride self_estem_pride parent_relationship agency subjective_wellbeing  bmi zhfa zbfa fhfa fbfa  child_hrs_sleep ///
						child_hrs_care child_hrs_chores child_hrs_school child_hrs_study child_hrs_play child_hrs_work support_ppl  econ_status_curr_child econ_status_past_child ///
						econ_status_struggle_child econ_status_poor_child econ_status_worse_child econ_enter_poverty_child econ_exit_poverty_child econ_stay_poverty_child cse cse2 ///
						college_aspiration
	 keep childcode $variables
	 foreach var in $variables{
	     rename `var' `var'_r4
		}	
save "$aux\round4.dta",replace	

*Child database: cognitive outcomes
use "$inputs\pe_r4_yccog_youngerchild.dta" , clear
	rename *, lower
	
	recode stdscre (-888 -99 -88 -79 =.), gen(ppvt_stdscre_r4)
	
	rename (lang_raw maths_raw ppvt_raw testlang) (lang_r4 math_r4 ppvt_r4 test_lang_r4)
	keep childid childcode lang_r4 math_r4 ppvt_r4 test_lang_r4 ppvt_stdscre_r4
	merge 1:1 childcode using "$aux\round4.dta"
	drop _merge
	order childid, first
save "$aux\round4.dta",replace	

*Child mobility database:
use "$inputs\pe_r3_childlevel8yrold.dta" , clear
	keep childid cdintyr
	merge 1:1 childid using "$aux\round4.dta"
	drop _merge
save "$aux\round4.dta",replace

use "$inputs\pe_r4_ychh_childmobility.dta" , clear
	rename *, lower
	gen moves=1
	gen moves_separ=cond(rsnmvr41==4|rsnmvr42==4|rsnmvr41==5|rsnmvr42==5,1,0)

	collapse (sum)  moves moves_separ, by (childcode)
	gen house_moved=1
	gen moves_separ_dum=cond(moves_separ>0,1,0)
	gen childid="PE"+string(childcode, "%06.0f")
	merge 1:1 childid using  "$aux\round4.dta"
	global move moves moves_separ house_moved moves_separ_dum
	foreach var in $move{
	    replace `var'=cond(_merge==2,0,`var')
		}
	drop _merge
save "$aux\round4.dta",replace

*HH database:	
use "$inputs\pe_r4_ychh_youngerhousehold.dta" , clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	recode finhlpr4 (76 77 78 79 88 99 =.), gen(finhlpr4_recode)
	rename (srsdbtr4 finhlpr4_recode ownhser4 bulldr4) (debt support_ppl_cg own_house_r4 bullied_r4)
	
	*Economic status:	
		recode dsccurr4 (1 2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_curr) /*Current economic status*/
		recode dscagor4 (1 2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute") (77 78 79 88 =.), gen(econ_status_past) /*Past economic status (4 yrs ago)*/
		
		gen econ_status_struggle=cond(econ_status_curr==.,.,cond(econ_status_curr==3,1,0))											/*Current economic status: struggling*/
		gen econ_status_poor=cond(econ_status_curr==.,.,cond(econ_status_curr<3,1,0))												/*Current economic status: poor*/
		gen econ_status_worse=cond(econ_status_curr==.,.,cond(econ_status_curr<econ_status_past,1,0))								/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past>2,1,0))	/*vs 4 yrs-HH entered poverty*/
		gen econ_exit_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==0 & econ_status_past<3,1,0))	/*vs 4 yrs-HH exited to poverty*/
		gen econ_stay_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past<3,1,0))	/*vs 4 yrs-HH exited stayed overty*/
		
	*Caregiver:
		*Wellbeing: 
			recode ladderr4 (77 78 79 88 = .), gen(subjective_wellbeing_cg_curr)										/*current future subjective wellbeing*/
			recode farladr4 (77 78 79 88 = .), gen(subjective_wellbeing_cg_fut)											/*perceived future subjective wellbeing*/
			*egen satisfaction_cg=rowmean( stslvgr3 stshthr3 stsachr3 stsrltr3 stssfer3 stscomr3 stssecr3 stsrlgr3)		/*satisfaction with elementos of life*/
			
		*Expectations from child:
			global expect  livclsr4 fnassbr4 hlphser4 fnasyur4 cresibr4 creyour4 emtsupr4 
			foreach var in $expect{
				recode `var' ( -88 -79 -77 76 77 78 79 88 99 277 288 303 =.), gen(`var'_recode)
				}
			
			global age  ernmnyr4 lveedcr4 finindr4 lvehser4 getmarr4 hvechdr4
			foreach var in $age{
				recode `var' ( -88 -79 -77 0 3 5 76 77 78 79 88 99 277 288 303 =.), gen(`var'_recode)
				}
			
			egen expec_support_child=rowmean( livclsr4_recode fnassbr4_recode hlphser4_recode fnasyur4_recode cresibr4_recode creyour4_recode emtsupr4_recode)
			egen expect_age_child=rowmean(ernmnyr4_recode lveedcr4_recode finindr4_recode lvehser4_recode getmarr4_recode hvechdr4_recode)
			
			rename ( ernmnyr4_recode lveedcr4_recode finindr4_recode lvehser4_recode getmarr4_recode hvechdr4_recode) ///
					(expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent)
			
			recode grdlker4 (79 88 =.), gen(grdlker4_recode)
			gen cg_college_expectations=cond(grdlker4_recode==.,.,cond(grdlker4_recode==16|grdlker4_recode==19,1,0))
			
		*Dedication to child:
			egen know_school=rowmean(nmfrndr4 aftrscr4 prntfrr4 tchrr4) /*knowledge of child's school peers and parents*/
			
			recode askschr4 (77 78 79 88 =.), gen(ask_school) 			/*times p/week caregiver asked the child about school*/
	
		*Agency and pride:
			global recode cag1r4 cag2r4 cps2r4 cps3r4 cps4r4 cps5r4  /*recoding items that make the agency and pride indices*/
			foreach var in $recode{
				recode `var' (77 78 79 88 =.), gen(`var'_recode)
				}
			
			recode cag5r4 (1 = 5 "Strongly disagree") (2 = 4 "Disagree") (4 =2 "Agree") (5 = 1 "Strongly agree") (77 78 79 88 =.), gen(cag5r4_recode)
			
			*Agency:
				global agency cag1r4_recode cag2r4_recode cag5r4_recode
				alpha $agency, item /*very low internal consistency*/
				foreach var in $agency{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen agency_cg=rowmean(*_z)
				drop *_z
		
			*Pride index: 
				global pride cps2r4_recode cps3r4_recode cps4r4_recode cps5r4_recode  
				alpha $pride, item
				foreach var in $pride{
					sum `var'
					gen `var'_pr_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen pride_cg=rowmean(*_pr_z)
				drop *_z

		*Helping with homeworks:
			recode hlphmwr4 ( 77 78 79 88 = .), gen(hlphmwr4_recode)
			gen hlp_study_old_sibling=cond(hlphmwr4==5,1,0) 			/*older siblings*/
			gen hlp_study_mom=cond(hlphmwr4==3,1,0)						/*mother*/
			gen hlp_study_father=cond(hlphmwr4==2,1,0)					/*father*/
			gen hlp_study_nobody=cond(hlphmwr4==1,1,0)					/*nobody helps the child to study*/
			gen hlp_study_nobody2=cond(hlphmwr4_recode==.,.,cond(hlphmwr4==1,1,0))			/*nobody helps the child to study*/
			
			rename numhlpr4 hlp_study_days /*# of days caregiver helped the child with the homework during last week of classes*/
			replace hlp_study_days =cond(hlp_study_nobody==1,0,hlp_study_days)
		*Mental health:
			rename mcrymrr4 cry_cg /*cried more than usual during the last 30 days*/
	
	gen exp_pc_r4_log=log(totalexp)
	
	keep childid childcode debt support_ppl_cg econ_status_curr econ_status_past econ_status_struggle econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty ///
		 econ_stay_poverty know_school subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg know_school ask_school hlp_study_old_sibling hlp_study_mom ///
		 hlp_study_father hlp_study_nobody hlp_study_nobody2 hlp_study_days cry_cg exp_pc_r4_log hhsize hq cd sv wi bullied_r4 expec_support_child expect_age_child rptgrdr4 ///
		 cg_college_expectations expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent
	
	merge 1:1 childid using "$aux\round4.dta"
	summarize if _merge==2 /*they seem to be children who did not participate in the survey round*/
	drop _merge
save "$aux\round4.dta", replace

*Household composition database:
use "$inputs\pe_r4_ychh_householdrosterr4.dta" ,clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	gen mother=cond((relater4==1| relater4==3| relater4==24) & memsexr4==2,1,0)
	gen bio_father=cond((relater4==1| relater4==3| relater4==24) & memsexr4==1,1,0)
	gen stepfather=cond(relater4==2 & memsexr4==1,1,0)
	gen grandparent=cond(relater4==5|relater4==6,1,0)
	gen blood_sibling=cond(relater4==7,1,0)
	gen half_sibling=cond(relater4==8|relater4==9|relater4==10|relater4==11|relater4==12,1,0)
	gen all_sibling=cond(blood_sibling==1|half_sibling==1,1,0)
	
	gen dropme=cond(memidr4==0,memager4,.)
	bys childid: egen dropme2=max(dropme)
	
	gen blood_sibling_old=cond(blood_sibling==1 & memager4>dropme2,1,0)
	gen blood_sibling_young=cond(blood_sibling==1 & memager4<dropme2,1,0)
	
	gen half_sibling_old=cond(half_sibling==1 & memager4>dropme2,1,0)
	gen half_sibling_young=cond(half_sibling==1 &memager4<dropme2,1,0)
	
	gen all_sibling_old=cond(all_sibling==1 & memager4>dropme2,1,0)
	gen all_sibling_young=cond(all_sibling==1 & memager4<dropme2,1,0)
	
	gen uncle=cond(relater4==13,1,0)
	
	drop dropme*
	
	gen mother_hh=cond(mother==1 & livhser4==1,1,0)				/*mother still in hh*/
	gen bio_father_hh=cond(bio_father==1 & livhser4==1,1,0)		/*father still in hh*/
	gen step_father_hh=cond(stepfather==1 & livhser4==1,1,0)	/*stepfather still in hh*/
	gen grandparents_hh=cond(grandparent==1 & livhser4==1,1,0)	/*grandparent still in hh*/
	gen uncle_hh=cond(uncle==1 & livhser4==1,1,0)				/*uncle/aunt still in hh*/
	
	gen blood_sibling_old_hh=cond(blood_sibling_old==1 & livhser4==1,1,0)
	gen blood_sibling_young_hh=cond(blood_sibling_young==1 & livhser4==1,1,0)
	gen half_sibling_old_hh=cond(half_sibling_old==1 & livhser4==1,1,0)
	gen half_sibling_young_hh=cond(half_sibling_young==1 & livhser4==1,1,0)
	gen all_sibling_old_hh=cond(all_sibling_old==1 & livhser4==1,1,0)
	gen all_sibling_young_hh=cond(all_sibling_young==1 & livhser4==1,1,0)

	gen job1=cond(!inlist(impac1r4, 7, 15, 18, 20) & regjb1r4==1,1,0)
	gen job2=cond(!inlist(impac2r4, 7, 15, 18, 20) & regjb2r4==1,1,0)
	gen job3=cond(!inlist(impac3r4, 7, 15, 18, 20) & regjb3r4==1,1,0)
	
	egen mom_jobs=rowtotal(job*)
	replace mom_jobs=cond(mother==0|livhser4!=1,.,mom_jobs)	/*missing obs. come predominantly from mothers not living in HH. */
	
	summarize if livhser4!=1 /*in fact, most information is missing on those cases. We will have to take mom_works as proxy for time spent with child only of those mothers living in HH and 
								look for another measure for mother living outside HH*/
	
	gen mom_works=cond(mother==0|mom_jobs==.,.,cond(mom_jobs!=0,1,0))
	
	*Work hours:
		gen dropme1=cond(mother==0,.,cond(mom_works==0,0,cond(job1==1,hours1r4*dysmt1r4*month1r4,0)))
		gen dropme2=cond(mother==0,.,cond(mom_works==0,0,cond(job2==1,hours2r4*dysmt2r4*month2r4,0)))
		gen dropme3=cond(mother==0,.,cond(mom_works==0,0,cond(job3==1,hours3r4*dysmt3r4*month3r4,0)))
		egen mom_works_total=rowtotal(dropme*)
		replace mom_works_total=cond(mother==0|mom_jobs==.,.,mom_works_total)
		drop dropme*
	
	collapse (max)  blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh ///
					step_father_hh grandparents_hh mom_works mom_jobs mom_works_total blood_sibling_old_hh blood_sibling_young_hh half_sibling_old_hh half_sibling_young_hh all_sibling_old_hh ///
					all_sibling_young_hh uncle_hh, by(childid)
					
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh)
	gen alone_hh=cond(dropme==0,1,0) /*no presence of father, step father or grandparents*/
	drop dropme

	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh uncle_hh)
	gen alone_hh2=cond(dropme==0,1,0) /*no presence of father, step father, grandparents, uncles or aunts*/
	drop dropme
	
	merge 1:1 childid using "$aux\round4.dta"
	drop _merge
save "$aux\round4.dta",replace

*Proxy for time spent with child in the cases when mothers do not live in HH:
use "$inputs\pe_r4_ychh_householdrosterr4.dta" ,clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	gen mother=cond((relater4==1| relater4==3| relater4==24) & memsexr4==2,1,0)
	gen bio_father=cond((relater4==1| relater4==3| relater4==24) & memsexr4==1,1,0)
	keep if livhser4!=1 & livhser4!=77 & livhser4!=88
	keep if mother==1|bio_father==1
	recode seenmer4 (1 = 4 "Daily") (2 = 3 "Weekly") (7 = 2 "Every two weeks") (3 = 1 "Monthly") (4 5 6 = 0 "More than monthly") (88 = .), gen(seenmer4_recode)
	gen seemum_r4=cond(mother==0,.,seenmer4_recode)
	gen seedad_r4=cond(mother==1,.,seenmer4_recode)
	gen missing_dad=cond(bio_father==1 & seedad_r4==.,1,0)
	gen missing_mom=cond(mother==1 & seemum_r4==.,1,0)
	collapse (sum) seemum_r4 seedad_r4 missing_mom missing_dad mother bio_father, by(childid)
	replace seemum_r4=cond(missing_mom!=0|mother==0,.,seemum_r4)
	replace seedad_r4=cond(missing_dad!=0|bio_father==0,.,seedad_r4)
	drop missing_mom missing_dad mother bio_father
	merge 1:1 childid using "$aux\round4.dta"
	drop _merge
save "$aux\round4.dta",replace
	
*Shocks:
use "$inputs\pe_r4_ychh_shocks.dta" , clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	keep if shckidr4==40
	rename evntr4 hh_divorce
	egen hh_divorce_num=rowtotal(ev20*)
	keep childid hh_divorce hh_divorce_num
	merge 1:1 childid using "$aux\round4.dta"
	drop _merge
save "$aux\round4.dta", replace	
	
*Remittances:
use "$inputs\pe_r4_ychh_transferdebtremittances.dta" , clear
	rename *, lower
	gen childid="PE"+string(childcode, "%06.0f")
	order childid, first
	global remit  rcvlstr4 oftrcvr4 rcvamtr4
	foreach var in $remit{
	    recode `var' (-99 -88 -79 -77  =.), gen(`var'_recode)
		}
	gen remit_total=1
	gen remit_transf=cond(srmnidr4==8|srmnidr4==9,1,0)
	collapse (sum) remit_total rcvlstr4_recode oftrcvr4_recode rcvamtr4_recode, by(childid remit_transf)

	gen remit_transf_num=cond(remit_transf==1,oftrcvr4_recode,0) 	/*# of remittances in transfers received during the last 12 months*/
	gen remit_rest_num=cond(remit_transf==0,oftrcvr4_recode,0)		/*# of other types of remittances received during the last 12 months*/
	
	gen remit_transf_last=cond(remit_transf==1,rcvamtr4_recode,0) 	/*total amount of remittances in transfers received during the last 12 months*/
	gen remit_rest_last=cond(remit_transf==0,rcvamtr4_recode,0)		/*total amount of other types of remittances received during the last 12 months*/
	
	collapse (sum)  remit_transf_num remit_rest_num remit_transf_last remit_rest_last, by (childid)
 	gen remit_transf=cond(remit_transf_num>0,1,0)
	merge 1:1 childid using "$aux\round4.dta"
	
	gen attrition_r4=cond(_merge==2,1,0)
	drop _merge

*Final touches, ordering outcomes and relevant covariates:
	order childid attrition_r4 house_moved, first
	
	gen outcomes=. 
	order outcomes  lang_r4 math_r4 ppvt_r4 ppvt_stdscre_r4 subjective_wellbeing_r4 peer_relationship_r4 cse_r4 cse2_r4 self_efficacy_r4 self_esteem_r4 pride_r4 self_estem_pride_r4 ///
		  parent_relationship_r4 agency_r4 bullied_r4 support_ppl_r4, after(house_moved)
	
	gen cov_hh_charac=.
	order cov_hh_charac seemum_r4 seedad_r4 moves moves_separ moves_separ_dum hhsize blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old ///
		  half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh step_father_hh grandparents_hh blood_sibling_old_hh blood_sibling_young_hh half_sibling_old_hh ///
		  half_sibling_young_hh all_sibling_old_hh all_sibling_young_hh alone_hh alone_hh2 mom_works mom_jobs mom_works_total college_aspiration_r4 rptgrdr4, after(support_ppl_r4)
	
	gen cov_hh_econ=.
	order cov_hh_econ debt remit_transf_num remit_rest_num remit_transf_last remit_rest_last remit_transf econ_enter_poverty econ_exit_poverty econ_status_curr econ_status_past ///
		  econ_status_poor econ_status_struggle econ_status_worse econ_stay_poverty  econ_status_curr_child_r4 econ_status_past_child_r4 econ_status_struggle_child_r4 ///
		  econ_status_poor_child_r4 econ_status_worse_child_r4 econ_enter_poverty_child_r4 econ_exit_poverty_child_r4 econ_stay_poverty_child_r4  exp_pc_r4_log hq cd sv wi, ///
		  after(grandparents_hh)
	
	gen cov_time_use=.
	order cov_time_use child_hrs_care_r4 child_hrs_chores_r4 child_hrs_play_r4 child_hrs_school_r4 child_hrs_sleep_r4 child_hrs_study_r4 child_hrs_work_r4, after(wi)
	
	gen cov_hh_risk=.
	order cov_hh_risk hh_divorce hh_divorce_num, after(child_hrs_work_r4)
	
	gen cov_caregiver=.
	order cov_caregiver  know_school ask_school subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg hlp_study_old_sibling hlp_study_mom hlp_study_father ///
		  hlp_study_nobody hlp_study_nobody2 hlp_study_days support_ppl_cg expec_support_child expect_age_child expect_age_earn expect_age_education expect_age_indp expect_age_leave ///
		  expect_age_marry expect_age_parent cry_cg cg_college_expectations, after(hh_divorce_num)

	gen cov_health=.
	order cov_health zbfa_r4 zhfa_r4 fhfa_r4 fbfa_r4 bmi_r4, after(cry_cg)
	
	global variables house_moved outcomes cov_hh_charac moves moves_separ moves_separ_dum hhsize blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old ///
					 half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh  blood_sibling_old_hh blood_sibling_young_hh ///
					 half_sibling_old_hh half_sibling_young_hh all_sibling_old_hh all_sibling_young_hh cov_hh_econ debt remit_transf_num remit_rest_num ///
					 remit_transf_last remit_rest_last remit_transf econ_enter_poverty econ_exit_poverty econ_status_curr econ_status_past econ_status_poor econ_status_struggle ///
					 econ_status_worse econ_stay_poverty hq cd sv wi cov_time_use cov_hh_risk hh_divorce hh_divorce_num cov_caregiver know_school ask_school ///
					 subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg hlp_study_old_sibling hlp_study_mom hlp_study_father hlp_study_nobody hlp_study_nobody2 ///
					 hlp_study_days support_ppl_cg cry_cg cov_health mom_works mom_jobs mom_works_total expec_support_child expect_age_child rptgrdr4 cg_college_expectations ///
					 expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent alone_hh2
	
	foreach var in $variables{
		rename `var' `var'_r4
		}
save "$outputs\round4.dta",replace	


