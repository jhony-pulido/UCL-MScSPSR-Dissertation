
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\1.Inputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

*Child database:
use "$inputs\pe_r2_childlevel5yrold.dta" , clear
	*Identify attrition:
		gen attrition_r2=cond(situac_r2!=1,1,0)
	
	*Child moving:
		gen house_moved=cond(childliv==.,.,cond(childliv==1,0,1))				/*child has moved to another house since birth*/
		gen dropme=longliv*12 													/*transforming the variable to months*/
		gen house_months=cond(longliv!=. & pelngliv!=.,dropme+pelngliv,.)		/*num. of months child has been living in the place*/
		
		drop dropme
	
	*Expenditure:
		gen dropme=cond(pebps03==1|pebps04==1|pebps04a==1,1,0) 		
		egen dropme2=rownonmiss(pebps03 pebps04 pebps04a)
		gen exp_clothes=cond(dropme==0,0,cond(dropme2==0,.,cond((spname03==. & spname04==. & spnam04a==.)|(spname03==0 & spname04==0 & spnam04a==.),0,1)))	/*spent money on clothes for child*/
		drop dropme* 
		
		gen dropme=cond(pebps07==1|pebps08==1,1,0) 		
		egen dropme2=rownonmiss(pebps07 pebps08)
		gen exp_footwear=cond(dropme2==0| (dropme==1 & spname07==. & spname08==.),.,cond(dropme==0|(spname07==0 & spname08==0),0,1))	/*spent money on footwear for child*/
		drop dropme* 
		
		gen exp_private_classes=cond(pebps13a==. & spnam13a==.,.,cond(spnam13a!=0 & spnam13a!=.,1,0))   /*spent money on private classes for child*/
		gen exp_books=cond(pebps15==. & spname15==.,.,cond(spname15!=0 & spname15!=.,1,0))  			/*spent money on books & stationary for child*/
		
		gen exp_med_cons=cond(pebps17==. & spname17==.,.,cond(spname17!=0 & spname17!=.,1,0))  			/*spent money on medical consultations for child*/
		gen exp_medicines=cond(pebps18==. & spname18==.,.,cond(spname18!=0 & spname18!=.,1,0))  		/*spent money on medicines for child*/
		
		gen exp_entertainment=cond(pebps19==. & spname19==.,.,cond(spname19!=0 & spname19!=.,1,0))  	/*spent money on entertainment for child*/
		gen exp_presents=cond(pebps20==. & spname20==.,.,cond(spname20!=0 & spname20!=.,1,0))  			/*spent money on presents for child*/
	
	*Food security:
		gen food_situation_bad=cond(pe4c14==.,.,cond(pe4c14>2,1,0))										/*food situation: sometimes or frequently not eating enough*/
		gen food_worry=cond(pe4c15==.,.,cond(pe4c15<4,1,0))												/*at least sometimes worried to run out of food before being able to get more*/
		gen food_worry_always=cond(pe4c15==.,.,cond(pe4c15==1,1,0))										/*almost always worried to run out of food before being able to get more*/
		gen food_not_enough=cond(pe4c16==.,.,cond(pe4c16<4,1,0))										/*at least sometimes food was not enough*/
		gen food_not_enough_always=cond(pe4c16==.,.,cond(pe4c16==1,1,0))								/*almost always food was not enough*/
		gen food_variety_never=cond(pe4c17==.,.,cond(pe4c17==4,1,0))									/*not enough money to eat varied*/
		gen food_child_low_cost=cond(pe4c18==.,.,cond(pe4c18<4,1,0))									/*at least sometimes children had to eat low-cost food*/
		gen food_child_low_cost_always=cond(pe4c18==.,.,cond(pe4c18==1,1,0))							/*almost always children had to eat low-cost food*/
		gen food_child_varied=cond(pe4c19==.,.,cond(pe4c19<4,1,0))										/*at least sometimes children could not eat varied*/
		gen food_child_varied_never=cond(pe4c19==.,.,cond(pe4c19==1,1,0))								/*almost never children could eat varied*/
		gen food_child_enough=cond(pe4c20==.,.,cond(pe4c20<4,1,0))										/*at least sometimes children could not eat enough*/
		gen food_child_enough_never=cond(pe4c20==.,.,cond(pe4c20==1,1,0))								/*almost never children could eat enough*/
		
		egen food_hh_problems=rowmean(food_situation_bad food_worry food_not_enough food_variety_never)					/*general food problems in hh index*/ 
		egen food_security_child=rowmean(food_child_low_cost food_child_varied food_child_enough pe4c26 pe4c28 pe4c29)  /*food problems affecting child index*/
		egen food_security_caregiver=rowmean( pe4c22 pe4c23 pe4c24) 													/*food problems affecting caregiver*/

	*Support:
		gen support_material=cond(finhelp==.,.,cond(finhelp>1,1,0))			/*Do not have anyone to rely on for material support*/
		gen support_less=cond(relyon==.,.,cond(relyon==2,1,0))				/*Has less people to rely on for material support than 4 yrs ago */
		egen support_index=rowmean(support_material support_less anyhelp)   /*Support index*/
	
	*Important information regarding child:
		gen inf01=cond(getinf01==2,0,getinf01)
		gen inf03=cond(getinf03==2,0,getinf03)
		egen info_child_acess=rowmean(inf01 getinf02 inf03)			/*Have access to relevant information regarding child*/
		egen info_child_sought=rowmean(st3yr01 st3yr02 st3yr03)		/*Sought relevant information*/
		
		gen info_income=cond(getinf04==2,0,getinf04)	/*Have access to information of income earning opportunities*/
	
	*Economic status & changes:
		recode circum01 (2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute"), gen(econ_status_curr) /*Current economic status*/
		recode circum02 (2 = 5 "Rich") (3 = 4 "Comfortable") (4 = 3 "Struggle") (5 = 2 "Poor") (6 = 1 "Destitute"), gen(econ_status_past) /*Current economic status*/
		
		gen econ_status_struggle=cond(econ_status_curr==.,.,cond(econ_status_curr==3,1,0))											/*Current economic status: struggling*/
		gen econ_status_poor=cond(econ_status_curr==.,.,cond(econ_status_curr<3,1,0))												/*Current economic status: poor*/
		gen econ_status_worse=cond(econ_status_curr==.,.,cond(econ_status_curr<econ_status_past,1,0))								/*Current economic status is worse than 4 yrs ago*/
		gen econ_enter_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past>2,1,0))	/*4 yrs comparison: HH entered poverty*/
		gen econ_exit_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==0 & econ_status_past<3,1,0))	/*4 yrs comparison: HH exited to poverty*/
		gen econ_stay_poverty=cond(econ_status_curr==.|econ_status_past==.,.,cond(econ_status_poor==1 & econ_status_past<3,1,0))	/*4 yrs comparison: HH exited stayed in poverty*/
		
		egen crime_victim=rowmean( event01 event02 event03 event04 event05)									/*crime victim index*/
	
	*Remittances:
		egen remit_transf_num=rowtotal(remno09 remno13 remno14) 	/*# of remittances in transfers received during the last 12 months*/
		egen remit_rest_num=rowtotal(remno01 remno02 remno05 remno06  remno11 remno12 remno15 remno16 remno17 remno18 remno19 remno20 remno21) 
																	/*# of other types of remittances received during the last 12 months*/
		
		egen remit_transf_last=rowtotal(remamt09 remamt13 remamt14 ) 	/*total amount of last remittances in transfers received during the last 12 months*/
		egen remit_rest_last=rowtotal( remamt01 remamt02 remamt05 remamt06  remamt11 remamt12 remamt15 remamt16 remamt17 remamt18 remamt19 remamt20 remamt21)
																		/*total amount of last other types of remittances received during the last 12 months*/
		
		gen remit_transf=cond(remit_transf_num>0,1,0)
		
		rename (remit09 remno09 remamt13) (remit_alimony remit_alimony_num remit_alimony_amount)

	*Childcare, education, health:
		gen carer3y_father=cond(hmcare1==2| hmcare2==2| hmcare3==2,1,0)													/*Father as one of the three main carers*/
		gen carer3y_grandparents=cond((hmcare1==3|hmcare1==4)| (hmcare2==3|hmcare2==4)| (hmcare3==3|hmcare3==4),1,0)	/*Grandparents as one of the three main carers*/
		egen carer3y_num=rownonmiss(hmcare1 hmcare2 hmcare3) 															/*Num. of main carers (max. 3)*/
			
		gen edu_private_expect=cond(qualsch==1,1,cond(qualsch==.,.,0))														/*Child is expected to attend private school*/
		gen edu_good_qual_expect=cond(whysch1==5|whysch2==5|whysch3==5,1,cond(whysch1==. & whysch2==. & whysch3==.,.,0))	/*Decision based on the importance given to education quality*/
		gen edu_trav_no_adult=cond(travalon==1,0,cond(travalon==.,.,1))														/*Children would travel alone to school*/
		
		egen health_vaccines=rowtotal(bcg measles dpt opv hib pehvb) /*Num. of vaccines the child has*/
		egen dropme=rownonmiss(bcg measles dpt opv hib pehvb)
		replace health_vaccines=cond(dropme==0,.,health_vaccines)
		drop dropme
		
	*Community:
		global area csv1 csv2 csv3 csv4 csv5 ctr1 ctr2 ctr4 ctr5 ctr6 
		foreach var in $area{
			recode `var' (0 = 0 "No") (1 = 2 "Yes") (2 = 1 "Some doubts"), gen(`var'_recode)
			}
		
		egen area_qual=rowmean(*_recode)				/*Quality of area*/
	 
	*Caregiver perceptions
		*Recoding:
			global recode cag1 cag2 cps1 cps3 cps4 cps5 csd1
			foreach var in $recode{
				recode `var'  (0 = 0 "Disagree") (1 = 2 "Agree") (2 = 1 "Some doubt") (77 78 79 =.), gen(`var'_recode)
				}
			
			global recode  cag3 cag4 cag5 cps2  csd2 csd3
			foreach var in $recode{
				recode `var'  (0 = 2 "Disagree") (1 = 0 "Agree") (1 = 1 "Some doubt") (77 78 79 =.), gen(`var'_recode)
				}	
		
		*Subjective Wellbeing:
			rename (ladder farlad ) (subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut)  
			
		*Agency:
			global agency cag1_recode cag2_recode cag3_recode cag4_recode cag5_recode
			foreach var in $agency{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen agency_cg=rowmean(*_z)
			drop *_z
		
		*Proud index:
			global pride cps1_recode cps2_recode cps3_recode cps4_recode cps5_recode
			foreach var in $pride{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen pride_cg=rowmean(*_z)
			drop *_z	
		
		*Self-respect and inclusion:
			global inclusion csd1_recode csd2_recode csd3_recode
			foreach var in $inclusion{
				sum `var'
				gen `var'_z=(`var'-r(mean))/r(sd)
				}
			alpha *_z, item std
			egen inclusion_cg=rowmean(*_z)
			drop *_z
		
		*Locus of control:
			egen dropme=rownonmiss(hlpmove1 hlpmove2 hlpmove3)
			gen percp_locus_child_prog_cg=cond(dropme==0,.,cond(hlpmove1==2|hlpmove2==2|hlpmove3==2,1,0))	/*Internal locus of control: dependence of child*/
			gen percp_child_edu_cg=cond(gradlike==16 & expgrade==1,1,cond(gradlike==.|expgrade==.,.,0))	/*Perception of child education in the future*/
			drop dropme
		*Perception of home importance:
			global home_importance  impind imphdwk impresp impima imptol impthri impdet impunsel impob impsis imprpas imppart impcreat impdream
				foreach var in $home_importance{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen home_importance_cg=rowmean(*_z)
				drop *_z

		*Values attached to children (Bulatao, 1979):
			*Instrumental assistance:
				global instrumental nereas1 nereas2 nereas3 nereas4 nereas5 nereas6
				foreach var in $instrumental{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen instrumental_assistance_cg=rowmean(*_z)
				drop *_z
				
			*Psychological appreciation:
				global appreciation epreas1 epreas2 epreas3  epreas4 epreas5 
				foreach var in $appreciation{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen psychological_appreciation_cg=rowmean(*_z)
				drop *_z				
				
			*Rewarding interaction:
				global rewarding inreas1 inreas2 inreas3
				foreach var in $rewarding{
					sum `var'
					gen `var'_z=(`var'-r(mean))/r(sd)
					}
				alpha *_z, item std
				egen rewarding_interaction_cg=rowmean(*_z)
				drop *_z	

		*Expectations from child:
			global age  expearn expedu expind expleav expmar expchild
			foreach var in $age{
				recode `var' ( -88 -79 -77 0 3 5 76 77 78 79 88 99 277 288 303 =.), gen(`var'_recode)
				}
			
			egen expec_support_child=rowmean( gufinan guhelps gufinyou gucares guold guemot)
			egen expect_age_child=rowmean( expearn_recode expedu_recode expind_recode expleav_recode expmar_recode expchild_recode)	
			
			rename (expearn_recode expedu_recode expind_recode expleav_recode expmar_recode expchild_recode) ///
					(expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent)
			
	*Household risks:
		recode pedrunk  (. = 0 "no") (1 = 2 "yes") (2 = 1 "sometimes"), gen(hh_drunk)		/*Any household member gets drunk*/
		recode peaggrss (. = 0 "no") (1 = 2 "yes") (2 = 1 "sometimes"), gen(hh_agressive)	/*Any household member becomes aggressive*/
		
	destring typesite, gen(dropme)
	gen urban=cond(dropme==2,0,dropme)
	drop dropme
	
	gen exp_pc_r2_log=log(totalexp_pc)
	
	keep childid headid careid id1 hhsize situac_r2 mvdtypr2 primum careed daded dadal seedad mumal mumed peemb pemumliv pewherem seemum carelive peedback peylive pemoved peyback pemvfrom ///
		 peymove ownland bencg methods lookid decid dsaid dsacg dsachild lkacg lkachild bnaid bnacg bnachild decwagid decwagcg rspwagid rspwagcg decbusid decbuscg rspbusid rspbuscg  ///
		 trnchd01 trncn101 trncn201 trnchd02 trncn102 trncn202  trnchd05 trncn105 trncn205  trnchd06 trncn106 trncn206 trnchd09  trnchd12 ///
		 trnchd13 trncn113 trncn213 trnchd14 trncn114 trncn214 trnchd16 trncn116 trncn216 trnchd19 trncn119 trncn219 ///
		 trnchd20 trncn120 trncn220 trnchd21 trncn121 trncn221 donfood debt raise peloan pemxngo pemxbank pemxfam fdsp32 spend01 spend33 ///
		 spndyr06 st3yr04 internet numroom invest crech mightdie longterm peinsure foodtot foodshrt score_ppvt stdscre score_cog zwfa zhfa fwfa fhfa agemon ///
		 foodexp_rpc nfoodexp_rpc hq cd sv wi access12 access13 access15 event40 presch pesmoke attrition_r2 exp_clothes exp_footwear exp_private_classes exp_books ///
		 exp_med_cons exp_medicines exp_entertainment exp_presents house_moved house_months food_situation_bad food_worry food_worry_always food_not_enough food_not_enough_always ///
		 food_variety_never food_child_low_cost food_child_low_cost_always food_child_varied food_child_varied_never food_child_enough food_child_enough_never food_hh_problems ///
		 food_security_child food_security_caregiver support_material support_less support_index info_child_sought info_child_acess info_income econ_status_curr econ_status_past ///
		 econ_status_struggle econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty econ_stay_poverty crime_victim carer3y_father carer3y_grandparents carer3y_num ///
		 edu_private_expect edu_good_qual_expect edu_trav_no_adult health_vaccines area_qual hh_drunk hh_agressive finpresc bornbef bornaft  remit_transf_num remit_rest_num ///
		 remit_transf_last remit_rest_last remit_transf remit_alimony remit_alimony_num remit_alimony_amount percp_locus_child_prog_cg percp_child_edu_cg home_importance_cg ///
		 instrumental_assistance_cg psychological_appreciation_cg rewarding_interaction_cg inclusion_cg pride_cg agency_cg subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut ///
		 expec_support_child expect_age_child expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry expect_age_parent urban exp_pc_r2_log
		  
	rename 	(dadal mumal seedad seemum carelive peyback pemvfrom peymove spend01 spend33 spndyr06 access12 access13 access15 st3yr04 ///
			 event40 crech presch mightdie longterm peinsure foodtot foodshrt pesmoke peemb finpresc) ///
			(father_alive_r2 mother_alive_r2 seedad_r2 seemum_r2 carelive_r2 enter_hh want_hh_out want_hh_out_reason exp_tobacco exp_tv exp_party ///
			 access_selfhelp access_welfare access_female_gr info_income_sought hh_divorce edu_child_center edu_presch health_inj_ill health_longterm health_insurance ///
			 food_total food_shortage hh_smokers mother_pregnancies edu_presch_age_end)
save "$aux\round2.dta",replace

*Household composition database:
use "$inputs\pe_r2_subhouseholdmember5.dta" ,clear
	*HH composition:
		gen mother=cond((relate==1| relate==24) & memsex==2,1,0)
		gen bio_father=cond((relate==1| relate==24) & memsex==1,1,0)
		gen stepfather=cond(relate==2 & memsex==1,1,0)
		gen grandparent=cond(relate==5|relate==6,1,0)
		gen uncle=cond(relate==13,1,0)
		
		gen mother_hh=cond(mother==0,.,cond(livhse==1,1,0))				/*mother still in hh*/
		gen bio_father_hh=cond(bio_father==0,.,cond(livhse==1,1,0))		/*father still in hh*/
		gen step_father_hh=cond(stepfather==0,.,cond(livhse==1,1,0))	/*stepfather still in hh*/
		gen grandparents_hh=cond(grandparent==0,.,cond(livhse==1,1,0))	/*grandparent still in hh*/
		gen uncle_hh=cond(uncle==1 & livhse==1,1,0)						/*uncle/aunt still in hh*/
		
		gen blood_sibling=cond(relate==7,1,0) 
		gen half_sibling=cond(relate==8|relate==9|relate==10,1,0)
		gen all_sibling=cond(blood_sibling==1|half_sibling==1,1,0)
		
		gen dropme=cond(relate==0,age,.)
		bys childid: egen dropme2=max(dropme)
		
		gen blood_sibling_old=cond(blood_sibling==1 & age>dropme2,1,0)
		gen blood_sibling_young=cond(blood_sibling==1 & age<=dropme2,1,0)
		
		gen half_sibling_old=cond(half_sibling==1 & age>dropme2,1,0)
		gen half_sibling_young=cond(half_sibling==1 &age<=dropme2,1,0)
		
		gen all_sibling_old=cond(all_sibling==1 & age>dropme2,1,0)
		gen all_sibling_young=cond(all_sibling==1 & age<=dropme2,1,0)
	
		drop dropme* 
		
	*HH education:
		gen education=cond(grade ==.,chgrade,grade)	/*max education level*/
		gen edu_some_college=cond(education==.,.,cond(education>12 & education<17,1,0)) /*max education level: at least incomplete college*/
		
		gen mom_some_college=cond(mother==1,edu_some_college,.)
		gen father_some_college=cond(bio_father==1,edu_some_college,.)
	
	*Mom labour: 
		gen job1=cond(!inlist(act1, 7, 15, 17, 18, 19, 20) & act1!=.,1,0)
		gen job2=cond(!inlist(act2, 7, 15, 17, 18, 19, 20) & act2!=.,1,0)
		gen job3=cond(!inlist(act3, 7, 15, 17, 18, 19, 20) & act3!=.,1,0)
		
		egen mom_jobs=rowtotal(job*)
		replace mom_jobs=cond(mother==0|livhse!=1,.,mom_jobs)	/*missing obs. come predominantly from mothers not living in HH. */

		gen mom_works=cond(mother==0|mom_jobs==.,.,cond(mom_jobs!=0,1,0))
	
	*Work hours:
		gen dropme1=cond(mother==0,.,cond(mom_works==0,0,cond(job1==1,hours1*diasmes1*months1,0)))
		gen dropme2=cond(mother==0,.,cond(mom_works==0,0,cond(job2==1,hours2*diasmes2*months2,0)))
		gen dropme3=cond(mother==0,.,cond(mom_works==0,0,cond(job3==1,hours3*diasmes3*months3,0)))
		egen mom_works_total=rowtotal(dropme*)
		replace mom_works_total=cond(mother==0|mom_jobs==.,.,mom_works_total)
		drop dropme*
	
	*Income:
		gen work_wage_month=cond(inctime==.,0,cond(inctime==1,incamnt*8*22,cond(inctime==2,incamnt*22,cond(inctime==3,(incamnt/7)*22,cond(inctime==4,(incamnt/14)*22, ///
							cond(inctime==5,incamnt,cond(inctime==6,incamnt/3,cond(inctime==7,incamnt/6,incamnt/12))))))))
							
		gen work_bonus_month=cond(bontime==.|bontime==1,0,cond(bontime==2,bonamnt*22,cond(bontime==3,(bonamnt/7)*22,cond(bontime==4,(bonamnt/14)*22, ///
							 cond(bontime==5,bonamnt,cond(bontime==6,bonamnt/3,cond(bontime==7,bonamnt/6,bonamnt/12)))))))
							 
		gen work_remun_month=cond(remtime==.,0,cond(remtime==1,remamnt*8*22,cond(remtime==2,remamnt*22,cond(remtime==3,(remamnt/7)*22,cond(remtime==4,(remamnt/14)*22, ///
							 cond(remtime==5,remamnt,cond(remtime==6,remamnt/3,cond(remtime==7,remamnt/6,remamnt/12))))))))
			
		egen mom_works_inc_month=rowtotal(work_wage_month work_bonus_month work_remun_month) /*mom: last payment for most important activity converted to a monthly period*/
	
	*Child: time use
		gen child_hrs_sleep=cond(relate==0 & sleep<99,sleep,.)								/*child: hours sleeping in a typical day*/
		gen child_hrs_care=cond(relate==0 & chcare<99,chcare,.)								/*child: hours taking care of others in a typical day*/
		gen child_hrs_chores=cond(relate==0 & hhchore<99,hhchore,.)							/*child: hours performing hh chores in a typical day*/	
		gen child_hrs_work=cond(relate==0 & npaywork<99 & paywork<99,(npaywork+paywork),.)	/*child: hours working in a typical day*/
		gen child_hrs_school=cond(relate==0 & school<99,school,.)							/*child: hours at school in a typical day*/
		gen child_hrs_study=cond(relate==0 & study<99,study,.)								/*child: hours studying in a typical day*/
		gen child_hrs_play=cond(relate==0 & play<99,play,.)									/*child: hours playing in a typical day*/
	
	*Final touches:
		collapse (max) mother_hh bio_father_hh step_father_hh grandparents_hh blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young ///
						all_sibling_old all_sibling_young mom_some_college father_some_college mom_works mom_jobs mom_works_total mom_works_inc  uncle_hh ///
						child_hrs_sleep child_hrs_care child_hrs_chores child_hrs_work child_hrs_school child_hrs_study child_hrs_play, by(childid)
		
		gen mom_works_inc_month_log=log(mom_works_inc_month)
		
		replace mother_hh=cond(mother_hh==.,0,mother_hh)
		replace bio_father_hh=cond(bio_father_hh==.,0,bio_father_hh)
		replace grandparents_hh=cond(grandparents_hh==.,0,grandparents_hh)
		replace uncle_hh=cond(uncle_hh==.,0,uncle_hh)
		
		egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh)
		gen alone_hh=cond(dropme==0,1,0) /*no presence of father, step father or grandparents*/
		drop dropme
		
		egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh uncle_hh)
		gen alone_hh2=cond(dropme==0,1,0) /*no presence of father, step father, grandparents, uncles or aunts*/
		drop dropme	
		
		global r2_hh mother_hh bio_father_hh step_father_hh grandparents_hh blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young ///
					 all_sibling_old all_sibling_young alone_hh mom_some_college father_some_college mom_works mom_jobs mom_works_total mom_works_inc_month ///
					 mom_works_inc_month_log child_hrs_sleep child_hrs_care child_hrs_chores child_hrs_work child_hrs_school child_hrs_study child_hrs_play
		
		foreach var in $r2_hh{
			rename `var' `var'_r2
			}
		
*Merging both databases:
	merge 1:1 childid using "$aux\round2.dta" 
	
	replace father_some_college_r2=cond(father_some_college_r2!=.,father_some_college_r2,cond(daded>12 & daded<18,1,cond(daded==.,.,0)))
	replace mom_some_college_r2=cond(mom_some_college_r2!=.,mom_some_college_r2,cond(mumed>12 & mumed<18,1,cond(mumed==.,.,0)))
	
	drop _merge
save "$aux\round2.dta",replace


*Adding variables to measure mothers' decision power:
keep childid careid decid lookid dsaid bnaid decwagid rspwagid decbusid rspbusid trncn102 trncn202 trncn105 trncn205 trncn106 trncn206 trncn113 trncn213 trncn114 trncn214 trncn116 trncn216 ///
	 trncn119 trncn219 trncn120 trncn220 trncn121 trncn221 trncn101 trncn201 bencg dsacg lkacg bnacg decwagcg rspwagcg decbuscg rspbuscg
	
preserve
	use "$inputs\pe_r2_subhouseholdmember5.dta" ,clear
	gen mother=cond((relate==1| relate==24) & memsex==2,1,0)
	keep childid id relate mother
	global ids decid lookid dsaid bnaid decwagid rspwagid decbusid rspbusid trncn101 trncn201 trncn102 trncn202 trncn105 trncn205 trncn106 trncn206 trncn113 trncn213 trncn114 trncn214 ///
			   trncn116 trncn216 trncn119 trncn219 trncn120 trncn220 trncn121 trncn221
	foreach var in $ids{
		gen `var'=id
		}
	gen careid=id
	save "$aux\hh_id_round2.dta",replace
restore

foreach var in $ids{
	merge 1:1 childid `var' using  "$aux\hh_id_round2.dta"	
	drop if _merge==2
	gen mom_`var'=cond(`var'==.,.,cond(_merge==3 & mother==1,1,0))
	drop _merge
	}
	
	rename ( mom_decid mom_lookid mom_dsaid mom_bnaid mom_decwagid mom_rspwagid mom_decbusid mom_rspbusid)  ///
		   ( mom_decisions_land mom_look_land mom_decisions_animals mom_earnings_animals mom_decisions_wages mom_earnings_wages mom_decisions_business mom_earnings_business)
	
	gen mom_decisions_pension=cond(mom_trncn101==. & mom_trncn201==.,.,cond(mom_trncn101==1| mom_trncn201==1,1,0))
	gen mom_decisions_ss=cond(mom_trncn102==. & mom_trncn202==.,.,cond(mom_trncn102==1| mom_trncn202==1,1,0))
	gen mom_decisions_religion=cond(mom_trncn105==. & mom_trncn205==.,.,cond(mom_trncn105==1| mom_trncn205==1,1,0))
	gen mom_decisions_charity=cond(mom_trncn106==. & mom_trncn206==.,.,cond(mom_trncn106==1| mom_trncn206==1,1,0))
	gen mom_decisions_donations=cond(mom_trncn113==. & mom_trncn213==.,.,cond(mom_trncn113==1| mom_trncn213==1,1,0))
	gen mom_decisions_transfers=cond(mom_trncn114==. & mom_trncn214==.,.,cond(mom_trncn114==1| mom_trncn214==1,1,0))
	gen mom_decisions_insurance=cond(mom_trncn116==. & mom_trncn216==.,.,cond(mom_trncn116==1| mom_trncn216==1,1,0))
	gen mom_decisions_public=cond(mom_trncn119==. & mom_trncn219==.,.,cond(mom_trncn119==1| mom_trncn219==1,1,0))
	gen mom_decisions_private=cond(mom_trncn120==. & mom_trncn220==.,.,cond(mom_trncn120==1| mom_trncn220==1,1,0))
	gen mom_decisions_other=cond(mom_trncn121==. & mom_trncn221==.,.,cond(mom_trncn121==1| mom_trncn221==1,1,0))
	
	egen mom_decisions_resources_r2=rowmean( mom_decisions_pension mom_decisions_ss mom_decisions_religion mom_decisions_charity mom_decisions_donations ///
										  mom_decisions_transfers mom_decisions_insurance mom_decisions_public mom_decisions_private mom_decisions_other) /*decisions over resources*/
											
	egen dropme=rownonmiss( trncn101 trncn201 trncn102 trncn202 trncn105 trncn205 trncn106 trncn206 trncn113 trncn213 trncn114 trncn214 trncn116 trncn216 trncn119 trncn219 trncn120 ///
							trncn220 trncn121 trncn221)
	gen external_resources=cond(dropme==0,0,0)
	
	drop trncn* *_trncn* mom_decisions_pension mom_decisions_ss mom_decisions_religion mom_decisions_charity mom_decisions_donations mom_decisions_transfers mom_decisions_insurance ///
		  mom_decisions_public mom_decisions_private mom_decisions_other mother
	
	merge 1:1 childid careid using "$aux\hh_id_round2.dta"	
	drop if _merge==2
	
	rename relate caregiver
	
	gen mom_earnings_land=cond(_merge==1|bencg==.,.,cond(mother==1 & bencg==1, 1,0))
	gen mom_decisions_animals_main=mom_decisions_animals									/*mother is the main person making decisions regarding the animals */
	replace mom_decisions_animals=cond(mother==1 & dsacg==1,1,mom_decisions_animals)		/*mother can make decisions regarding the animals */
	
	gen mom_earnings_animals_main=mom_earnings_animals										/*mother is the main person that controls earnings from the animals */
	replace mom_earnings_animals=cond(mother==1 & bnacg==1,1,mom_earnings_animals)			/*mother control any earnings from animals */
	
	gen mom_decisions_wages_main=mom_decisions_wages										/*mother is the main person making decisions regarding the wages activities*/
	replace mom_decisions_wages=cond(mother==1 & decwagcg==1,1,mom_decisions_wages)			/*mother can make decisions regarding the wages activities*/
	
	gen mom_earnings_wages_main=mom_earnings_wages											/*mother is the main person controlling the earnings from work for wages activities*/
	replace mom_earnings_wages=cond(mother==1 & rspwagcg==1,1,mom_earnings_wages)			/*mother can control the earnings from work for wages activities*/

	gen mom_decisions_business_main=mom_decisions_business									/*mother is the main person controlling the earnings from work for wages activities*/
	replace mom_decisions_business=cond(mother==1 & decbuscg==1,1,mom_decisions_business)	/*mother can control the earnings from work for wages activities*/
	
	gen mom_earnings_business_main=mom_earnings_business									/*mother is the main person controlling the earnings from work for wages activities*/
	replace mom_earnings_business=cond(mother==1 & rspbuscg==1,1,mom_earnings_business)		/*mother can control the earnings from work for wages activities*/
	
	egen mom_decisions_main_prod_r2=rowmean(mom_decisions_animals_main mom_decisions_wages_main mom_decisions_business_main) /*index: mother main decision-maker about productive activities*/
	egen mom_earnings_main_prod_r2=rowmean(mom_earnings_animals_main mom_earnings_wages_main mom_earnings_business_main) /*index: mother main controller of earnings from productive activities*/
	
	egen mom_decisions_prod_r2=rowmean(mom_decisions_animals mom_decisions_wages mom_decisions_business)					/*index: mother can make decisions about productive activities*/
	egen mom_earnings_prod_r2=rowmean(mom_earnings_land mom_earnings_animals mom_earnings_wages mom_earnings_business)		/*index: mother can control some earnings from productive activities*/
	
	keep childid mom_decisions_resources_r2 mom_decisions_main_prod_r2 mom_decisions_prod_r2 mom_earnings_prod_r2  mom_earnings_main_prod_r2 external_resources	
	
	merge 1:1 childid using "$aux\round2.dta"
	drop _merge decid lookid dsaid bnaid decwagid rspwagid decbusid rspbusid trncn* bencg dsacg lkacg bnacg decwagcg rspwagcg decbuscg rspbuscg trnchd*
save "$aux\round2.dta", replace	

*Preeschol database:
use "$inputs\pe_r2_subpreschool5.dta" ,clear
	keep childid agepresc presctyp
	rename agepresc edu_presch_age_begin
	gen edu_presch_private=cond(presctyp==.,.,cond(presctyp==1,1,0))
	collapse (max) edu_presch_age_begin edu_presch_private, by(childid)
	
	merge 1:1 childid using "$aux\round2.dta"
	
	gen edu_presch_duration=cond(_merge==2,.,cond(edu_presch_age_end>0 & edu_presch_age_end!=.,edu_presch_age_end-edu_presch_age_begin, agemon-edu_presch_age_begin)) /*months in preschool*/
	replace edu_presch_duration=cond(edu_presch_duration<0,.,edu_presch_duration) /*change strange cases to missing*/	
	drop _merge
save "$aux\round2.dta", replace

*Mother's migration background databases:
use "$aux\round1.dta", clear
	keep childid age_pregnancy
	gen age_pregnancy_round=round(age_pregnancy)
	merge 1:m child using "$inputs\pe_r1_subsec2householdroster1.dta"
	drop _merge
	keep if sex==2 & rel==1
	keep childid age age_pregnancy age_pregnancy_round
	merge 1:m childid using "$inputs\pe_r2_subresidencychanges15.dta" /*Residency changes if the mother (or caregiver) was born in a different community to the one in which she is now living*/
	drop if _merge==2
	gen moves=cond(_merge!=1,1,0)
	drop _merge
	rename (rcfid whymove agemove) (rcbid whyborn ageborn)
	merge 1:1 childid rcbid using "$inputs\pe_r2_subresidencychanges25.dta" /*Residency changes if the mother (or caregiver) was born in a the same community to the one in which she is now 	
																				living, moved at some point and returned.*/
	replace move=cond(_merge==2,1,moves)
	replace ageborn=cond(ageborn<0,.,ageborn)
	rename (rcbid whyborn ageborn) (rcfid whymove agemove)
	sort childid rcfid
	by childid: egen dropme =max(age)
	replace age=cond(age==.,dropme,age)
	drop dropme _merge
	
	gen dropme=agemove- age_pregnancy
	gen moves_pregnancy=cond(age==.,.,cond(agemove== age_pregnancy_round| agemove< age_pregnancy_round & (-1<dropme<0),1,0)) /*moves during pregnancy*/
	
	gen moves_before=cond(agemove==.|age==.,.,cond(agemove< age_pregnancy_round & dropme<-1,1,0))							/*moves before pregnancy*/
	gen moves_before_minor=cond(agemove==.|age==.,.,cond((moves_before==1|moves_pregnancy==1) & agemove<18,1,0))			/*moves before being an adult*/
	
	gen moves_cohab_preg=cond((whymove==13|whymove==18) & moves_pregnancy==1,1,0)
	gen moves_cohab_bef=cond((whymove==13|whymove==18) & moves_before==1,1,0)
	gen moves_cohab_minor=cond((whymove==13|whymove==18) & moves_before_minor==1,1,0)
	
	gen moves_separ_preg=cond(whymove==7 & moves_pregnancy==1,1,0)
	gen moves_separ_bef=cond(whymove==7 & moves_before==1,1,0)
	gen moves_separ_minor=cond(whymove==7 & moves_before_minor==1,1,0)
	
	gen moves_viole_preg=cond(whymove==9 & moves_pregnancy==1,1,0)
	gen moves_viole_bef=cond(whymove==9 & moves_before==1,1,0)
	gen moves_viole_minor=cond(whymove==9 & moves_before_minor==1,1,0)

	gen moves_disp_preg=cond(whymove==4 & moves_pregnancy==1,1,0)
	gen moves_disp_bef=cond(whymove==4 & moves_before==1,1,0)
	gen moves_disp_minor=cond(whymove==4 & moves_before_minor==1,1,0)
	
	egen moves_pregnancy2=rowtotal(moves_cohab_preg moves_separ_preg moves_viole_preg moves_disp_preg)
	egen moves_before2=rowtotal(moves_cohab_bef moves_separ_bef moves_viole_bef moves_disp_bef)
	egen moves_minor2=rowtotal(moves_cohab_minor moves_separ_minor moves_viole_minor moves_disp_minor)
	
	gen moves_missing=cond(rcfid==.,.,cond(whymove==.,1,0))
	
	gen moves_after=cond(agemove==.|age==.,.,cond(agemove> age_pregnancy_round,1,0))
	
	collapse (sum)  moves_pregnancy moves_before moves_before_minor moves_cohab_preg moves_cohab_bef moves_cohab_minor moves_separ_preg moves_separ_bef moves_separ_minor moves_viole_preg ///
					moves_viole_bef moves_viole_minor moves_disp_preg moves_disp_bef moves_disp_minor moves_pregnancy2 moves_before2 moves_minor2 moves_missing moves_after, by(childid)
	
	*egen dropme=rowtotal(moves_pregnancy moves_before moves_before_minor moves_cohab_preg moves_cohab_bef moves_cohab_minor moves_separ_preg moves_separ_bef moves_missing ///
	*						moves_separ_minor moves_viole_preg moves_viole_bef moves_viole_minor moves_disp_preg moves_disp_bef moves_disp_minor moves_pregnancy2 moves_before2 moves_minor2)
	
	egen dropme=rowtotal(moves_before moves_pregnancy moves_missing)
	gen no_moves_before=cond(dropme==0,1,0)
	drop dropme
	
	global moves moves_pregnancy moves_before moves_before_minor moves_cohab_preg moves_cohab_bef moves_cohab_minor moves_separ_preg moves_separ_bef moves_separ_minor moves_viole_preg ///
				 moves_viole_bef moves_viole_minor moves_disp_preg moves_disp_bef moves_disp_minor moves_pregnancy2 moves_before2 moves_minor2 moves_missing no_moves_before
	
	foreach var in $moves{
		gen `var'_dum=cond(`var'>0,1,0)
		}
merge 1:1 childid using "$aux\round2.dta"
drop _merge

*Final touches, ordering outcomes and relevant covariates:
	replace mom_works_r2=cond(mother_hh==0,.,mom_works_r2)
	order childid situac_r2 attrition_r2 agemon house_moved house_months urban, first
	
	rename (stdscre score_ppvt score_cog) (ppvt_stdscre_r2 ppvt_r2 cda_r2 )
	gen outcomes=. 
	order outcomes ppvt_stdscre_r2 ppvt_r2 cda_r2 , after(house_months)
	
	gen cov_hh_charac=.
	order cov_hh_charac father_alive_r2 mother_alive_r2 seedad_r2 seemum_r2 carelive_r2  moves_pregnancy moves_before moves_before_minor moves_cohab_preg moves_cohab_bef moves_cohab_minor ///
		  moves_separ_preg moves_separ_bef moves_separ_minor moves_viole_preg moves_viole_bef moves_viole_minor moves_disp_preg moves_disp_bef moves_disp_minor moves_pregnancy2 moves_before2 ///
		  moves_minor2 moves_missing no_moves_before moves_pregnancy_dum moves_before_dum moves_before_minor_dum moves_cohab_preg_dum moves_cohab_bef_dum moves_cohab_minor_dum ///
		  moves_separ_preg_dum moves_separ_bef_dum moves_separ_minor_dum moves_viole_preg_dum moves_viole_bef_dum moves_viole_minor_dum moves_disp_preg_dum moves_disp_bef_dum ///
		  moves_disp_minor_dum moves_pregnancy2_dum moves_before2_dum moves_minor2_dum moves_missing_dum no_moves_before_dum bornbef bornaft enter_hh want_hh_out want_hh_out_reason ///
		  want_hh_out mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh_r2 alone_hh2 blood_sibling_r2 half_sibling_r2 all_sibling_r2 blood_sibling_old_r2 ///
		  blood_sibling_young_r2 half_sibling_old_r2 half_sibling_young_r2 all_sibling_old_r2 all_sibling_young_r2 mom_some_college father_some_college hhsize numroom mom_works_r2 /// 
		  mom_jobs_r2 mom_works_total_r2 mom_works_inc_month_r2 mom_works_inc_month_log_r2 mom_decisions_resources_r2 mom_decisions_main_prod_r2 mom_decisions_prod_r2 mom_earnings_prod_r2 ///
		  mom_earnings_main_prod_r2 carer3y_father carer3y_grandparents carer3y_num , after(cda_r2)
	
	gen cov_hh_econ=.
	order cov_hh_econ external_resources remit_transf_num remit_rest_num remit_transf_last  remit_rest_last remit_transf remit_alimony remit_alimony_num remit_alimony_amount econ_status_curr ///
		  econ_status_past econ_status_struggle econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty econ_stay_poverty food_hh_problems food_security_child ///
		  food_security_caregiver donfood food_shortage foodexp_rpc nfoodexp_rpc exp_pc_r2_log hq cd sv wi debt raise pemxbank pemxngo pemxfam peloan invest ownland , after(carer3y_num)
	
	gen cov_education=.
	order cov_education exp_private_classes exp_books edu_child_center edu_presch edu_presch_private edu_presch_duration edu_private_expect edu_good_qual_expect edu_trav_no_adult ///
		  percp_child_edu, after(ownland)
	
	gen cov_health=.
	order cov_health exp_med_cons exp_medicines health_inj_ill health_longterm health_insurance health_vaccines zwfa zhfa fwfa fhfa, after(percp_child_edu)
	
	gen cov_time_use=.
	order cov_time_use child_hrs_sleep child_hrs_care child_hrs_chores child_hrs_work child_hrs_school child_hrs_study child_hrs_play, after(fhfa)
	
	gen cov_hh_risk=.
	order cov_hh_risk exp_tobacco exp_tv exp_party  hh_divorce hh_drunk hh_agressive hh_smokers mother_pregnancies, after(child_hrs_play)
	
	gen cov_caregiver=.
	order cov_caregiver  percp_child_edu_cg subjective_wellbeing_cg_curr subjective_wellbeing_cg_fut agency_cg pride_cg inclusion_cg percp_locus_child_prog_cg home_importance_cg ///
		  instrumental_assistance_cg psychological_appreciation_cg rewarding_interaction_cg expec_support_child expect_age_child expect_age_earn expect_age_education expect_age_indp ///
		  expect_age_leave expect_age_marry expect_age_parent support_material support_less, after(mother_pregnancies)
		  
	gen cov_area=.
	order cov_area crime_victim area_qual access_selfhelp access_welfare access_female_gr, after(expect_age_child)
	
	gen cov_others=.
	order cov_others support_index info_child_sought info_child_acess info_income info_income_sought, after(access_female_gr)
	
	drop  exp_clothes exp_footwear exp_entertainment exp_presents food_situation_bad food_worry food_worry_always food_not_enough food_not_enough_always food_variety_never ///
	      food_child_low_cost food_child_low_cost_always food_child_varied food_child_varied_never food_child_enough food_child_enough_never food_total edu_presch_age_begin headid ///
		  mvdtypr2 id1 primum careid daded mumed careed pemumliv pewherem peylive pemoved peedback methods dsachild lkachild bnachild fdsp32 internet edu_presch_age_end
	
	global variables house_moved house_months enter_hh want_hh_out want_hh_out_reason numroom hhsize carer3y_father carer3y_grandparents carer3y_num econ_status_struggle ///
					 econ_status_poor econ_status_worse econ_enter_poverty econ_exit_poverty econ_stay_poverty food_hh_problems food_security_child food_security_caregiver donfood ///
					 food_shortage foodexp_rpc nfoodexp_rpc hq cd sv wi debt raise pemxbank pemxngo pemxfam exp_private_classes exp_books edu_child_center edu_presch ///
					 edu_private_expect edu_good_qual_expect edu_trav_no_adult  exp_med_cons exp_medicines percp_child_edu_cg subjective_wellbeing_cg_curr ///
					 subjective_wellbeing_cg_fut agency_cg pride_cg inclusion_cg percp_locus_child_prog_cg home_importance_cg instrumental_assistance_cg psychological_appreciation_cg ///
					 rewarding_interaction_cg expec_support_child expect_age_child support_material support_less health_inj_ill health_longterm health_insurance health_vaccines exp_tobacco ///
					 exp_tv exp_party hh_divorce hh_drunk hh_agressive hh_smokers mother_pregnancies crime_victim area_qual access_selfhelp ///
					 access_welfare access_female_gr  support_index info_child_sought info_child_acess info_income info_income_sought zwfa zhfa fwfa fhfa peloan invest ///
					 ownland edu_presch_private edu_presch_duration  cov_hh_charac cov_hh_econ cov_education cov_health cov_time_use cov_hh_risk cov_caregiver cov_area cov_others ///
					 external_resources remit_rest_last remit_transf remit_alimony remit_alimony_num remit_alimony_amount outcomes  moves_pregnancy moves_before moves_before_minor ///
					 moves_cohab_preg moves_cohab_bef moves_cohab_minor moves_separ_preg moves_separ_bef moves_separ_minor moves_viole_preg moves_viole_bef moves_viole_minor moves_disp_preg ///
					 moves_disp_bef moves_disp_minor moves_pregnancy2 moves_before2 moves_minor2 moves_missing no_moves_before moves_pregnancy_dum moves_before_dum moves_before_minor_dum ///
					 moves_cohab_preg_dum moves_cohab_bef_dum moves_cohab_minor_dum moves_separ_preg_dum moves_separ_bef_dum moves_separ_minor_dum moves_viole_preg_dum moves_viole_bef_dum ///
					 moves_viole_minor_dum moves_disp_preg_dum moves_disp_bef_dum moves_disp_minor_dum moves_pregnancy2_dum moves_before2_dum moves_minor2_dum moves_missing_dum ///
					 no_moves_before_dum bornbef bornaft  econ_status_curr econ_status_past expect_age_earn expect_age_education expect_age_indp expect_age_leave expect_age_marry ///
					 expect_age_parent urban alone_hh2
	
	foreach var in $variables{
		rename `var' `var'_r2
		}
save "$outputs\round2.dta",replace	
	
	
	
	