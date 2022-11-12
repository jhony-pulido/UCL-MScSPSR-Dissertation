
********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\1.Build\1.Inputs"
global aux		"$root\1.Build\2.Aux"
global outputs	"$root\1.Build\3.Outputs"
global code		"$root\1.Build\0.Code"
********************************************************************************************************************

*Child database:
use "$inputs\pe_r1_childlevel1yrold.dta" , clear
	*Treatment: father not present everyday
	gen treatment=cond(seedad==. & daddead!=3,.,cond(seedad!=1|daddead==3,1,0))
	
	*Covariates: 
	/* Caliendo & Kopeinig (2008): "The matching strategy builds on the CIA, requiring that the outcome variable(s) must be independent of treatment conditional on the propensity 	
	score. Hence, implementing matching requires choosing a set of variables X that credibly satisfy this condition... Only variables that influence simultaneously the participation decision
	and the outcome variable should be included. It should also be clear that only variables that are unaffected by participation (or the anticipation of it) should be included in the model. To
	ensure this, variables should either be fixed over time or measured before participation. 
	Not knowing the exact data of separation stops being able to include some variables in the PS construction because they could have been affected by this event (for example, child health 
	and information about pregnancy).
	*/
	gen female=cond(sex==2,1,0) 								/*sex of child*/
	gen urban=cond(typesite==1,1,0)								/*household location*/
	gen child_not_first=cond(chdborn!=1,1,0) 					/*child is not the first*/
	gen no_boy=cond(boyborn==0,1,0) 							/*parents have not a boy*/
	gen cg_mother_not_spanish=cond(mothidio!=1,1,0)
	gen not_spanish=cond( idio1!=1,1,0)  						/*caregiver main language is not spanish*/
	gen read_spanish_dif=cond(literspc!=1,1,0)
	gen dead_children=cond(brchk!=0,1,0)						/*caregiver had experienced death of a child*/
	gen abortion=cond(aborto>0,1,cond(aborto==.,.,0))			/*caregiver had experienced an abortion or miscarriage*/
	gen catholic=cond(mothrel==5,1,0)							/*caregiver is catholic*/
	egen bad_event=rowmean( hhlstck hhcrps hhlstl hhcstl )		/*household faced an exogenous bad event*/
		replace bad_event=bad_event-1
		replace bad_event=cond(badevent==2,0,bad_event)
	gen lima=cond(pecodpt1==15,1,0)
	gen violence_gp_mom=cond(violenam==1|violenam==3,1,0)
	gen violence_gp_dad=cond(violenap==1|violenap==3,1,0)
	gen beaten_mom=cond(lpegma==1|lpegma==3,1,0)
	gen beaten_dad=cond(lpegpa==1|lpegpa==3,1,0)
	gen birth_home=cond(bplace==1,1,0)
	gen birth_premature=cond(prematur==1,1,0)
	egen dropme=rowmean( docbrth nurbrth midbrth)
	gen birth_no_prof=cond(dropme==.,.,cond(dropme==2,1,0))
	gen birth_no_rel=cond(relbrth==2,1,0)
	gen birth_alone=cond(relbrth==.,.,cond(relbrth==2 & othbrth==2,1,0))
	
	global recode ownhouse antnata wantcld
	foreach var in $recode{
		recode `var' (2=0), gen(`var'_recode)
		}
	
	rename (ownhouse_recode antnata_recode wantcld_recode) (own_house antenatal_visit child_wanted)
	gen sex_hoped=cond(female==1 & whichsex==1,0,1)
	gen poor_pregnancy=cond(diffpreg==.,.,cond(diffpreg==2,1,0))
	
	*Analysing the conviniency of adding community-related covariates:
	tab timelive treat, col
	reg timelive treatment 
	gen new_home=cond(timelive<=6,1,0)
	logit new_home treatment, or  
	margins, dydx(treatment) 	/*children who experienced father absence had a very higher probability of having moved. */
	drop new_home
	
	*Remittances:
		egen remit_rest_num=rowtotal(  veces1 veces3 veces4 veces6)
		gen remit_rest=cond(remit_rest_num>0,1,0)
		rename veces5 remit_transf_num
		gen remit_transf=cond(remit_transf_num>0,1,0)
	
	keep childid placeid clustid pecodpt1 momlive seedad daddead treatment female lima urban child_not_first no_boy not_spanish dead_children abortion catholic bad_event agemon	///
		  antenatal_visit child_wanted poor_pregnancy sex_hoped own_house hq sv cd wi remit_transf remit_transf_num  remit_rest remit_rest_num  violence_gp_mom violence_gp_dad beaten_mom ///
		  beaten_dad birth_home birth_premature birth_no_prof birth_no_rel birth_alone cg_mother_not_spanish read_spanish_dif timelive
save "$aux\round1.dta",replace

*Household composition database:
use "$inputs\pe_r1_subsec2householdroster1.dta" ,clear
	gen mother=cond(relate==1 & sex==2,1,0)
	
	keep if mother==1
	
	*Saving data base with mothers' id:
	preserve
		tostring id, gen(parent_id)
		gen child_parent_id=childid+parent_id
		keep child_parent_id childid
		save "$aux\r1_mother_id.dta",replace
	restore
	
	rename age mother_age
	gen mother_edu=cond(yrschool<34,1,cond(yrschool==34,2,3))

	keep childid mother_age mother_edu

	merge 1:1 childid using "$aux/round1.dta"
	
	gen age_pregnancy=((mother_age*12)-agemon-9)/12  /*Age of pregnancy: current mother age - child age - 9 months of pregnancy*/
	gen teen_pregnancy=cond(age_pregnancy<18 & mother_age!=.,1,0)
	
	drop _merge
	
	save "$aux/round1.dta", replace
	
	keep childid agemon
	merge 1:m childid using "$inputs\pe_r1_subsec2householdroster1.dta" 
	drop _merge
	
	gen mother_hh=cond(relate==1 & sex==2,2,0)
	gen bio_father_hh=cond(relate==1 & sex==1,1,0)
	gen step_father_hh=cond(relate==2 & sex==1,1,0)
	gen grandparents_hh=cond(relate==3,1,0)
	gen uncle_hh=cond(relate==4,1,0)
	
	gen blood_sibling=cond(relate==5,1,0)
	gen half_sibling=cond(relate==10,1,0)
	gen all_sibling=cond(relate==5|relate==10,1,0)
	
	gen dropme=round(agemon/12)
	
	gen blood_sibling_old=cond(blood_sibling==1 & age>dropme,1,0)
	gen blood_sibling_young=cond(blood_sibling==1 & age<=dropme,1,0)
	
	gen half_sibling_old=cond(half_sibling==1 & age>dropme,1,0)
	gen half_sibling_young=cond(half_sibling==1 &age<=dropme,1,0)
	
	gen all_sibling_old=cond(all_sibling==1 & age>dropme,1,0)
	gen all_sibling_young=cond(all_sibling==1 & age<=dropme,1,0)
	
	drop dropme
	
	collapse (max)  blood_sibling half_sibling all_sibling blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young mother_hh bio_father_hh ///
					step_father_hh grandparents_hh uncle_hh, by(childid)
	
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh)
	gen alone_hh=cond(dropme==0,1,0) /*no presence of father, step father or grandparents*/
	drop dropme
	
	egen dropme=rowtotal(bio_father_hh step_father_hh grandparents_hh uncle_hh)
	gen alone_hh2=cond(dropme==0,1,0) /*no presence of father, step father, grandparents, uncle or aunts*/
	drop dropme
	
	merge 1:1 childid using "$aux\round1.dta"
	drop _merge
save "$aux\round1.dta",replace
	
*Livelihoods database:
use "$inputs\pe_r1_subsec7hhlivelihoods1.dta",clear
	tostring id, gen(parent_id)
	gen child_parent_id=childid+parent_id
	recode paymt (2 = 0 "No"), gen(payment)
	gen mom_jobs=1
	tab day, gen(mom_work_days)
	collapse (sum) mom_jobs payment (max)  mom_work_days1 mom_work_days2 mom_work_days3 mom_work_days4  months, by(child_parent_id)
	
	merge 1:1 child_parent_id using "$aux/r1_mother_id.dta"
	drop if _merge==1
	
	global work_var mom_jobs payment mom_work_days1 mom_work_days2 mom_work_days3 mom_work_days4 months
	foreach var in $work_var{
		replace `var'=cond(_merge==2,0,`var')
		}
	drop _merge child_parent_id
	gen mom_works=cond(mom_jobs==0,0,1)
	merge 1:1 childid using "$aux/round1.dta"
	drop _merge
save "$aux\round1.dta",replace

*Community databases:
use "$inputs\pe_r1_comm_level.dta",clear
	egen env_problems=rowmean( waste airpol watpol contsoil specpol shanty)
	
	global crime thfcrm violcrm yuthcrm proscrm comcrm speccrm
	foreach var in $crime{
		replace `var'=cond(`var'==1,1,cond(`var'==.,.,0))
		label define `var' 0 "No" 1 "Yes",replace
		label values `var' `var'
		}
		
	egen community_crime=rowtotal($crime)
	
	recode shanty   (2 = 0), gen(barrio)
	recode pland2   (8 =0), gen(land_invasion)
	recode plang2   (1 2 3 = 1) (8 = 0), gen(quechua)
	recode ptoil1   (1 = 0) (2 = 1), gen(toilets)
	recode pwater1  (1 = 0) (2 = 1), gen(water)
	recode pgarb1   (1 = 0) (2 = 1), gen(rubbish)
	recode proad1   (1 = 0) (2 = 1), gen(roads_paved)
	recode ppubtran1 (2 8 9 = 0), gen(pub_transp)
	
	global com_org  pcomgrp1 pcomgrp2 pcomgrp3 pcomgrp4 pcomgrp5 pcomgrp6 pcomgrp7 pcomgrp8 pcomgrp9 pcomgrp10 pcomgrp11 
	foreach var in $com_org{
	    recode `var' (2 8 9 = 0) ,gen(`var'_rec)
		}
	
	egen social_capital=rowmean( pcomgrp1_rec pcomgrp2_rec pcomgrp3_rec pcomgrp4_rec pcomgrp5_rec pcomgrp6_rec pcomgrp7_rec pcomgrp8_rec pcomgrp10_rec pcomgrp11_rec pcomgrp9_rec)
	
	keep commid comnro pop env_problems community_crime  barrio land_invasion quechua toilets water rubbish roads_paved pub_transp social_capital
	rename commid placeid 
	
	*Create additional databases:
	preserve
		*Monthly food accesability and work opportunities:
		use "$inputs\pe_r1_community_stblsec1diary.dta",clear
			collapse (mean) movwork sufood litwork nofood , by(comnro) /*collapse to have an idea of the prevalance*/
		save "$aux\r1_community1.dta",replace
		
		*Existence of government programmes:
		use "$inputs\pe_r1_community_stblsec3programmes.dta",clear
			keep if exists<3
			replace exists=cond(exists==2,0,exists)
			
			gen prog_food=cond(prid>1 & prid<7 & exists==1,1,0)
			gen prog_health=cond(prid>14 & prid<26 & exists==1,1,0)
			gen prog_infrast=cond(prid>25 & prid<36 & exists==1,1,0)
			gen prog_produc=cond(prid>35 & prid<39 & exists==1,1,0)
			gen prog_proper=cond(prid==39 & exists==1,1,0)
			gen prog_microcred=cond(prid==41 & exists==1,1,0)
			gen prog_work=cond(prid==42 & exists==1,1,0)
			
			collapse (sum) prog_*, by(comnro)
		save "$aux\r1_community2.dta",replace
		
		*Services:
		use "$inputs\pe_r1_community_stblsec3services.dta",clear
			drop if seraval==8
			keep if services<15
			gen police=cond(services==1 & seraval==1,1,cond(services==1 & seraval==2,0,.))
			gen electricity=cond(services==9 & seraval==1,1,cond(services==9 & seraval==2,0,.))
			gen drink_water=cond(services==11 & seraval==1,1,cond(services==11 & seraval==2,0,.))
			gen park=cond(services==2 & seraval==1,1,cond(services==2 & seraval==2,0,.))
			gen cinema=cond(services==3 & seraval==1,1,cond(services==3 & seraval==2,0,.))
			gen bank=cond((services==12|services==13) & seraval==1,1,cond((services==12|services==13) & seraval==2,0,.))
			gen internet_cab=cond(services==14 & seraval==1,1,cond(services==14 & seraval==2,0,.))
			
			gen police_dist=cond(services==1,sertme,.)
			gen electricity_dist=cond(services==9,sertme,.)
			gen drink_water_dist=cond(services==11,sertme,.)
			gen park_dist=cond(services==2,sertme,.)
			gen cinema_dist=cond(services==3,sertme,.)
			gen bank_dist=cond((services==12|services==13),sertme,.)
			gen internet_cab_dist=cond(services==14,sertme,.)
			
			collapse (mean) police electricity drink_water park cinema bank internet_cab police_dist electricity_dist drink_water_dist park_dist cinema_dist internet_cab_dist ///
					 (min) bank_dist, by(comnro)
		save "$aux\r1_community3.dta",replace
		
		*Health facilities
		use "$inputs\pe_r1_community_stblsec5healthfacility.dta",clear
			gen pharmacy=cond(helid==8 & facility==1,1,cond(helid!=8,.,0))
			gen pub_health=cond(facility==1 & (helid==1|helid==3|helid==5),1,cond(helid!=1 & helid!=3 & helid!=5,.,0))
			
			gen pharmacy_dist=cond(helid==8,heltme,.)
			gen pub_health_dist=cond((helid==1|helid==3|helid==5),heltme,.)
			
			collapse (mean)  pharmacy pub_health pharmacy_dist (min) pub_health_dist, by(comnro)
		save "$aux\r1_community4.dta",replace
	restore
	
	*Merging all community databases:
	merge 1:1 comnro using "$aux\r1_community1.dta"
	drop _merge
	merge 1:1 comnro using "$aux\r1_community2.dta"
	drop _merge
	merge 1:1 comnro using "$aux\r1_community3.dta"
	drop _merge
	merge 1:1 comnro using "$aux\r1_community4.dta"
	drop _merge
	
	save "$aux/r1_community_total.dta",replace
	
	merge 1:m placeid using "$aux/round1.dta" /*1 observation did not match*/
	
	tab placeid if _merge==2 	
	tab clustid if placeid=="PE20C02"
	tab placeid if clustid==20 /*it seems that there was a confusion when assigning this place id. We will change it to PE20C01*/
	
	preserve
		use "$aux/round1.dta", clear
		replace placeid=cond(placeid=="PE20C02","PE20C01",placeid)
		save "$aux/round1.dta", replace
	restore
	
	use "$aux/r1_community_total.dta",clear
	merge 1:m placeid using "$aux/round1.dta"
	drop _merge
	
	*Ordering the database:
	order (childid placeid comnro clustid treatment female agemon ), first
	
	gen cov_hh_charac=.
	order cov_hh_charac mother_age mother_edu age_pregnancy teen_pregnancy not_spanish dead_children abortion catholic no_boy child_not_first own_house antenatal_visit child_wanted /// 
		  birth_home birth_premature birth_no_prof birth_no_rel birth_alone cg_mother_not_spanish read_spanish_dif ////
		  poor_pregnancy sex_hoped urban lima mother_hh  bio_father_hh step_father_hh grandparents_hh alone_hh alone_hh2 blood_sibling half_sibling all_sibling blood_sibling_old ///
		  blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young violence_gp_mom violence_gp_dad beaten_mom beaten_dad , after(agemon)
	
	gen cov_hh_econ=.
	order cov_hh_econ bad_event hq sv cd wi mom_jobs payment mom_work_days1 mom_work_days2 mom_work_days3 mom_work_days4 months mom_works remit_transf remit_transf_num remit_rest ///
		  remit_rest_num, after(alone_hh) 

	gen covariates_com=.
	order pop env_problems community_crime barrio land_invasion quechua toilets water rubbish roads_paved pub_transp social_capital movwork sufood litwork nofood prog_food prog_health ///
			prog_infrast prog_produc prog_proper prog_microcred prog_work police electricity drink_water park cinema bank internet_cab police_dist electricity_dist drink_water_dist ///
			park_dist cinema_dist internet_cab_dist bank_dist pharmacy pub_health pharmacy_dist pub_health_dist, after(remit_rest_num)
	
	global variables momlive daddead seedad pecodpt1 own_house placeid comnro clustid female agemon cov_hh_charac urban lima bad_event mother_age mother_edu age_pregnancy teen_pregnancy ///
					 not_spanish dead_children abortion catholic no_boy child_not_first mother_hh bio_father_hh step_father_hh grandparents_hh alone_hh blood_sibling half_sibling all_sibling ///
					 blood_sibling_old blood_sibling_young half_sibling_old half_sibling_young all_sibling_old all_sibling_young covariates_com pop env_problems community_crime barrio ///
					 land_invasion quechua toilets water rubbish roads_paved pub_transp social_capital movwork sufood litwork nofood prog_food prog_health prog_infrast prog_produc ///
					 prog_proper prog_microcred prog_work police electricity drink_water park cinema bank internet_cab police_dist electricity_dist drink_water_dist park_dist cinema_dist ///
					 internet_cab_dist bank_dist pharmacy pub_health pharmacy_dist pub_health_dist mom_jobs payment mom_work_days1 mom_work_days2 mom_work_days3 mom_work_days4 months ///
					 mom_works cd hq sv wi remit_transf remit_transf_num  remit_rest remit_rest_num cov_hh_econ antenatal_visit child_wanted poor_pregnancy sex_hoped violence_gp_mom ///
					 violence_gp_dad beaten_mom beaten_dad birth_home birth_premature birth_no_prof birth_no_rel birth_alone cg_mother_not_spanish read_spanish_dif timelive alone_hh2
	foreach var in $variables{
	    rename `var' `var'_r1
		}
save "$outputs\round1.dta",replace	
	
	
	
	