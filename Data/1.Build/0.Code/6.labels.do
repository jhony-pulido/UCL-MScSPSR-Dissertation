

forvalues i=1(1)1{
		*Variable labels:

*Child, household, and caregiver:
	label var cov_hh_charac_r`i'   "*************** HOUSEHOLD CHARACTERISTICS- R`i'********************"
	label var mother_age_r`i' 	   "Age of mother (years)'"
	label var mother_edu_r`i' 	   "Mother's education level'"
	label var female_r`i' 		   "Child is female'"
	label var urban_r`i' 		   "HH in urban location'"
	label var child_not_first_r`i' "Not first child'"
	label var no_boy_r`i'  		   "Parents have not a boy'"
	label var not_spanish_r`i' 	   "CG's first language not spanish'"
	label var dead_children_r`i'   "CGs experienced a child death'"
	label var abortion_r`i'	       "CGs experienced an abortion'"
	label var catholic_r`i'		   "CG is Catholic'"
	label var bad_event_r`i'   	   "External bad event index'"
	label var lima_r`i'   		   "HH located in Lima'"
	label var own_house_r`i'   	   "HH owns the house'"
	label var age_pregnancy_r`i'   "Mother's age pregnancy'"
	label var teen_pregnancy_r`i'  "Mother teen pregnancy'"

*Community:
	label var covariates_com_r`i'  "*************** COMMUNITY - R`i' ********************"
	label var pop_r`i' 			   "Population'"
	label var env_problems_r`i'    "Environmental problems index'"
	label var community_crime_r`i' "Crime index'"
	label var barrio_r`i'		   "Mostly a slum'"
	label var land_invasion_r`i'   "Invasion for housing'"
	label var quechua_r`i'		   "Speaks quechua'"
	label var toilets_r`i'		   "Toilets mostly w/out sewage/septic tank'"
	label var water_r`i'		   "Mostly w/out piped water'"
	label var rubbish_r`i'		   "Rubbish mostly not collected'"
	label var roads_paved_r`i'	   "Ppal access: paved road'"
	label var pub_transp_r`i'	   "Public transport'"
	label var social_capital_r`i'  "Social capital index'"

	label var sufood_r`i'		  "% months with food shortage'"
	label var litwork_r`i'		  "% months with more work'"
	label var movwork_r`i'		  "% months workers emigrate'"
	label var nofood_r`i'		  "% months high food prices'"

	label var prog_food_r`i'	  "Food programmes'"
	label var prog_health_r`i'	  "Health programmes'"
	label var prog_infrast_r`i'	  "Infrastructure programmes'"
	label var prog_produc_r`i'	  "Productivity programmes'"
	label var prog_proper_r`i'	  "House property/title programmes'"
	label var prog_microcred_r`i' "Microcredit programmes'"
	label var prog_work_r`i'	  "Work/labour programmes'"

	label var police_r`i'		  "Police service'"
	label var electricity_r`i'	  "Electricity'"
	label var drink_water_r`i'	  "Drinking water'"
	label var park_r`i'			  "Parks'"
	label var cinema_r`i'		  "Cinema'"
	label var bank_r`i'			  "Banks' "
	label var internet_cab_r`i'	  "Internet cab'"

	label var police_dist_r`i'	     "Mins. to nearest police'"
	label var electricity_dist_r`i'  "Mins. to nearest electricity'"
	label var drink_water_dist_r`i'  "Mins. to nearest drink water'"
	label var park_dist_r`i'		 "Mins. to nearest park'"
	label var cinema_dist_r`i'	     "Mins. to nearest cinema'"
	label var bank_dist_r`i'		 "Mins. to nearest bank'"
	label var internet_cab_dist_r`i' "Mins. to nearest internet cab'"
		 
	label var pharmacy_r`i'		  	"Pharmacy'"
	label var pub_health_r`i'	  	"Pub. health facility'"

	label var pharmacy_dist_r`i'	 "Mins. to nearest pharmacy'"
	label var pub_health_dist_r`i'	 "Mins. to nearest pub. health facility'" 

	label define lbmother_edu 1 "Less than secondary" 2 "Secondary" 3 "At least some college",replace
	label values mother_edu_r`i' lbmother_edu
	}

forvalues i=2(1)2{
*Child, household, and caregiver:
	label var cov_hh_charac_r`i'   "*************** HOUSEHOLD CHARACTERISTICS- R`i'********************"
	label var cov_hh_econ_r`i' 	   "*************** HOUSEHOLD ECONOMIC STATUS- R`i'********************"
	label var cov_education_r`i'   "*************** CHILD EDUCATION- R`i'********************"
	label var cov_health_r`i'	   "*************** CHILD HEALTH- R`i'********************"
	label var cov_time_use_r`i'	   "*************** CHILD TIME USE- R`i'********************" 
	label var cov_hh_risk_r`i'	   "*************** HOUSEHOLD RISK FACTORS- R`i'********************"
	label var cov_caregiver_r`i'   "*************** CAREGIVER PERCEPTIONS- R`i'********************" 
	label var cov_area_r`i' 	   "*************** AREA CHARACTERISTICS- R`i'********************"
	label var cov_others_r`i'	   "*************** OTHER COVARIATES- R`i'********************"
	}
	
forvalues i=3(1)5{
		*Variable labels:	
	if `i'==3{
		label var subjective_wellbeing_r`i' "Subjective Wellbeing"
		label var agency_r`i' 				"Agency"
		label var selfesteem_r`i' 			"Self-esteem"	
		label var ppvt_r`i' "Vocabulary score (PPVT)"
		label var math_r`i'	"Math score"
		}
		else{
			label var subjective_wellbeing_r`i' "Subjective Wellbeing"
			label var agency_r`i' 				"Agency"
			label var selfesteem_r`i' 			"Self-esteem"
			label var social_skills_r`i'		"Social skills"
			label var selfefficacy_r`i'			"Self-efficacy"
			label var parent_relationship_r`i' 	"Relationship w/parents"
			label var ppvt_r`i' "Vocabulary score (PPVT)"
			label var math_r`i'	"Math score"
			}
	}

forvalues i=1(1)5{
	label var round`i' "****************ROUND `i'***************************"
	}
	
*Treatment:
	label var treatment 	  	"Father absent"
	label var outcomes		  	"********************************OUTCOMES******************************"
	label var outcomes_cog	  	"********************************OUTCOMES: COGNITIVE******************************"
	label var outcomes_non_cog	"********************************OUTCOMES: NON COGNITIVE******************************"
	
	
	label var cda_r2				"Cognitive Development and Achievement (CDA) score"
	label var egra_r3				"Reading score (EGRA)"
	label var reads_well_r3 		"Reads well (CDA)"
	label var writes_well_r3		"Writes well (CDA)"
	label var lang_r4				"Language score"
	label var reading_r5			"Reading score"
	label var gender_equality_r5	"Gender equality"
*Value labels: 
	label define lbfilter 0 "Effective sample" 1 "Not living with their mothers (age 1)" 2 "No information of father involvement", replace
	label values filter lbfilte
	
	