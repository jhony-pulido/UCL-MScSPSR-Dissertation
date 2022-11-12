********************************************************************************************************************
*Setting the globals:
global root		"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs	"$root\2.Analysis\3.Outputs\1.alternatives_eval"
global aux		"$root\2.Analysis\2.Aux"
global outputs	"$root\2.Analysis\3.Outputs\Figures"
global code		"$root\2.Analysis\0.Code"
********************************************************************************************************************
*run "1.matching_alternatives.do"

use "$inputs/matching_alternatives.dta", clear
	label var treat_raw 			"Treatment mean before matching"
	label var control_raw 			"Control mean before matching"
	label var treat_matched 		"Treatment mean after matching"
	label var control_matched 		"Control mean after matching"
	label var var_ratio_raw 		"Variance ratio before matching"
	label var var_ratio_matched 	"Variance ratio after matching"
	label var var_ratio_raw_adj 	"Variance ratio (adjusted) before matching"
	label var var_ratio_matched_adj "Variance ratio (adjusted) after matching"

	label var std_dif_raw 			"Standardised dif. before matching"
	label var std_dif_matched 		"Standardised dif. after matching"
	label var std_dif_raw_abs 		"Standardised dif. (absolute value) before matching"
	label var std_dif_matched_abs 	"Standardised dif. (absolute value) after matching"

	label var treated_excluded 		"Number of excluded treated units"
	label var metric				"Matching metric"
	label var regression			"Regression used to calculate PS"
	label var matching				"Matching algorithm"

	order (matching metric regression treated_excluded treat_raw control_raw treat_matched control_matched std_dif_raw std_dif_raw_abs std_dif_matched std_dif_matched_abs var_ratio_raw ///
			var_ratio_raw_adj var_ratio_matched var_ratio_matched_adj), after (variable)

	bys metric regression matching: egen dropme=max(treated_excluded)
	replace treated_excluded=cond(treated_excluded==.,dropme,treated_excluded)
	drop dropme
	replace treated_excluded=cond(matching=="NN",0,treated_excluded)
	
	replace metric=cond(metric=="Mahalanobis", "Mahalanobis Distance",metric)
	replace metric=cond(metric=="PS", "Propensity score",metric)
	replace matching=cond(matching=="Entropy", "Entropy balancing",matching)
	
	*Generate mean values:
	collapse (mean) treated_excluded std_dif_raw_abs std_dif_matched_abs var_ratio_raw_adj var_ratio_matched_adj, by(metric regression matching )

	set obs 7
	replace matching = "Before matching" in 7
	replace metric = "N/A" in 7
	replace regression = "N/A" in 7

	sum std_dif_raw_abs if matching=="NN" & regression=="logit"
	replace std_dif_matched_abs = r(mean) in 7

	sum var_ratio_raw_adj if matching=="NN" & regression=="logit"
	replace var_ratio_matched_adj= r(mean) in 7

	drop std_dif_raw_abs var_ratio_raw_adj

	rename (std_dif_matched_abs var_ratio_matched_adj) (std_dif_abs var_ratio_adj)
	label var std_dif_abs 	 "Standardised dif. (absolute value)"
	label var var_ratio_adj  "Variance ratio (adjusted)"

	gen alternative=cond(metric!="N/A",metric+"-"+matching,matching) 

	graph dot std_dif_abs, over(alternative, sort (std_dif_abs) descending) ytitle(Mean standardised difference (absolute values), margin(small)) ylabel(0(0.01)0.22, labsize(small) grid) ///
								name(graph_std_dif_1,replace)
	*graph export "$outputs/alt_eval_dots_difsd.png", as(png) replace
	
	graph dot var_ratio_adj, over(alternative, sort ( var_ratio_adj ) descending) ytitle(Mean variance ratio (adjusted values), margin(small)) exclude0 ///
								 ylabel(1(0.04)1.63, grid labsize(small)) name(graph_var_1,replace)
	*graph export "$outputs/alt_eval_dots_var.png", as(png) replace
		
	graph combine graph_std_dif_1 graph_var_1, col(2) xsize(10) name(alternatives_test_1,replace)
graph export "$outputs/figure2.png", as(png) replace

	


						