********************************************************************************************************************
*Setting the globals:
global root			"C:\Users\jhony\Google Drive\Universidades\UCL\MSc. Social Policy and Social Research\3. Dissertation\Dissertation\Data"
global inputs		"$root\1.Build\3.Outputs"
global aux			"$root\1.Build\2.Aux"
global outputs_1	"$root\2.Analysis\3.Outputs\2.balance"
global outputs_2	"$root\2.Analysis\3.Outputs\3.impact"
global code			"$root\1.Build\0.Code"
********************************************************************************************************************

*Building databases for individual rounds:
forvalues i=1(1)5{
	run "$code/`i'.round`i'.do"
	}
*Merging all rounds and creating the final database:
run "$code/7.final_base.do"	