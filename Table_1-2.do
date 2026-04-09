use "FAARM_PIP_data.dta"

*Convert land size from "decimal" into square meter 
gen bl_homelandm2 = bl_homeland * 40.4686
gen bl_aglandm2 = bl_agland * 40.4686

********************************************************************************
**# Table 1 - Baseline characteristics
********************************************************************************
tab treatment

//Woman and household variables
*Relgion, education category, baseline minimum dietary diversity
foreach var of varlist religion edu_cat bl_mdd {
	proportion `var' , over(treatment) cluster(ps_c_code)
}

*Wealth quintile, excluding imputed values
proportion bl_quint_all if bl_quint_missing!=1, over(treatment) cluster(ps_c_code)

*Baseline dietary diversity score, excluding imputed values
mean bl_dds_m if impute_dd!=1, over(treat) vce(cluster ps_c_code)
	estat sd
	bysort treatment: tabstat bl_dds_m if impute_dd!=1, stats(n)
	
*Woman's age
mean bl_age, over(treat) vce(cluster ps_c_code)
	estat sd

//Agricultural variables, excluding imputed values
foreach var of varlist bl_homelandm2 bl_aglandm2 bl_cropsr {
	mean `var' if impute_ag!=1, over(treat) vce(cluster ps_c_code)				
	estat sd
	bysort treatment: tabstat `var' if impute_ag!=1, stats(n)
}

//Market variable, excluding imputed values
mean bl_market if impute_market!=1, over(treat) vce(cluster ps_c_code)
	estat sd
	bysort treatment: tabstat bl_market if impute_market!=1, stats(n)

//Empowerment variables, excluding imputed values
foreach var of varlist bl_lefthome bl_ss_cont bl_huscomm_cont bl_extcomm_cont bl_dec_cont bl_income {
	mean `var', over(treat) vce(cluster ps_c_code)		
	estat sd
	bysort treatment: tabstat `var', stats(n)
}


********************************************************************************
**# Table 2 - Impacts on women's dietary diversity and mediators
********************************************************************************
//Garden practices score, number of poultry owned, number of eggs produced, nutrition knowledge scores
foreach var of varlist gardenprac poultrynum_avg poultryegg_avg knfg_score_el knowscore {
	mean `var' , over(treat) vce(cluster ps_c_code)
	estat sd
	mixed `var' i.treatment || ps_c_code:
}

//Dietary diversity score and met minimum dietary diversity, controlling for baseline dietary diversity score
foreach var of varlist dds_avg mdd_avg {
	mean `var' , over(treat) vce(cluster ps_c_code)
	estat sd
	mixed `var' i.treatment bl_dds_m || ps_c_code:
}

//Crop species richness, controlling for baseline crop species richness
foreach var of varlist cropsr {
	mean `var' , over(treat) vce(cluster ps_c_code)
	estat sd
	mixed `var' i.treatment bl_cropsr || ps_c_code:
}

//Market score, controlling for baseline market score
foreach var of varlist el_emp_market {
	mean `var' , over(treat) vce(cluster ps_c_code)
	estat sd
	mixed `var' i.treatment bl_market || ps_c_code:
}
