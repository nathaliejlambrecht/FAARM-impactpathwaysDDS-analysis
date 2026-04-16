# -------------------------------------------------------------------
# Supplemental SEM model for estimating direct and indirect effects 
# of the FAARM intervention on women's dietary diversity, including
# women's empowerment mediators.
# 
# Data: FAARM_PIP_data.csv
#
# - Models are adjusted for cluster (ps_c_code).
# - Missing data handled using FIML.
# - Random seed set for Monte Carlo CIs.
#
# R version: 4.5
# lavaan version: 0.6-19
# semTools version: 0.5-7
# -------------------------------------------------------------------

# Load required libraries
library(lavaan) # for SEM modeling
library(semTools) # for calculating Monte Carlo CIs


#### Load Data ####
# Read the analysis dataset (FAARM_PIP_data.csv) from the current working directory.
aggregated <- read.csv(
  file="FAARM_PIP_data.csv",
  header=TRUE,
  sep = ",",
  dec = "."
)

# Set categorical variable as factor
aggregated$religion <- as.factor(aggregated$religion) 

# Generate dummy indicators for baseline wealth quintile (omitted reference: quintile 1)
aggregated$bl_quint1 <- as.numeric(aggregated$bl_quint_all == 1)
aggregated$bl_quint2 <- as.numeric(aggregated$bl_quint_all == 2)
aggregated$bl_quint3 <- as.numeric(aggregated$bl_quint_all == 3)
aggregated$bl_quint4 <- as.numeric(aggregated$bl_quint_all == 4)
aggregated$bl_quint5 <- as.numeric(aggregated$bl_quint_all == 5)

# Generate dummy indicators for education category (reference: none/0)
aggregated$edu_cat1 <- as.numeric(aggregated$edu_cat == 0)
aggregated$edu_cat2 <- as.numeric(aggregated$edu_cat == 1)
aggregated$edu_cat3 <- as.numeric(aggregated$edu_cat == 2)
aggregated$edu_cat4 <- as.numeric(aggregated$edu_cat == 3)
aggregated$edu_cat5 <- as.numeric(aggregated$edu_cat == 4)

# Log transform land size variables
aggregated$log_bl_homeland <- log1p(aggregated$bl_homeland)  # log(var + 1)
aggregated$log_bl_agland <- log1p(aggregated$bl_agland)  # log(var + 1)


############################# SEM for Appendix 9 & Appendix 10 #################################

model <- '
# Structural equations (direct and indirect pathways)
dds_avg ~ cp*treatment + b1*poultrynum_avg + b2*poultryegg_avg + b3*gardenprac + b4*cropsr + b5*knfg_score_el + b6*knowscore + b7*el_ss_cont + b8*el_huscomm_cont + b9*el_extcomm_cont + b10*el_dec_cont + b11*el_selfefficacy + b12*el_network_cont + b13*el_lefthome + b14*el_income + b15*el_dec_income + b16*el_emp_market + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
poultrynum_avg ~ a1*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ a2*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ a3*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ a4*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
knfg_score_el ~ a5*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
knowscore ~ a6*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_ss_cont ~ a7*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_huscomm_cont ~ a8*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_extcomm_cont ~ a9*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_dec_cont ~ a10*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_selfefficacy ~ a11*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_network_cont ~ a12*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_lefthome ~ a13*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_income ~ a14*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_dec_income ~ a15*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_emp_market ~ a16*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac

knowscore ~~ knfg_score_el

el_ss_cont ~~ el_huscomm_cont
el_ss_cont ~~ el_extcomm_cont
el_ss_cont ~~ el_dec_cont
el_ss_cont ~~ el_selfefficacy
el_ss_cont ~~ el_network_cont
el_ss_cont ~~ el_lefthome
el_ss_cont ~~ el_income
el_ss_cont ~~ el_dec_income

el_huscomm_cont ~~ el_extcomm_cont
el_huscomm_cont ~~ el_dec_cont
el_huscomm_cont ~~ el_selfefficacy
el_huscomm_cont ~~ el_network_cont
el_huscomm_cont ~~ el_lefthome
el_huscomm_cont ~~ el_income
el_huscomm_cont ~~ el_dec_income

el_extcomm_cont ~~ el_dec_cont
el_extcomm_cont ~~ el_selfefficacy
el_extcomm_cont ~~ el_network_cont
el_extcomm_cont ~~ el_lefthome
el_extcomm_cont ~~ el_income
el_extcomm_cont ~~ el_dec_income

el_dec_cont ~~ el_selfefficacy
el_dec_cont ~~ el_network_cont
el_dec_cont ~~ el_lefthome
el_dec_cont ~~ el_income
el_dec_cont ~~ el_dec_income

el_selfefficacy ~~ el_network_cont
el_selfefficacy ~~ el_lefthome
el_selfefficacy ~~ el_income
el_selfefficacy ~~ el_dec_income

el_network_cont ~~ el_lefthome
el_network_cont ~~ el_income
el_network_cont ~~ el_dec_income

el_lefthome ~~ el_income
el_lefthome ~~ el_dec_income

el_income ~~ el_dec_income

el_emp_market ~~ el_huscomm_cont
el_emp_market ~~ el_extcomm_cont
el_emp_market ~~ el_dec_cont
el_emp_market ~~ el_selfefficacy
el_emp_market ~~ el_network_cont
el_emp_market ~~ el_lefthome
el_emp_market ~~ el_income
el_emp_market ~~ el_dec_income
el_emp_market ~~ el_ss_cont

# Define all direct, indirect, and total effects
directeffect := cp
indirecteffect1 := a1*b1    ## poultry number
indirecteffect2 := a2*b2    ## poultry egg
indirecteffect3 := a3*b3    ## garden practices 
indirecteffect4 := a4*b4    ## crop species richness
indirecteffect5 := a5*b5    ## nutrition knowledge
indirecteffect6 := a6*b6    ## knowscore
indirecteffect7 := a7*b7    ## social support
indirecteffect8 := a8*b8    ## communication with husband
indirecteffect9 := a9*b9    ## external communication
indirecteffect10 := a10*b10 ## decision making  
indirecteffect11 := a11*b11 ## self-efficacy
indirecteffect12 := a12*b12 ## network score
indirecteffect13 := a13*b13 ## mobility
indirecteffect14 := a14*b14 ## own income
indirecteffect15 := a15*b15 ## decision making on own income
indirecteffect16 := a16*b16 ## market activity

# Define path estimates
poultrynum_a := a1
poultryegg_a := a2
gardenprac_a := a3
cropsr_a := a4
knfg_score_a := a5
knowscore_a := a6
support_a := a7
huscommunication_a := a8
extcommunication_a := a9
decision_a := a10
selfefficacy_a := a11
network_a := a12
mobility_a := a13
income_a := a14
incomedec_a := a15
market_a := a16

poultrynum_b := b1
poultryegg_b := b2
gardenprac_b := b3
cropsr_b := b4
knfg_score_b := b5
knowscore_b := b6
support_b := b7
huscommunication_b := b8
extcommunication_b := b9
decision_b := b10
selfefficacy_b := b11
network_b := b12
mobility_b := b13
income_b := b14
incomedec_b := b15
market_b := b16

# Aggregate and categorize direct and indirect effects by pathways and domains
totaleffect := directeffect + indirecteffect1 + indirecteffect2 + indirecteffect3 + indirecteffect4 + indirecteffect5 + indirecteffect6 + indirecteffect7 + indirecteffect8 + indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12 + indirecteffect13 + indirecteffect14 + indirecteffect15 + indirecteffect16

indirect_poultry := indirecteffect1 + indirecteffect2
indirect_poultrynum := indirecteffect1 
indirect_poultryegg := indirecteffect2 

indirect_garden := indirecteffect3 + indirecteffect4
indirect_gardenprac := indirecteffect3 
indirect_gardensr := indirecteffect4

indirect_knowledge := indirecteffect5 + indirecteffect6
indirect_fgknow := indirecteffect5
indirect_ddknow := indirecteffect6

indirect_empowerment := indirecteffect7 + indirecteffect8 + indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12 + indirecteffect13 + indirecteffect14 + indirecteffect15 
indirect_support := indirecteffect7
indirect_huscommunication := indirecteffect8
indirect_extcommunication := indirecteffect9 
indirect_decision := indirecteffect10
indirect_selfefficacy := indirecteffect11
indirect_network := indirecteffect12
indirect_mobility := indirecteffect13
indirect_income := indirecteffect14
indirect_incomedec := indirecteffect15

indirect_market := indirecteffect16

total_indirect := indirecteffect1 + indirecteffect2 + indirecteffect3 + indirecteffect4 + indirecteffect5 + indirecteffect6 + indirecteffect7 + indirecteffect8 + indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12  + indirecteffect13 + indirecteffect14 + indirecteffect15 + indirecteffect16 

pct_direct_total := directeffect/totaleffect*100
pct_poultry_total := indirect_poultry/totaleffect*100
pct_indirect_garden_total := indirect_garden/totaleffect*100
pct_nutrition_total := indirect_knowledge/totaleffect*100
pct_market_total := indirect_market/totaleffect*100
pct_empowerment_total := indirect_empowerment/totaleffect*100
pct_indirect_total := total_indirect/totaleffect*100
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
m1 <- sem(model, data=aggregated, missing="fiml", cluster="ps_c_code")

# Output summary
summary(m1, standardized=TRUE)
parameterestimates_emp <- parameterEstimates(m1)

# Calculate goodness of fit indices for overall model fit
fit_indices_emp <- fitMeasures(m1, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices_emp)

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234) # Set random seed for replicable confidence intervals
mc_ci_emp <- monteCarloCI(m1)

# Save as data.frames
parameter_df_emp <- as.data.frame(parameterestimates_emp)
fit_df_emp <- data.frame(index = names(fit_indices_emp), value=as.numeric(fit_indices_emp))
mcci_df_emp <- data.frame(parameter = row.names(mc_ci_emp), mc_ci_emp)
