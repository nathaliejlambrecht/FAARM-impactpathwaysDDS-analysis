# -------------------------------------------------------------------
# SEM model for estimating direct and indirect effects of the FAARM
# intervention on the proportion of times women met minimum dietary diversity.
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


############################# SEM for Appendix 5 & Appendix 6 #################################

model <- '
# Structural equations (direct and indirect pathways)
mdd_avg ~ cp*treatment + b1*poultrynum_avg + b2*poultryegg_avg + b3*gardenprac + b4*cropsr + b5*knfg_score_el + b6*knowscore + b7*el_emp_market + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultrynum_avg ~ a1*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ a2*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ a3*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ a4*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knfg_score_el ~ a5*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knowscore ~ a6*treatment + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
el_emp_market ~ a7*treatment + d1*poultrynum_avg + d2*poultryegg_avg + d3*gardenprac + d4*cropsr + d5*knfg_score_el + d6*knowscore + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac
knowscore ~~ knfg_score_el

# Define all direct, indirect, and total effects
directeffect := cp

indirecteffect1 := a1*b1 ## poultry number (but not marketing)
indirecteffect2 := a1*d1*b7 ## poultry number and marketing

indirecteffect3 := a2*b2 ## poultry egg (but not marketing)
indirecteffect4 := a2*d2*b7 ## poultry egg and marketing

indirecteffect5 := a3*b3 ## garden practices (but not marketing)
indirecteffect6 := a3*d3*b7 ## garden practices and marketing

indirecteffect7 := a4*b4 ## crop species richness (but not marketing)
indirecteffect8 := a4*d4*b7 ## crop species richness and marketing

indirecteffect9 := a5*b5 ## food group knowledge (but not marketing) 
indirecteffect10 := a5*d5*b7 ## food group knowledge and marketing

indirecteffect11 := a6*b6 ## diet diversity knowledge (but not marketing) 
indirecteffect12 := a6*d6*b7 ## diet diversity knowledge and marketing

indirecteffect13 := a7*b7 ## market activity

# Define path estimates
poultrynum_a := a1
poultryegg_a := a2
gardenprac_a := a3
cropsr_a := a4
knfg_score_a := a5
knowscore_a := a6
market_a := a7

poultrynum_b := b1
poultryegg_b := b2
gardenprac_b := b3
cropsr_b := b4
knfg_score_b := b5
knowscore_b := b6
market_b := b7

poultrynum_d := d1
poultryegg_d := d2
gardenprac_d := d3
cropsr_d := d4
knfg_score_d := d5
knowscore_d := d6


# Aggregate and categorize direct and indirect effects by pathways and domains
totaleffect := directeffect + indirecteffect1 + indirecteffect2 + indirecteffect3 + indirecteffect4 + indirecteffect5 + indirecteffect6 + indirecteffect7 + indirecteffect8 + indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12 + indirecteffect13

indirect_poultry := indirecteffect1 + indirecteffect3
indirect_poultry_mrkt := indirecteffect1 + indirecteffect2 + indirecteffect3 + indirecteffect4
indirect_poultrynum := indirecteffect1 
indirect_poultryegg := indirecteffect3 
indirect_poultrynummrkt := indirecteffect2
indirect_poultryeggmrkt := indirecteffect4

indirect_garden := indirecteffect5 + indirecteffect7
indirect_garden_mrkt := indirecteffect5 + indirecteffect6 + indirecteffect7 + indirecteffect8
indirect_gardenprac := indirecteffect5 
indirect_gardensr := indirecteffect7
indirect_gardenpracmrkt := indirecteffect6
indirect_gardensrmrkt := indirecteffect8

indirect_knowledge := indirecteffect9 + indirecteffect11
indirect_knowledge_mrkttot := indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12
indirect_fgknowledge := indirecteffect9
indirect_ddknowledge := indirecteffect11
indirect_fgknowmrkt := indirecteffect10
indirect_ddknowmrkt := indirecteffect12

indirect_market := indirecteffect13 + indirecteffect2 + indirecteffect4 + indirecteffect6 + indirecteffect8 + indirecteffect10 + indirecteffect12
indirect_market_alone := indirecteffect13
indirect_market_dpaths := indirecteffect2 + indirecteffect4 + indirecteffect6 + indirecteffect8 + indirecteffect10 + indirecteffect12

total_indirect := indirecteffect1 + indirecteffect2 + indirecteffect3 + indirecteffect4 + indirecteffect5 + indirecteffect6 + indirecteffect7 + indirecteffect8 + indirecteffect9 + indirecteffect10 + indirecteffect11 + indirecteffect12 + indirecteffect13

pct_direct_total := directeffect/totaleffect*100
pct_poultry_total := indirect_poultry/totaleffect*100
pct_poultrymrkt_total := indirect_poultry_mrkt/totaleffect*100
pct_garden_total := indirect_garden/totaleffect*100
pct_gardenmrkt_total := indirect_garden_mrkt/totaleffect*100
pct_knowledge_total := indirect_knowledge/totaleffect*100
pct_knowledgemrkt_total := indirect_knowledge_mrkttot/totaleffect*100
pct_market_total := indirect_market/totaleffect*100
pct_marketalone := indirect_market_alone/totaleffect*100
pct_indirect_total := total_indirect/totaleffect*100
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
m1 <- sem(model, data=aggregated, missing="fiml", cluster="ps_c_code")

# Output summary
summary(m1, standardized=TRUE)
parameterestimates <- parameterEstimates(m1)

# Calculate goodness of fit indices for overall model fit
fit_indices <- fitMeasures(m1, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices)

# Calculate standardized estimates
std_est <- standardizedSolution(m1, type = "std.nox")

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234) # Set random seed for replicable confidence intervals
mc_ci <- monteCarloCI(m1)
print(mc_ci)

# Save as data.frames
parameter_df <- as.data.frame(parameterestimates)
fit_df <- data.frame(index = names(fit_indices), value=as.numeric(fit_indices))
mcci_df <- data.frame(parameter = row.names(mc_ci), mc_ci)
parameter_std_df <- as.data.frame(std_est)
