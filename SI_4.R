# -------------------------------------------------------------------
# SEM model for estimating direct and indirect effects of the FAARM
# intervention on women's dietary diversity, with separate market
# variables.
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


############################# SEM for Appendix 4 #################################

model <- '
# Structural equations (direct and indirect pathways)
dds_avg ~ cp*treatment + b1*poultrynum_avg + b2*poultryegg_avg + b3*gardenprac + b4*cropsr + b5*knfg_score_el + b6*knowscore + b7*el_emp_buygoods + b8*el_emp_buymarket + b9*el_emp_soldgoods + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
poultrynum_avg ~ a1*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ a2*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ a3*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ a4*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
knfg_score_el ~ a5*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
knowscore ~ a6*treatment  + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
el_emp_buygoods ~ a7*treatment + d1*poultrynum_avg + d2*poultryegg_avg + d3*cropsr + d4*gardenprac + d5*knfg_score_el + d6*knowscore + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_emp_buymarket ~ a8*treatment + d7*poultrynum_avg + d8*poultryegg_avg + d9*cropsr + d10*gardenprac + d11*knfg_score_el + d12*knowscore + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 
el_emp_soldgoods ~ a9*treatment + d13*poultrynum_avg + d14*poultryegg_avg + d15*cropsr + d16*gardenprac + d17*knfg_score_el + d18*knowscore + religion + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market 

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac
knfg_score_el ~~ knowscore

el_emp_buygoods ~~ el_emp_buymarket
el_emp_buygoods ~~ el_emp_soldgoods
el_emp_buymarket ~~ el_emp_soldgoods

# Define all direct, indirect, and total effects

# Direct effect
directeffect := cp

# Poultry Number
indirecteffect1 := a1*b1                           # poultry number, neither market
indirecteffect2b := a1*d1*b7                       # market pathway: through buying
indirecteffect2m := a1*d7*b8                       # market pathway: through market days buying
indirecteffect2s := a1*d13*b9                      # market pathway: through selling

# Poultry Eggs
indirecteffect3 := a2*b2
indirecteffect4b := a2*d2*b7
indirecteffect4m := a2*d8*b8
indirecteffect4s := a2*d14*b9

# Garden practices
indirecteffect5 := a3*b3
indirecteffect6b := a3*d4*b7
indirecteffect6m := a3*d10*b8
indirecteffect6s := a3*d16*b9

# Crop species richness
indirecteffect7 := a4*b4
indirecteffect8b := a4*d3*b7
indirecteffect8m := a4*d9*b8
indirecteffect8s := a4*d15*b9

# Food Group Knowledge
indirecteffect9 := a5*b5
indirecteffect10b := a5*d5*b7
indirecteffect10m := a5*d11*b8
indirecteffect10s := a5*d17*b9

# Diet Diversity Knowledge
indirecteffect11 := a6*b6
indirecteffect12b := a6*d6*b7
indirecteffect12m := a6*d12*b8
indirecteffect12s := a6*d18*b9

# Each market variable direct a-b path
indirecteffect13 := a7*b7              # bought goods
indirecteffect14 := a8*b8              # go to market to buy
indirecteffect15 := a9*b9              # sold goods

# a, b, d paths
poultrynum_a := a1
poultryegg_a := a2
gardenprac_a := a3
cropsr_a := a4
knfg_score_a := a5
knowscore_a := a6
buygoods_a := a7
wommarketdays_buy_a := a8
soldgoods_a := a9

poultrynum_b := b1
poultryegg_b := b2
gardenprac_b := b3
cropsr_b := b4
knfg_score_b := b5
knowscore_b := b6
buygoods_b := b7
wommarketdays_buy_b := b8
soldgoods_b := b9

poultrynum_d_buy := d1
poultryegg_d_buy := d2
gardenprac_d_buy := d4
cropsr_d_buy := d3
knfg_score_d_buy := d5
knowscore_d_buy := d6

poultrynum_d_marketdays := d7
poultryegg_d_marketdays := d8
gardenprac_d_marketdays := d10
cropsr_d_marketdays := d9
knfg_score_d_marketdays := d11
knowscore_d_marketdays := d12

poultrynum_d_sold := d13
poultryegg_d_sold := d14
gardenprac_d_sold := d16
cropsr_d_sold := d15
knfg_score_d_sold := d17
knowscore_d_sold := d18

# -- Aggregate and categorize direct and indirect effects by pathways and domains

# Total effect
totaleffect := directeffect +
  indirecteffect1 + indirecteffect2b + indirecteffect2m + indirecteffect2s +
  indirecteffect3 + indirecteffect4b + indirecteffect4m + indirecteffect4s +
  indirecteffect5 + indirecteffect6b + indirecteffect6m + indirecteffect6s +
  indirecteffect7 + indirecteffect8b + indirecteffect8m + indirecteffect8s +
  indirecteffect9 + indirecteffect10b + indirecteffect10m + indirecteffect10s +
  indirecteffect11 + indirecteffect12b + indirecteffect12m + indirecteffect12s +
  indirecteffect13 + indirecteffect14 + indirecteffect15

# ----------- POULTRY PRODUCTION ---------
# Direct paths (not via any market mediator)
indirect_poultry := indirecteffect1 +         # poultry number (a1*b1)
                    indirecteffect3           # poultry eggs (a2*b2)

# Poultry, including all market mediated paths
indirect_poultry_mrkt :=  indirecteffect1 +                                                 # poultry num direct
                          indirecteffect2b + indirecteffect2m + indirecteffect2s +          # poultry num via market
                          indirecteffect3 +                                                 # poultry eggs direct
                          indirecteffect4b + indirecteffect4m + indirecteffect4s            # poultry eggs via market

indirect_poultrynum := indirecteffect1
indirect_poultryegg := indirecteffect3
indirect_poultrynummrkt := indirecteffect2b + indirecteffect2m + indirecteffect2s
indirect_poultryeggmrkt := indirecteffect4b + indirecteffect4m + indirecteffect4s

# ----------- GARDEN PRODUCTION ---------
indirect_garden := indirecteffect5 +         # garden practices (a3*b3)
                   indirecteffect7           # crop species richness (a4*b4)

indirect_garden_mrkt :=  indirecteffect5 +                                                         # garden prac direct
                         indirecteffect6b + indirecteffect6m + indirecteffect6s +                  # garden prac via market
                         indirecteffect7 +                                                         # crop species richness direct
                         indirecteffect8b + indirecteffect8m + indirecteffect8s                    # crop species richness via market

indirect_gardenprac := indirecteffect5
indirect_gardensr := indirecteffect7
indirect_gardenpracmrkt := indirecteffect6b + indirecteffect6m + indirecteffect6s
indirect_gardensrmrkt := indirecteffect8b + indirecteffect8m + indirecteffect8s

# ----------- KNOWLEDGE ----------------
indirect_knowledge := indirecteffect9 +   # food group knowledge score (a5*b5)
                      indirecteffect11    # diet diversity knowledge score (a6*b6)
indirect_knowledge_mrkttot := indirecteffect9 + 
                              indirecteffect10b + indirecteffect10m + indirecteffect10s +
                              indirecteffect11 +
                              indirecteffect12b + indirecteffect12m + indirecteffect12s
indirect_fgknow := indirecteffect9
indirect_ddknow := indirecteffect11
indirect_fgknowmrkt := indirecteffect10b + indirecteffect10m + indirecteffect10s 
indirect_ddknowmrket := indirecteffect12b + indirecteffect12m + indirecteffect12s

# ----------- MARKET -------------------
indirect_market :=  indirecteffect13 + indirecteffect14 + indirecteffect15 +                      # direct (a7*b7, a8*b8, a9*b9)
                    indirecteffect2b + indirecteffect2m + indirecteffect2s +                      # poultry num via market
                    indirecteffect4b + indirecteffect4m + indirecteffect4s +                      # poultry egg via market
                    indirecteffect6b + indirecteffect6m + indirecteffect6s +                      # garden prac via market
                    indirecteffect8b + indirecteffect8m + indirecteffect8s +                      # crop species richness via market
                    indirecteffect10b + indirecteffect10m + indirecteffect10s +                   # fg knowledge via market
                    indirecteffect12b + indirecteffect12m + indirecteffect12s                     # dd knowledge via market
                    
indirect_market_alone := indirecteffect13 + indirecteffect14 + indirecteffect15

indirect_market_buypaths := indirecteffect2b + indirecteffect4b + indirecteffect6b + indirecteffect8b + indirecteffect10b + indirecteffect12b
indirect_market_marketdayspaths := indirecteffect2m + indirecteffect4m + indirecteffect6m + indirecteffect8m + indirecteffect10m + indirecteffect12m
indirect_market_sellpaths := indirecteffect2s + indirecteffect4s + indirecteffect6s + indirecteffect8s + indirecteffect10s + indirecteffect12s

# ----------- TOTAL INDIRECT -----------
total_indirect :=  indirecteffect1 +                                                 # poultry num direct
                   indirecteffect2b + indirecteffect2m + indirecteffect2s +          # poultry num market
                   indirecteffect3 +                                                 # poultry egg direct
                   indirecteffect4b + indirecteffect4m + indirecteffect4s +          # poultry egg market
                   indirecteffect5 +                                                 # garden prac direct
                   indirecteffect6b + indirecteffect6m + indirecteffect6s +          # garden prac market
                   indirecteffect7 +                                                 # crop richness direct
                   indirecteffect8b + indirecteffect8m + indirecteffect8s +          # crop richness market
                   indirecteffect9 +                                                 # fg knowledge direct
                   indirecteffect10b + indirecteffect10m + indirecteffect10s +       # fg knowledge market
                   indirecteffect11 +                                                # dd knowledge direct
                   indirecteffect12b + indirecteffect12m + indirecteffect12s +       # dd knowledge market
                   indirecteffect13 + indirecteffect14 + indirecteffect15            # all market mediators direct

# ----------- PROP. MEDIATED -----------
pct_direct_total := directeffect / totaleffect * 100
pct_poultry_total := indirect_poultry / totaleffect * 100
pct_poultrymrkt_total := indirect_poultry_mrkt / totaleffect * 100
pct_garden_total := indirect_garden / totaleffect * 100
pct_gardenmrkt_total := indirect_garden_mrkt / totaleffect * 100
pct_knowledge_total := indirect_knowledge / totaleffect * 100
pct_knowledgemrkt_total := indirect_knowledge_mrkttot / totaleffect * 100
pct_market_total := indirect_market / totaleffect * 100
pct_marketalone := indirect_market_alone / totaleffect * 100
pct_indirect_total := total_indirect / totaleffect * 100
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
m1 <- sem(model, data=aggregated, missing="fiml", cluster="ps_c_code")

# Output summary
summary(m1, standardized=TRUE)
parameterestimates_market <- parameterEstimates(m1)

# Calculate goodness of fit indices for overall model fit
fit_indices_market <- fitMeasures(m1, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices_market)

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234) # Set random seed for replicable confidence intervals
mc_ci_market <- monteCarloCI(m1)

# Save as data.frames
parameter_df_market <- as.data.frame(parameterestimates_market)
fit_df_market <- data.frame(index = names(fit_indices_market), value=as.numeric(fit_indices_market))
mcci_df_market <- data.frame(parameter = row.names(mc_ci_market), mc_ci_market)
