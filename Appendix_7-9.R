# -------------------------------------------------------------------
# Heterogeneity analysis for estimating direct and indirect effects 
# of the FAARM intervention on women's dietary diversity score.
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
library(dplyr)

#### Load Data ####
# Read the analysis dataset (FAARM_PIP_data.csv) from the current working directory.
aggregated <- read.csv(
  file="FAARM_PIP_data.csv",
  header=TRUE,
  sep = ",",
  dec = "."
)

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

#### Define binary indicators for wealth quintile and women's education ####
# Create binary wealth category (1 = low, 2 = high)
aggregated <- aggregated %>%
  mutate(bl_quint_category = case_when(
    bl_quint_all %in% c(1, 2) ~ 1,
    bl_quint_all %in% c(3, 4, 5) ~ 2
  ))

# Create binary education category (1 = lower, 2 = higher)
aggregated <- aggregated %>%
  mutate(edu_cat_bin = case_when(
    edu_cat %in% c(0, 1, 2) ~ 1,
    edu_cat %in% c(3, 4) ~ 2
  ))

#Convert to factor variables and label
aggregated$religion <- factor(aggregated$religion,
                              levels = c(1, 2),
                              labels = c("1:Muslim", "2:Hindu"))

aggregated$bl_quint_category <- factor(aggregated$bl_quint_category,
                                       levels = c(1, 2),
                                       labels = c("1:Low wealth (Quint 1-2)", "2:Mid to high wealth (Quint 3-5)"))

aggregated$edu_cat_bin <- factor(aggregated$edu_cat_bin,
                             levels = c(1, 2),
                             labels = c("1:No or primary education (0-2)", "2:Secondary education (3-4)"))

# Log transform land size variables
aggregated$log_bl_homeland <- log1p(aggregated$bl_homeland)  # log(var + 1)
aggregated$log_bl_agland <- log1p(aggregated$bl_agland)  # log(var + 1)


#### Appendix 6: Religion ####
model <- '
# Structural equations (direct and indirect pathways) with group specific coefficients
dds_avg ~ c(cp1,cp2)*treatment + c(b11,b12)*poultrynum_avg + c(b21,b22)*poultryegg_avg + c(b31,b32)*gardenprac + c(b41,b42)*cropsr + c(b51,b52)*knfg_score_el + c(b61,b62)*knowscore + c(b71,b72)*el_emp_market + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultrynum_avg ~ c(a11,a12)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ c(a21,a22)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ c(a31,a32)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ c(a41,a42)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knfg_score_el ~ c(a51,a52)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knowscore ~ c(a61,a62)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
el_emp_market ~ c(a71,a72)*treatment + c(d11,d12)*poultrynum_avg + c(d21,d22)*poultryegg_avg + c(d31,d32)*cropsr + c(d41,d42)*gardenprac + c(d51,d52)*knfg_score_el + c(d61,d62)*knowscore + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac
knfg_score_el ~~ knowscore

# Define all direct, indirect, and total effects by group
directeffectg1 := cp1
directeffectg2 := cp2

indirecteffect1g1 := (a11*b11) ## poultry number (but not marketing) - group 1
indirecteffect1g2 := (a12*b12) ## poultry number (but not marketing) - group 2

indirecteffect2g1 := (a11*d11*b71) ## poultry number and marketing - group 1
indirecteffect2g2 := (a12*d12*b72) ## poultry number and marketing - group 2

indirecteffect3g1 := (a21*b21) ## poultry egg (but not marketing) - group 1
indirecteffect3g2 := (a22*b22) ## poultry egg (but not marketing) - group 2

indirecteffect4g1 := (a21*d21*b71) ## poultry egg and marketing - group 1
indirecteffect4g2 := (a22*d22*b72) ## poultry egg and marketing - group 2

indirecteffect5g1 := (a31*b31) ## garden practices (but not marketing) - group 1
indirecteffect5g2 := (a32*b32) ## garden practices (but not marketing) - group 2

indirecteffect6g1 := (a31*d41*b71) ## garden practices and marketing - group 1
indirecteffect6g2 := (a32*d42*b72) ## garden practices and marketing - group 2

indirecteffect7g1 := (a41*b41) ## crop species richness (but not marketing) - group 1
indirecteffect7g2 := (a42*b42) ## crop species richness (but not marketing) - group 2

indirecteffect8g1 := (a41*d31*b71) ## crop species richness and marketing - group 1
indirecteffect8g2 := (a42*d32*b72) ## crop species richness and marketing - group 2

indirecteffect9g1 := (a51*b51) ## food group knowledge (but not marketing) - group 1
indirecteffect9g2 := (a52*b52) ## food group knowledge (but not marketing) - group 2

indirecteffect10g1 := (a51*d51*b71) ## food group knowledge and marketing - group 1
indirecteffect10g2 := (a52*d52*b72) ## food group knowledge and marketing - group 2


indirecteffect11g1 := (a61*b61) ## diet diversity knowledge (but not marketing) - group 1
indirecteffect11g2 := (a62*b62) ## diet diversity knowledge (but not marketing) - group 2

indirecteffect12g1 := (a61*d61*b71) ## diet diversity knowledge and marketing - group 1
indirecteffect12g2 := (a62*d62*b72) ## diet diversity knowledge and marketing - group 2


indirecteffect13g1 := (a71*b71) ## market activity - group 1
indirecteffect13g2 := (a72*b72) ## market activity - group 2


# -- Aggregate and categorize direct and indirect effects by pathways and domains, by group (g1 = group 1, g2 = group 2)

totaleffectg1 := directeffectg1 + indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1
totaleffectg2 := directeffectg2 + indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Group 1
indirect_poultryg1 := indirecteffect1g1 + indirecteffect3g1
indirect_poultry_mrktg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1
indirect_poultrynumg1 := indirecteffect1g1 
indirect_poultryeggg1 := indirecteffect3g1 
indirect_poultrynummrktg1 := indirecteffect2g1
indirect_poultryeggmrktg1 := indirecteffect4g1

indirect_gardeng1 := indirecteffect5g1 + indirecteffect7g1
indirect_garden_mrktg1 := indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1
indirect_gardenpracg1 := indirecteffect5g1 
indirect_gardensrg1 := indirecteffect7g1
indirect_gardenpracmrktg1 := indirecteffect6g1
indirect_gardensrmrktg1 := indirecteffect8g1

indirect_knowledgeg1 := indirecteffect9g1 + indirecteffect11g1
indirect_knowledge_mrkttotg1 := indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1
indirect_fgknowg1 := indirecteffect9g1
indirect_ddknowg1 := indirecteffect11g1
indirect_fgknowmrktg1 := indirecteffect10g1
indirect_ddknowmrktg1 := indirecteffect12g1

indirect_marketg1 := indirecteffect13g1 + indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1
indirect_market_aloneg1 := indirecteffect13g1
indirect_market_dpathsg1 := indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1

total_indirectg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1

# Calculate proportion mediated by pathways and domains
pct_direct_totalg1 := directeffectg1/totaleffectg1*100
pct_poultry_totalg1 := indirect_poultryg1/totaleffectg1*100
pct_poultrymrkt_totalg1 := indirect_poultry_mrktg1/totaleffectg1*100
pct_garden_totalg1 := indirect_gardeng1/totaleffectg1*100
pct_gardenmrkt_totalg1 := indirect_garden_mrktg1/totaleffectg1*100
pct_knowledge_totalg1 := indirect_knowledgeg1/totaleffectg1*100
pct_knowledgemrkt_totalg1 := indirect_knowledge_mrkttotg1/totaleffectg1*100
pct_market_totalg1 := indirect_marketg1/totaleffectg1*100
pct_marketaloneg1 := indirect_market_aloneg1/totaleffectg1*100
pct_indirect_totalg1 := total_indirectg1/totaleffectg1*100


# Group 2

indirect_poultryg2 := indirecteffect1g2 + indirecteffect3g2
indirect_poultry_mrktg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2
indirect_poultrynumg2 := indirecteffect1g2 
indirect_poultryeggg2 := indirecteffect3g2 
indirect_poultrynummrktg2 := indirecteffect2g2
indirect_poultryeggmrktg2 := indirecteffect4g2

indirect_gardeng2 := indirecteffect5g2 + indirecteffect7g2
indirect_garden_mrktg2 := indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2
indirect_gardenpracg2 := indirecteffect5g2 
indirect_gardensrg2 := indirecteffect7g2
indirect_gardenpracmrktg2 := indirecteffect6g2
indirect_gardensrmrktg2 := indirecteffect8g2

indirect_knowledgeg2 := indirecteffect9g2 + indirecteffect11g2
indirect_knowledge_mrkttotg2 := indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2
indirect_fgknowg2 := indirecteffect9g2
indirect_ddknowg2 := indirecteffect11g2
indirect_fgknowmrktg2 := indirecteffect10g2
indirect_ddknowmrktg2 := indirecteffect12g2

indirect_marketg2 := indirecteffect13g2 + indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2
indirect_market_aloneg2 := indirecteffect13g2
indirect_market_dpathsg2 := indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2

total_indirectg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Calculate proportion mediated by pathways and domains
pct_direct_totalg2 := directeffectg2/totaleffectg2*100
pct_poultry_totalg2 := indirect_poultryg2/totaleffectg2*100
pct_poultrymrkt_totalg2 := indirect_poultry_mrktg2/totaleffectg2*100
pct_garden_totalg2 := indirect_gardeng2/totaleffectg2*100
pct_gardenmrkt_totalg2 := indirect_garden_mrktg2/totaleffectg2*100
pct_knowledge_totalg2 := indirect_knowledgeg2/totaleffectg2*100
pct_knowledgemrkt_totalg2 := indirect_knowledge_mrkttotg2/totaleffectg2*100
pct_market_totalg2 := indirect_marketg2/totaleffectg2*100
pct_marketaloneg2 := indirect_market_aloneg2/totaleffectg2*100
pct_indirect_totalg2 := total_indirectg2/totaleffectg2*100

# -- Calculate group differences (interaction):
diff_direct := directeffectg2 - directeffectg1
diff_total := totaleffectg2 - totaleffectg1
diff_indirect := total_indirectg2 - total_indirectg1

# -- Component-wise differences in mediation by domain
diff_indirect_poultry := indirect_poultryg2 - indirect_poultryg1
diff_indirect_poultry_mrkt := indirect_poultry_mrktg2 - indirect_poultry_mrktg1
diff_indirect_garden := indirect_gardeng2 - indirect_gardeng1
diff_indirect_garden_mrkt := indirect_garden_mrktg2 - indirect_garden_mrktg1
diff_indirect_knowledge := indirect_knowledgeg2 - indirect_knowledgeg1
diff_indirect_knowledge_mrkt := indirect_knowledge_mrkttotg2 - indirect_knowledge_mrkttotg1
diff_indirect_market := indirect_marketg2 - indirect_marketg1
diff_indirect_market_alone := indirect_market_aloneg2 - indirect_market_aloneg1
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
# Set group label
m1 <- sem(
  model, 
  data=aggregated, 
  missing="fiml", 
  cluster="ps_c_code", 
  group="religion",
  group.label=c("2:Hindu", "1:Muslim")
)

# Output summary
summary(m1, standardized=TRUE)
parameterestimates_rel <- parameterEstimates(m1)

# Calculate goodness of fit indices for overall model fit
fit_indices_rel <- fitMeasures(m1, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices_rel)

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234)  # Set random seed for replicable confidence intervals
mc_ci_rel <- monteCarloCI(m1)

# Save as data.frames
parameter_df_rel <- as.data.frame(parameterestimates_rel)
fit_df_rel <- data.frame(index = names(fit_indices_rel), value=as.numeric(fit_indices_rel))
mcci_df_rel <- data.frame(parameter = row.names(mc_ci_rel), mc_ci_rel)




######### Appendix 7: Wealth Category ########
model <- '
# Structural equations (direct and indirect pathways) with group specific coefficients
dds_avg ~ c(cp1,cp2)*treatment + c(b11,b12)*poultrynum_avg + c(b21,b22)*poultryegg_avg + c(b31,b32)*gardenprac + c(b41,b42)*cropsr + c(b51,b52)*knfg_score_el + c(b61,b62)*knowscore + c(b71,b72)*el_emp_market + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultrynum_avg ~ c(a11,a12)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ c(a21,a22)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ c(a31,a32)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ c(a41,a42)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knfg_score_el ~ c(a51,a52)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knowscore ~ c(a61,a62)*treatment  + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
el_emp_market ~ c(a71,a72)*treatment + c(d11,d12)*poultrynum_avg + c(d21,d22)*poultryegg_avg + c(d31,d32)*cropsr + c(d41,d42)*gardenprac + c(d51,d52)*knfg_score_el + c(d61,d62)*knowscore + religion + edu_cat2 + edu_cat3 + edu_cat4 + edu_cat5 + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac
knfg_score_el ~~ knowscore

# Define all direct, indirect, and total effects by group
directeffectg1 := cp1
directeffectg2 := cp2

indirecteffect1g1 := (a11*b11) ## poultry number (but not marketing) - group 1
indirecteffect1g2 := (a12*b12) ## poultry number (but not marketing) - group 2

indirecteffect2g1 := (a11*d11*b71) ## poultry number and marketing - group 1
indirecteffect2g2 := (a12*d12*b72) ## poultry number and marketing - group 2

indirecteffect3g1 := (a21*b21) ## poultry egg (but not marketing) - group 1
indirecteffect3g2 := (a22*b22) ## poultry egg (but not marketing) - group 2

indirecteffect4g1 := (a21*d21*b71) ## poultry egg and marketing - group 1
indirecteffect4g2 := (a22*d22*b72) ## poultry egg and marketing - group 2

indirecteffect5g1 := (a31*b31) ## garden practices (but not marketing) - group 1
indirecteffect5g2 := (a32*b32) ## garden practices (but not marketing) - group 2

indirecteffect6g1 := (a31*d41*b71) ## garden practices and marketing - group 1
indirecteffect6g2 := (a32*d42*b72) ## garden practices and marketing - group 2

indirecteffect7g1 := (a41*b41) ## crop species richness (but not marketing) - group 1
indirecteffect7g2 := (a42*b42) ## crop species richness (but not marketing) - group 2

indirecteffect8g1 := (a41*d31*b71) ## crop species richness and marketing - group 1
indirecteffect8g2 := (a42*d32*b72) ## crop species richness and marketing - group 2

indirecteffect9g1 := (a51*b51) ## food group knowledge (but not marketing) - group 1
indirecteffect9g2 := (a52*b52) ## food group knowledge (but not marketing) - group 2

indirecteffect10g1 := (a51*d51*b71) ## food group knowledge and marketing - group 1
indirecteffect10g2 := (a52*d52*b72) ## food group knowledge and marketing - group 2


indirecteffect11g1 := (a61*b61) ## diet diversity knowledge (but not marketing) - group 1
indirecteffect11g2 := (a62*b62) ## diet diversity knowledge (but not marketing) - group 2

indirecteffect12g1 := (a61*d61*b71) ## diet diversity knowledge and marketing - group 1
indirecteffect12g2 := (a62*d62*b72) ## diet diversity knowledge and marketing - group 2


indirecteffect13g1 := (a71*b71) ## market activity - group 1
indirecteffect13g2 := (a72*b72) ## market activity - group 2


# -- Aggregate and categorize direct and indirect effects by pathways and domains, by group (g1 = group 1, g2 = group 2)

totaleffectg1 := directeffectg1 + indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1
totaleffectg2 := directeffectg2 + indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Group 1
indirect_poultryg1 := indirecteffect1g1 + indirecteffect3g1
indirect_poultry_mrktg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1
indirect_poultrynumg1 := indirecteffect1g1 
indirect_poultryeggg1 := indirecteffect3g1 
indirect_poultrynummrktg1 := indirecteffect2g1
indirect_poultryeggmrktg1 := indirecteffect4g1

indirect_gardeng1 := indirecteffect5g1 + indirecteffect7g1
indirect_garden_mrktg1 := indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1
indirect_gardenpracg1 := indirecteffect5g1 
indirect_gardensrg1 := indirecteffect7g1
indirect_gardenpracmrktg1 := indirecteffect6g1
indirect_gardensrmrktg1 := indirecteffect8g1

indirect_knowledgeg1 := indirecteffect9g1 + indirecteffect11g1
indirect_knowledge_mrkttotg1 := indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1
indirect_fgknowg1 := indirecteffect9g1
indirect_ddknowg1 := indirecteffect11g1
indirect_fgknowmrktg1 := indirecteffect10g1
indirect_ddknowmrktg1 := indirecteffect12g1

indirect_marketg1 := indirecteffect13g1 + indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1
indirect_market_aloneg1 := indirecteffect13g1
indirect_market_dpathsg1 := indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1

total_indirectg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1

# Calculate proportion mediated by pathways and domains
pct_direct_totalg1 := directeffectg1/totaleffectg1*100
pct_poultry_totalg1 := indirect_poultryg1/totaleffectg1*100
pct_poultrymrkt_totalg1 := indirect_poultry_mrktg1/totaleffectg1*100
pct_garden_totalg1 := indirect_gardeng1/totaleffectg1*100
pct_gardenmrkt_totalg1 := indirect_garden_mrktg1/totaleffectg1*100
pct_knowledge_totalg1 := indirect_knowledgeg1/totaleffectg1*100
pct_knowledgemrkt_totalg1 := indirect_knowledge_mrkttotg1/totaleffectg1*100
pct_market_totalg1 := indirect_marketg1/totaleffectg1*100
pct_marketaloneg1 := indirect_market_aloneg1/totaleffectg1*100
pct_indirect_totalg1 := total_indirectg1/totaleffectg1*100


# Group 2

indirect_poultryg2 := indirecteffect1g2 + indirecteffect3g2
indirect_poultry_mrktg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2
indirect_poultrynumg2 := indirecteffect1g2 
indirect_poultryeggg2 := indirecteffect3g2 
indirect_poultrynummrktg2 := indirecteffect2g2
indirect_poultryeggmrktg2 := indirecteffect4g2

indirect_gardeng2 := indirecteffect5g2 + indirecteffect7g2
indirect_garden_mrktg2 := indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2
indirect_gardenpracg2 := indirecteffect5g2 
indirect_gardensrg2 := indirecteffect7g2
indirect_gardenpracmrktg2 := indirecteffect6g2
indirect_gardensrmrktg2 := indirecteffect8g2

indirect_knowledgeg2 := indirecteffect9g2 + indirecteffect11g2
indirect_knowledge_mrkttotg2 := indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2
indirect_fgknowg2 := indirecteffect9g2
indirect_ddknowg2 := indirecteffect11g2
indirect_fgknowmrktg2 := indirecteffect10g2
indirect_ddknowmrktg2 := indirecteffect12g2

indirect_marketg2 := indirecteffect13g2 + indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2
indirect_market_aloneg2 := indirecteffect13g2
indirect_market_dpathsg2 := indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2

total_indirectg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Calculate proportion mediated by pathways and domains
pct_direct_totalg2 := directeffectg2/totaleffectg2*100
pct_poultry_totalg2 := indirect_poultryg2/totaleffectg2*100
pct_poultrymrkt_totalg2 := indirect_poultry_mrktg2/totaleffectg2*100
pct_garden_totalg2 := indirect_gardeng2/totaleffectg2*100
pct_gardenmrkt_totalg2 := indirect_garden_mrktg2/totaleffectg2*100
pct_knowledge_totalg2 := indirect_knowledgeg2/totaleffectg2*100
pct_knowledgemrkt_totalg2 := indirect_knowledge_mrkttotg2/totaleffectg2*100
pct_market_totalg2 := indirect_marketg2/totaleffectg2*100
pct_marketaloneg2 := indirect_market_aloneg2/totaleffectg2*100
pct_indirect_totalg2 := total_indirectg2/totaleffectg2*100

# -- Calculate group differences (interaction):
diff_direct := directeffectg2 - directeffectg1
diff_total := totaleffectg2 - totaleffectg1
diff_indirect := total_indirectg2 - total_indirectg1

# -- Component-wise differences in mediation by domain
diff_indirect_poultry := indirect_poultryg2 - indirect_poultryg1
diff_indirect_poultry_mrkt := indirect_poultry_mrktg2 - indirect_poultry_mrktg1
diff_indirect_garden := indirect_gardeng2 - indirect_gardeng1
diff_indirect_garden_mrkt := indirect_garden_mrktg2 - indirect_garden_mrktg1
diff_indirect_knowledge := indirect_knowledgeg2 - indirect_knowledgeg1
diff_indirect_knowledge_mrkt := indirect_knowledge_mrkttotg2 - indirect_knowledge_mrkttotg1
diff_indirect_market := indirect_marketg2 - indirect_marketg1
diff_indirect_market_alone := indirect_market_aloneg2 - indirect_market_aloneg1
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
# Set group label 
m2 <- sem(
  model, 
  data=aggregated, 
  missing="fiml", 
  cluster="ps_c_code", 
  group="bl_quint_category",
  group.label=c("1:Low wealth (Quint 1-2)", "2:Mid to high wealth (Quint 3-5)")
)

# Output summary
summary(m2, standardized=TRUE)
parameterestimates_wlth <- parameterEstimates(m2)

# Calculate goodness of fit indices for overall model fit
fit_indices_wlth <- fitMeasures(m2, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices_wlth)

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234)  # Set random seed for replicable confidence intervals
mc_ci_wlth <- monteCarloCI(m2)

# Save as data.frames
parameter_df_wlth <- as.data.frame(parameterestimates_wlth)
fit_df_wlth <- data.frame(index = names(fit_indices_wlth), value=as.numeric(fit_indices_wlth))
mcci_df_wlth <- data.frame(parameter = row.names(mc_ci_wlth), mc_ci_wlth)



##### Appendix 8: Education Category #####
# Structural equations (direct and indirect pathways) with group specific coefficients
model <- '
dds_avg ~ c(cp1,cp2)*treatment + c(b11,b12)*poultrynum_avg + c(b21,b22)*poultryegg_avg + c(b31,b32)*gardenprac + c(b41,b42)*cropsr + c(b51,b52)*knfg_score_el + c(b61,b62)*knowscore + c(b71,b72)*el_emp_market + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultrynum_avg ~ c(a11,a12)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
poultryegg_avg ~ c(a21,a22)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
gardenprac ~ c(a31,a32)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
cropsr ~ c(a41,a42)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knfg_score_el ~ c(a51,a52)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
knowscore ~ c(a61,a62)*treatment  + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market
el_emp_market ~ c(a71,a72)*treatment + c(d11,d12)*poultrynum_avg + c(d21,d22)*poultryegg_avg + c(d31,d32)*cropsr + c(d41,d42)*gardenprac + c(d51,d52)*knfg_score_el + c(d61,d62)*knowscore + bl_quint2 + bl_quint3 + bl_quint4 + bl_quint5 + religion + bl_dds_m + log_bl_homeland + log_bl_agland + bl_cropsr + bl_lefthome_m + bl_ss_cont_m + bl_huscomm_cont_m + bl_extcomm_cont_m + bl_dec_cont_m + bl_income_m + bl_market

# Residual covariances between mediator error terms
poultrynum_avg ~~ poultryegg_avg 
cropsr ~~ gardenprac 
poultrynum_avg ~~ cropsr
poultrynum_avg ~~ gardenprac
poultryegg_avg ~~ cropsr
poultryegg_avg ~~ gardenprac
knfg_score_el ~~ knowscore

# Define all direct, indirect, and total effects by group
directeffectg1 := cp1
directeffectg2 := cp2

indirecteffect1g1 := (a11*b11) ## poultry number (but not marketing) - group 1
indirecteffect1g2 := (a12*b12) ## poultry number (but not marketing) - group 2

indirecteffect2g1 := (a11*d11*b71) ## poultry number and marketing - group 1
indirecteffect2g2 := (a12*d12*b72) ## poultry number and marketing - group 2

indirecteffect3g1 := (a21*b21) ## poultry egg (but not marketing) - group 1
indirecteffect3g2 := (a22*b22) ## poultry egg (but not marketing) - group 2

indirecteffect4g1 := (a21*d21*b71) ## poultry egg and marketing - group 1
indirecteffect4g2 := (a22*d22*b72) ## poultry egg and marketing - group 2

indirecteffect5g1 := (a31*b31) ## garden practices (but not marketing) - group 1
indirecteffect5g2 := (a32*b32) ## garden practices (but not marketing) - group 2

indirecteffect6g1 := (a31*d41*b71) ## garden practices and marketing - group 1
indirecteffect6g2 := (a32*d42*b72) ## garden practices and marketing - group 2

indirecteffect7g1 := (a41*b41) ## crop species richness (but not marketing) - group 1
indirecteffect7g2 := (a42*b42) ## crop species richness (but not marketing) - group 2

indirecteffect8g1 := (a41*d31*b71) ## crop species richness and marketing - group 1
indirecteffect8g2 := (a42*d32*b72) ## crop species richness and marketing - group 2

indirecteffect9g1 := (a51*b51) ## food group knowledge (but not marketing) - group 1
indirecteffect9g2 := (a52*b52) ## food group knowledge (but not marketing) - group 2

indirecteffect10g1 := (a51*d51*b71) ## food group knowledge and marketing - group 1
indirecteffect10g2 := (a52*d52*b72) ## food group knowledge and marketing - group 2


indirecteffect11g1 := (a61*b61) ## diet diversity knowledge (but not marketing) - group 1
indirecteffect11g2 := (a62*b62) ## diet diversity knowledge (but not marketing) - group 2

indirecteffect12g1 := (a61*d61*b71) ## diet diversity knowledge and marketing - group 1
indirecteffect12g2 := (a62*d62*b72) ## diet diversity knowledge and marketing - group 2


indirecteffect13g1 := (a71*b71) ## market activity - group 1
indirecteffect13g2 := (a72*b72) ## market activity - group 2


# -- Aggregate and categorize direct and indirect effects by pathways and domains, by group (g1 = group 1, g2 = group 2)

totaleffectg1 := directeffectg1 + indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1
totaleffectg2 := directeffectg2 + indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Group 1
indirect_poultryg1 := indirecteffect1g1 + indirecteffect3g1
indirect_poultry_mrktg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1
indirect_poultrynumg1 := indirecteffect1g1 
indirect_poultryeggg1 := indirecteffect3g1 
indirect_poultrynummrktg1 := indirecteffect2g1
indirect_poultryeggmrktg1 := indirecteffect4g1

indirect_gardeng1 := indirecteffect5g1 + indirecteffect7g1
indirect_garden_mrktg1 := indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1
indirect_gardenpracg1 := indirecteffect5g1 
indirect_gardensrg1 := indirecteffect7g1
indirect_gardenpracmrktg1 := indirecteffect6g1
indirect_gardensrmrktg1 := indirecteffect8g1

indirect_knowledgeg1 := indirecteffect9g1 + indirecteffect11g1
indirect_knowledge_mrkttotg1 := indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1
indirect_fgknowg1 := indirecteffect9g1
indirect_ddknowg1 := indirecteffect11g1
indirect_fgknowmrktg1 := indirecteffect10g1
indirect_ddknowmrktg1 := indirecteffect12g1

indirect_marketg1 := indirecteffect13g1 + indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1
indirect_market_aloneg1 := indirecteffect13g1
indirect_market_dpathsg1 := indirecteffect2g1 + indirecteffect4g1 + indirecteffect6g1 + indirecteffect8g1 + indirecteffect10g1 + indirecteffect12g1

total_indirectg1 := indirecteffect1g1 + indirecteffect2g1 + indirecteffect3g1 + indirecteffect4g1 + indirecteffect5g1 + indirecteffect6g1 + indirecteffect7g1 + indirecteffect8g1 + indirecteffect9g1 + indirecteffect10g1 + indirecteffect11g1 + indirecteffect12g1 + indirecteffect13g1

# Calculate proportion mediated by pathways and domains
pct_direct_totalg1 := directeffectg1/totaleffectg1*100
pct_poultry_totalg1 := indirect_poultryg1/totaleffectg1*100
pct_poultrymrkt_totalg1 := indirect_poultry_mrktg1/totaleffectg1*100
pct_garden_totalg1 := indirect_gardeng1/totaleffectg1*100
pct_gardenmrkt_totalg1 := indirect_garden_mrktg1/totaleffectg1*100
pct_knowledge_totalg1 := indirect_knowledgeg1/totaleffectg1*100
pct_knowledgemrkt_totalg1 := indirect_knowledge_mrkttotg1/totaleffectg1*100
pct_market_totalg1 := indirect_marketg1/totaleffectg1*100
pct_marketaloneg1 := indirect_market_aloneg1/totaleffectg1*100
pct_indirect_totalg1 := total_indirectg1/totaleffectg1*100


# Group 2

indirect_poultryg2 := indirecteffect1g2 + indirecteffect3g2
indirect_poultry_mrktg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2
indirect_poultrynumg2 := indirecteffect1g2 
indirect_poultryeggg2 := indirecteffect3g2 
indirect_poultrynummrktg2 := indirecteffect2g2
indirect_poultryeggmrktg2 := indirecteffect4g2

indirect_gardeng2 := indirecteffect5g2 + indirecteffect7g2
indirect_garden_mrktg2 := indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2
indirect_gardenpracg2 := indirecteffect5g2 
indirect_gardensrg2 := indirecteffect7g2
indirect_gardenpracmrktg2 := indirecteffect6g2
indirect_gardensrmrktg2 := indirecteffect8g2

indirect_knowledgeg2 := indirecteffect9g2 + indirecteffect11g2
indirect_knowledge_mrkttotg2 := indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2
indirect_fgknowg2 := indirecteffect9g2
indirect_ddknowg2 := indirecteffect11g2
indirect_fgknowmrktg2 := indirecteffect10g2
indirect_ddknowmrktg2 := indirecteffect12g2

indirect_marketg2 := indirecteffect13g2 + indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2
indirect_market_aloneg2 := indirecteffect13g2
indirect_market_dpathsg2 := indirecteffect2g2 + indirecteffect4g2 + indirecteffect6g2 + indirecteffect8g2 + indirecteffect10g2 + indirecteffect12g2

total_indirectg2 := indirecteffect1g2 + indirecteffect2g2 + indirecteffect3g2 + indirecteffect4g2 + indirecteffect5g2 + indirecteffect6g2 + indirecteffect7g2 + indirecteffect8g2 + indirecteffect9g2 + indirecteffect10g2 + indirecteffect11g2 + indirecteffect12g2 + indirecteffect13g2

# Calculate proportion mediated by pathways and domains
pct_direct_totalg2 := directeffectg2/totaleffectg2*100
pct_poultry_totalg2 := indirect_poultryg2/totaleffectg2*100
pct_poultrymrkt_totalg2 := indirect_poultry_mrktg2/totaleffectg2*100
pct_garden_totalg2 := indirect_gardeng2/totaleffectg2*100
pct_gardenmrkt_totalg2 := indirect_garden_mrktg2/totaleffectg2*100
pct_knowledge_totalg2 := indirect_knowledgeg2/totaleffectg2*100
pct_knowledgemrkt_totalg2 := indirect_knowledge_mrkttotg2/totaleffectg2*100
pct_market_totalg2 := indirect_marketg2/totaleffectg2*100
pct_marketaloneg2 := indirect_market_aloneg2/totaleffectg2*100
pct_indirect_totalg2 := total_indirectg2/totaleffectg2*100

# -- Calculate group differences (interaction):
diff_direct := directeffectg2 - directeffectg1
diff_total := totaleffectg2 - totaleffectg1
diff_indirect := total_indirectg2 - total_indirectg1

# -- Component-wise differences in mediation by domain
diff_indirect_poultry := indirect_poultryg2 - indirect_poultryg1
diff_indirect_poultry_mrkt := indirect_poultry_mrktg2 - indirect_poultry_mrktg1
diff_indirect_garden := indirect_gardeng2 - indirect_gardeng1
diff_indirect_garden_mrkt := indirect_garden_mrktg2 - indirect_garden_mrktg1
diff_indirect_knowledge := indirect_knowledgeg2 - indirect_knowledgeg1
diff_indirect_knowledge_mrkt := indirect_knowledge_mrkttotg2 - indirect_knowledge_mrkttotg1
diff_indirect_market := indirect_marketg2 - indirect_marketg1
diff_indirect_market_alone := indirect_market_aloneg2 - indirect_market_aloneg1
'

# --- Model Fitting ---
# Run SEM model using lavaan, with missingness handled by FIML
# Adjust standard errors for clustering at settlement level (ps_c_code)
# Set group label
m3 <- sem(
  model, 
  data=aggregated, 
  missing="fiml", 
  cluster="ps_c_code", 
  group="edu_cat_bin",
  group.label=c("1:No or primary education (0-2)", "2:Secondary education (3-4)")
)

# Output summary
summary(m3, standardized=TRUE)
parameterestimates_educ <- parameterEstimates(m3)

# Calculate goodness of fit indices for overall model fit
fit_indices_educ <- fitMeasures(m3, c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea"))
print(fit_indices_educ)

# --- Confidence Intervals via Monte Carlo Simulation ---
library(semTools)
set.seed(1234)  # Set random seed for replicable confidence intervals
mc_ci_educ <- monteCarloCI(m3)

# Save as data.frames
parameter_df_educ <- as.data.frame(parameterestimates_educ)
fit_df_educ <- data.frame(index = names(fit_indices_educ), value=as.numeric(fit_indices_educ))
mcci_df_educ <- data.frame(parameter = row.names(mc_ci_educ), mc_ci_educ)


