###################################################################
# DESCRIBE THE VARIABLES
###################################################################

var_selection <- c("sel_data_incomplete", "sel_no_obs_periods",
                   "sel_obs_period_not_overlapped_study_period", 
                   "sel_never18plus_during_study_period", "sel_no_drug",
                   "sel_no_drug_during_obs_period_correct_age",
                   "sel_no_lookback", "sel_no_ASL", "is_in_study",
                   "is_prevalent")

covariates_binary_tab1 <- c("met", "antidiabother", "CV", "cerebro", "aop", 
                            "Cvrisk", "HF", "renal")

covariates_binary_tab2 <- c("diab_gestaz", "diab_pregrav", "bmi_low", 
                            "bmi_medium", "bmi_high")

# names of variables

codelists_variable_condition <- c("CV","cerebro","aop", "Cvrisk", "HF", "renal")
variables_condition <- paste0("VAR_",codelists_variable_condition)

codelists_variable_medication <- c("met", "antidiabother")
variables_medication <- paste0("VAR_",codelists_variable_medication)

variables <- c(variables_condition, variables_medication)

# labels of variables

name_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  name_variable[[paste0("VAR_",concept)]] <- name_codelist[[concept]]
}


# assign the codelists and time spans to those covariates that are computed via codelists

codelists_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  codelists_variable[[paste0("VAR_",concept)]] <- concept
}
timespan <- list()
for (concept in c(codelists_variable_medication)) {
  timespan[[paste0("VAR_",concept)]] <- 730
}