# CRFV TOSCANA
# studio antidiabetici

# authors: Rosa Gini, Sabrina Giometto

# v 0.1 15 Mag 2026

# skeleton 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TEST <- T

#type_data_test <- "simulation" 
# type_data_test <- "dummy"

# to be removed
drug_names <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                "other_combinations")

drug_names_s <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i")

var_selection <- c("sel_data_incomplete", "sel_no_obs_periods", "sel_no_drug", 
                   "sel_no_lookback", "sel_prevalent")

covariates_binary_tab1 <- c("met", "antidiabother", "CV", "cerebro", "aop", 
                            "Cvrisk", "HF", "renal")

covariates_binary_tab2 <- c("diab_gestaz", "diab_pregrav", "bmi_low", 
                            "bmi_medium", "bmi_high")



####################
# load parameters
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
# source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
# source(paste0(thisdir,"/p_parameters/5_variable_lists.R"))
# source(paste0(thisdir,"/p_parameters/6_parameters_study.R"))
# source(paste0(thisdir,"/p_parameters/7_parameters_postprocessing.R"))


#

# ######################################
# # run scripts
# source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
# source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))
# source(paste0(thisdir,"/p_steps/01_T2_30_create_persons.R"))
# source(paste0(thisdir,"/p_steps/05_T4_10_cohort_characteristics.R"))
# source(paste0(thisdir,"/p_steps/06_T5_20_Create_D6_Table_2.R"))

